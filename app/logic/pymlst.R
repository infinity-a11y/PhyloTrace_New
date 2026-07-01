# app/logic/pymslt.R

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
    dbListTables,
    dbReadTable,
    dbGetQuery,
    dbWriteTable,
    dbDisconnect,
  ],
  processx[run, process],
  openssl[sha256],
  tidyr[pivot_wider],
  dplyr[select, left_join],
)

### Download cgmlst scheme
# db_path - target database path including '.db' file ending
# scheme - scheme corresponding to cgmlst.org schemes
# overwrite - overwrite existing database
#' @export
download_cgmlst_scheme <- function(
  scheme,
  db_path,
  env_name,
  overwrite = FALSE
) {
  download_status <- tryCatch(
    run(
      command = "conda",
      args = c(
        "run",
        "-n",
        env_name,
        "wgMLST",
        "import",
        if (overwrite) {
          "--force"
        },
        "--no-prompt",
        basename(db_path),
        scheme
      ),
      wd = dirname(db_path),
      echo_cmd = TRUE,
      echo = TRUE,
      stderr_to_stdout = TRUE,
      error_on_status = FALSE
    ),
    error = function(e) e
  )

  return(download_status)
}

### Read database
#' @export
read_database <- function(db_path) {
  message(paste(
    "Reading",
    basename(db_path),
    "from",
    dirname(db_path),
    "..."
  ))

  # Connect to database
  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)

  # List tables
  tables <- dbListTables(con)

  # Iterate over tables to summarize in list
  database <- list()
  for (table in tables) {
    database[[table]] <- dbReadTable(con, table)
  }

  # Digest new sequences with SHA-256
  database <- add_sequence_hashes(database)

  # Disconnect from the database
  dbDisconnect(con)

  return(database)
}

### Synchronize local changes with remote database
#' @export
synchronize_database <- function(database, db_path) {
  # Connect to remote database
  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)

  # Write local changes
  for (table in names(database)) {
    dbWriteTable(con, table, database[[table]], overwrite = TRUE)
  }

  # Disconnect from the database
  dbDisconnect(con)
}

### Assemble the loop-pymlst.sh command-line flags
# Shared by the blocking `type_genomes()` and the non-blocking `start_typing()`
# so the database / genome / parameter contract lives in a single place.
# `genome_files` is the explicit vector of assembly files to type; each is
# passed as a positional argument (after a `--` guard so file names are never
# mistaken for options) and typed in turn. Resolving the file list in R - rather
# than letting the script glob a directory - lets the caller drop assemblies
# that are already in the database before they ever reach `wgMLST add`.
typing_args <- function(db_path, genome_files, identity, coverage, env) {
  c(
    "-d",
    basename(db_path),
    "-i",
    as.character(identity),
    "-c",
    as.character(coverage),
    "-e",
    env,
    "--",
    genome_files
  )
}

### Typing isolates
#' @export
type_genomes <- function(
  database,
  db_path,
  genome_files,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = "pymlst"
) {
  # Run the process. `bash <script>` avoids depending on the script's execute
  # bit.
  typing_status <- run(
    command = "bash",
    args = c(
      normalizePath(script_path, mustWork = TRUE),
      typing_args(db_path, genome_files, identity, coverage, env)
    ),
    wd = dirname(db_path),
    echo_cmd = TRUE,
    echo = TRUE,
    stderr_to_stdout = TRUE,
    error_on_status = FALSE
  )

  # Parse logs
  stdout_text <- typing_status$stdout

  # Extract BLAT gene count
  gene_match <- regmatches(
    stdout_text,
    regexec("found ([0-9]+) genes", stdout_text)
  )
  typing_status$genes_found_by_blat <- if (length(gene_match[[1]]) > 1) {
    as.numeric(gene_match[[1]][2])
  } else {
    0
  }

  # Check for "Already Present" error
  typing_status$already_present <- grepl(
    "already present in the base",
    stdout_text
  )

  # Check for "Core Genome Path" error (The species mismatch/quality error)
  typing_status$species_mismatch <- grepl(
    "No path was found for the core genome",
    stdout_text
  )

  # Define success: Exit status 0 AND no "Error:" string in the output
  typing_status$success <- (typing_status$status == 0 &&
    !grepl("Error:", stdout_text))

  # Console Feedback
  if (typing_status$already_present) {
    message("DUPLICATE: Entry already present. No action taken.")
  } else if (typing_status$species_mismatch) {
    warning(
      "INCOMPATIBLE: ",
      typing_status$genes_found_by_blat,
      " hits found, but none passed QC. Verify scheme."
    )
  } else if (typing_status$success) {
    message("OK: New strain added successfully.")
  }

  # Read database with newly added genomes
  database <- read_database(db_path)

  return(database)
}

### Start typing in the background (non-blocking)
# Launches loop-pymlst.sh as a detached processx process that streams its
# combined stdout/stderr into `log_file`. Returns the live `process` object so
# the caller (the Typing module) can poll `is_alive()`, tail the log for live
# progress, and `kill()` it on demand. Unlike `type_genomes()` this does not
# block the R session and does not touch the database itself - read the result
# with `read_database()` once the process has finished.
#' @export
start_typing <- function(
  db_path,
  genome_files,
  log_file,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = "pymlst"
) {
  process$new(
    command = "bash",
    args = c(
      normalizePath(script_path, mustWork = TRUE),
      typing_args(db_path, genome_files, identity, coverage, env)
    ),
    wd = dirname(db_path),
    stdout = log_file,
    stderr = "2>&1",
    # The bash wrapper spawns `conda run` -> python (wgMLST), and it is that
    # descendant that actually holds the SQLite lock. cleanup_tree tags the whole
    # subtree so it can be killed as a unit (via kill_tree() or on GC) - killing
    # just the bash wrapper would orphan pymlst and leave the database locked.
    cleanup_tree = TRUE
  )
}

### Parse a (possibly partial) typing log into a per-strain status table
# `log_lines` is the captured loop-pymlst.sh output (character vector or single
# string); `strains` is the ordered vector of expected strain names (assembly
# file names without extension). Returns one row per expected strain so it can
# drive a live progress / results table. Columns:
#   status  - Pending | Running | Added | Duplicate | Incompatible | Error
#   found   - genes located by BLAT
#   added   - new MLST genes stored (len(genes) - bad)
#   partial  - partial genes detected
#   filled   - partial genes recovered
#   removed  - genes dropped (bad coverage / failed CDS test)
#   finished - wall-clock time of the "DONE" log line ("HH:MM:SS")
#   elapsed  - analysis duration in seconds (DONE minus first timestamp)
#   detail   - short human-readable explanation
#
# The outcomes mirror every branch `wgMLST add` (pymlst/wg/core.py::add_strain
# and pymlst/common/blat.py::run_blat) can take:
#   Added        - run reached "DONE"
#   Duplicate    - StrainAlreadyPresent ("already present in the base")
#   Incompatible - CoreGenomePathNotFound ("No path was found for the core
#                  genome"): BLAT matched no core gene, i.e. wrong species /
#                  unusable assembly
#   Error        - BinaryNotFound, BLAT failure, bad identity/coverage range,
#                  ChromosomeNotFound, invalid strain name, or any other
#                  ClickException printed as "Error: ..."
#' @export
parse_typing_log <- function(log_lines, strains) {
  log_text <- paste(log_lines, collapse = "\n")
  finished_all <- grepl("Done!", log_text, fixed = TRUE)

  # Each strain section is introduced by the script's "Processing Strain:" line.
  parts <- strsplit(log_text, "Processing Strain: ", fixed = TRUE)[[1]]
  sections <- list()
  order_seen <- character(0)
  for (part in parts[-1]) {
    name <- trimws(sub("\n.*$", "", part))
    sections[[name]] <- part
    order_seen <- c(order_seen, name)
  }

  num <- function(chunk, pattern) {
    match <- regmatches(chunk, regexec(pattern, chunk))[[1]]
    if (length(match) > 1) as.integer(match[2]) else NA_integer_
  }

  # Per-strain wall-clock timing taken from the "[INFO: <ts>]" log prefixes
  # (e.g. "[INFO: 2026-06-25 21:16:40,224] DONE"). `finished` is the time on the
  # DONE line; `elapsed` is DONE minus the strain's first timestamp (the BLAT
  # search). Both are NA when the strain never reached DONE.
  ts_pattern <-
    "\\[INFO: ([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}[.,][0-9]+)\\]"
  timing <- function(chunk) {
    to_posix <- function(x) {
      as.POSIXct(gsub(",", ".", x), format = "%Y-%m-%d %H:%M:%OS")
    }
    matches <- regmatches(chunk, gregexpr(ts_pattern, chunk))[[1]]
    stamps <- if (length(matches)) {
      to_posix(sub(ts_pattern, "\\1", matches))
    } else {
      as.POSIXct(character(0))
    }
    done <- regmatches(
      chunk,
      regexec(paste0(ts_pattern, "[ \t]*DONE"), chunk)
    )[[1]]
    done_ts <- if (length(done) > 1) to_posix(done[2]) else as.POSIXct(NA)
    start_ts <- if (length(stamps)) min(stamps, na.rm = TRUE) else as.POSIXct(NA)
    list(
      finished = if (!is.na(done_ts)) format(done_ts, "%H:%M:%S") else NA_character_,
      elapsed = if (!is.na(done_ts) && !is.na(start_ts)) {
        as.numeric(difftime(done_ts, start_ts, units = "secs"))
      } else {
        NA_real_
      }
    )
  }

  classify <- function(chunk, complete) {
    times <- timing(chunk)
    metrics <- list(
      found = num(chunk, "found ([0-9]+) genes"),
      added = num(chunk, "Added ([0-9]+) new MLST genes"),
      partial = num(chunk, "Found ([0-9]+) partial genes"),
      filled = num(chunk, "partial genes, filled ([0-9]+)"),
      removed = num(chunk, "Removed ([0-9]+) genes"),
      finished = times$finished,
      elapsed = times$elapsed
    )
    outcome <- function(status, detail) c(metrics, list(status = status, detail = detail))

    if (!complete) {
      return(outcome("Running", "Typing in progress ..."))
    }
    if (grepl("already present in the base", chunk)) {
      return(outcome("Duplicate", "Strain already present in the database."))
    }
    if (grepl("No path was found for the core genome", chunk)) {
      return(outcome(
        "Incompatible",
        "No core genes matched - assembly likely does not match the scheme species."
      ))
    }
    if (grepl("BLAT binary was not found", chunk)) {
      return(outcome("Error", "BLAT binary not found in the pymlst environment."))
    }
    if (grepl("An error occurred while running BLAT", chunk)) {
      return(outcome("Error", "BLAT failed to run on this assembly."))
    }
    if (grepl("must be in range", chunk)) {
      return(outcome("Error", "Identity / coverage must be within [0-1]."))
    }
    if (grepl("Chromosome .* not found", chunk)) {
      return(outcome("Error", "A matched contig was missing from the assembly."))
    }
    if (grepl("contains", chunk) && grepl("symbol", chunk)) {
      return(outcome("Error", "Invalid strain name (unsupported character)."))
    }
    if (grepl("DONE", chunk) || !is.na(metrics$added)) {
      # The gene counts are surfaced as their own columns; nothing extra to say.
      return(outcome("Added", ""))
    }
    # Complete but unrecognised: surface any explicit "Error: ..." line.
    err <- regmatches(chunk, regexec("Error:[ \t]*(.+)", chunk))[[1]]
    outcome(
      "Error",
      if (length(err) > 1) trimws(err[2]) else "Unrecognised outcome - see log."
    )
  }

  rows <- lapply(strains, function(strain) {
    if (!strain %in% names(sections)) {
      info <- list(
        status = "Pending", found = NA_integer_, added = NA_integer_,
        partial = NA_integer_, filled = NA_integer_, removed = NA_integer_,
        finished = NA_character_, elapsed = NA_real_,
        detail = "Waiting in queue ..."
      )
    } else {
      # A section is complete once another section follows it or the run is over.
      idx <- match(strain, order_seen)
      complete <- idx < length(order_seen) || finished_all
      info <- classify(sections[[strain]], complete)
    }
    data.frame(
      strain = strain,
      status = info$status,
      found = info$found,
      added = info$added,
      partial = info$partial,
      filled = info$filled,
      removed = info$removed,
      finished = info$finished,
      elapsed = info$elapsed,
      detail = info$detail,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

### Distinct strain names already stored in a database
# Used to flag selected assemblies that are already present (their wgMLST `add`
# would be rejected as a duplicate). The synthetic "ref" core-genome entry is
# excluded.
#' @export
existing_strains <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(character(0))
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(character(0))
  }

  souches <- dbGetQuery(con, "SELECT DISTINCT souche FROM mlst")$souche
  setdiff(souches, "ref")
}

### Total number of loci in the scheme
# The reference core genome is stored in `mlst` under the synthetic strain
# "ref", one row per locus, so the count of "ref" rows is the scheme size. Used
# as the denominator of the completeness (QC) metric and shown to the user.
#' @export
scheme_size <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(NA_integer_)
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(NA_integer_)
  }

  as.integer(
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM mlst WHERE souche = 'ref'")$n
  )
}

### Number of loci called for each strain
# One `mlst` row is stored per locus successfully called for a strain, so the
# row count per `souche` is the number of genes present. Divided by
# `scheme_size()` this gives the completeness (QC) metric. Returns an integer
# vector named by (and aligned to) `strains`; strains absent from the database
# are NA.
#' @export
strain_gene_counts <- function(db_path, strains) {
  counts <- rep(NA_integer_, length(strains))
  names(counts) <- strains

  if (
    !length(strains) ||
      is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(counts)
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(counts)
  }

  placeholders <- paste(rep("?", length(strains)), collapse = ",")
  result <- dbGetQuery(
    con,
    paste0(
      "SELECT souche, COUNT(*) AS n FROM mlst WHERE souche IN (",
      placeholders,
      ") GROUP BY souche"
    ),
    params = as.list(strains)
  )

  counts[result$souche] <- as.integer(result$n)
  counts
}

add_sequence_hashes <- function(database) {
  database$sequences$sha256 <- vapply(
    database$sequences$sequence,
    function(x) {
      if (is.na(x)) {
        NA_character_
      } else {
        as.character(sha256(chartr("", "", x)))
      }
    },
    character(1)
  )

  return(database)
}

#' @export
get_isolate_allele_profiles <- function(database) {
  # Pivoting by genes and joining with sequences
  mlst_wide <- database$mlst |>
    select(souche, gene, seqid) |>
    left_join(
      database$sequences[, c("id", "sha256")],
      by = c("seqid" = "id")
    ) |>
    select(souche, gene, sha256) |>
    pivot_wider(names_from = gene, values_from = sha256)

  return(mlst_wide)
}

#' @export
remove_genomes <- function(database) {
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/check.html#remove-strains-or-genes
}

#' @export
mlst_profile <- function(database) {
  # Get MLST profile
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/export_res.html#mlst
}

#' @export
stage_genomes <- function(database) {
  # Validate staged genomes
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/check.html#validate-strains
}

#' @export
push_genomes <- function(database) {
  # Merge new genomes with existing database
}
