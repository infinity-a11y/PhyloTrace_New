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
  con <- dbConnect(SQLite(), db_path)

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
  con <- dbConnect(SQLite(), db_path)

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
# `genome_input` is either a directory (passed as -g, every assembly inside is
# typed) or a single assembly file (passed as -f).
typing_args <- function(db_path, genome_input, identity, coverage, env) {
  c(
    "-d",
    basename(db_path),
    if (dir.exists(genome_input)) {
      c("-g", genome_input)
    } else if (file.exists(genome_input)) {
      c("-f", genome_input)
    },
    "-i",
    as.character(identity),
    "-c",
    as.character(coverage),
    "-e",
    env
  )
}

### Typing isolates
#' @export
type_genomes <- function(
  database,
  db_path,
  genome_input,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = "pymlst"
) {
  # Run the process. `bash <script>` avoids depending on the script's execute
  # bit and guarantees the brace-glob in the directory branch is expanded.
  typing_status <- run(
    command = "bash",
    args = c(
      normalizePath(script_path, mustWork = TRUE),
      typing_args(db_path, genome_input, identity, coverage, env)
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
  genome_input,
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
      typing_args(db_path, genome_input, identity, coverage, env)
    ),
    wd = dirname(db_path),
    stdout = log_file,
    stderr = "2>&1"
  )
}

### Parse a (possibly partial) typing log into a per-strain status table
# `log_lines` is the captured loop-pymlst.sh output (character vector or single
# string); `strains` is the ordered vector of expected strain names (assembly
# file names without extension). Returns a data frame with one row per expected
# strain so it can drive a live progress table:
#   status      - Pending | Running | Added | Duplicate | Incompatible | Error
#   genes_found - genes reported by BLAT (NA until known)
#   detail      - short human-readable explanation
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

  classify <- function(chunk, complete) {
    gene_match <- regmatches(chunk, regexec("found ([0-9]+) genes", chunk))[[1]]
    genes <- if (length(gene_match) > 1) as.integer(gene_match[2]) else NA_integer_

    if (!complete) {
      return(list(status = "Running", genes = genes, detail = "Typing in progress ..."))
    }
    if (grepl("already present in the base", chunk)) {
      return(list(
        status = "Duplicate",
        genes = genes,
        detail = "Strain already present in the database."
      ))
    }
    if (grepl("No path was found for the core genome", chunk)) {
      return(list(
        status = "Incompatible",
        genes = genes,
        detail = "No core-genome path - verify scheme/species."
      ))
    }
    if (grepl("Error:", chunk)) {
      return(list(status = "Error", genes = genes, detail = "Typing failed - see log."))
    }
    if (grepl("Added .* new MLST genes", chunk) || grepl("DONE", chunk)) {
      return(list(
        status = "Added",
        genes = genes,
        detail = sprintf("%s genes added.", if (is.na(genes)) "?" else genes)
      ))
    }
    list(status = "Error", genes = genes, detail = "Unrecognised outcome - see log.")
  }

  rows <- lapply(strains, function(strain) {
    if (!strain %in% names(sections)) {
      return(data.frame(
        strain = strain,
        status = "Pending",
        genes_found = NA_integer_,
        detail = "Waiting in queue ...",
        stringsAsFactors = FALSE
      ))
    }
    # A section is complete once another section follows it or the run is over.
    idx <- match(strain, order_seen)
    complete <- idx < length(order_seen) || finished_all
    info <- classify(sections[[strain]], complete)
    data.frame(
      strain = strain,
      status = info$status,
      genes_found = info$genes,
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

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(character(0))
  }

  souches <- dbGetQuery(con, "SELECT DISTINCT souche FROM mlst")$souche
  setdiff(souches, "ref")
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
