# app/logic/pymslt.R

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
    dbListTables,
    dbReadTable,
    dbWriteTable,
    dbDisconnect,
  ],
  processx[run],
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

### Typing isolates
#' @export
type_genomes <- function(
  database,
  db_path,
  genome_input,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = "pymlst_env"
) {
  # Set command arguments up
  cmd_args <- c(
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

  # Run the process
  typing_status <- run(
    command = normalizePath(script_path, mustWork = TRUE),
    args = cmd_args,
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
