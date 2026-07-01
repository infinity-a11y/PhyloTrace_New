# app/logic/database_functions.R

box::use(
  DBI[
    dbConnect,
    dbDisconnect,
    dbListTables,
    dbSendQuery,
    dbFetch,
    dbClearResult,
    dbReadTable,
    dbWriteTable,
    dbGetQuery,
    dbExecute
  ],
  RSQLite[SQLite],
)

# The scheme's `targets` table stores loci as "FTL_0001" while the `mlst` table
# stores the same locus as "FTL-0001"; normalise the separator so the two can
# be matched.
.norm_locus <- function(x) gsub("[-_]", "-", x)


#' @export
load_db_scheme_overview <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE("scheme_overview" %in% tables)) {
    message("Database does not contain 'scheme_overview' table")
    return(NULL)
  }

  return(dbReadTable(con, "scheme_overview"))
}

#' Read the organism/species name stored in the database's `mlst_type` table.
#'
#' This is the authoritative species of the loaded scheme (written at typing
#' time) and is more robust than parsing it out of the scheme overview. Returns
#' a single species string, or NULL when the table or value is absent.
#' @export
load_db_species <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE("mlst_type" %in% tables)) {
    message("Database does not contain 'mlst_type' table")
    return(NULL)
  }

  species <- dbReadTable(con, "mlst_type")$species
  species <- species[!is.na(species) & nzchar(species)]

  if (!length(species)) {
    return(NULL)
  }

  species[[1]]
}

#' @export
make_metadata_table <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  foo <<- tables

  if (isFALSE(all(c("mlst", "mlst_type", "sequences") %in% tables))) {
    message("Database does not contain expected tables")
    return()
  }

  # Get present isolates
  res <- dbSendQuery(con, "SELECT DISTINCT souche FROM mlst")
  all_isolates <- unname(unlist(dbFetch(res)))
  dbClearResult(res)
  isolates <- all_isolates[all_isolates != "ref"]
  if (!length(isolates)) {
    return()
  }

  # Get current organism
  organism <- dbReadTable(con, "mlst_type")$species

  # If metadata table exists, only append rows for isolates not yet listed
  if ("metadata" %in% tables) {
    existing <- dbReadTable(con, "metadata")
    new_isolates <- setdiff(isolates, existing$isolate)

    if (length(new_isolates)) {
      new_rows <- data.frame(
        isolate = new_isolates,
        primary_laboratory_sample_id = NA_character_,
        specimen_source_id = NA_character_,
        sample_collection_date = NA_character_,
        geo_loc_name_country = NA_character_,
        geo_loc_name_state_province = NA_character_,
        sample_collected_by = NA_character_,
        sequence_submitted_by = NA_character_,
        organism = organism,
        purpose_of_sampling = NA_character_,
        purpose_of_sequencing = NA_character_,
        stringsAsFactors = FALSE
      )
      dbWriteTable(con, "metadata", new_rows, append = TRUE)
    }

    return(dbReadTable(con, "metadata"))
  }

  # Build standard metadata table (GenEpiO-aligned fields)
  metadata <- data.frame(
    isolate = isolates,
    primary_laboratory_sample_id = NA_character_,
    specimen_source_id = NA_character_,
    sample_collection_date = NA_character_,
    geo_loc_name_country = NA_character_,
    geo_loc_name_state_province = NA_character_,
    sample_collected_by = NA_character_,
    sequence_submitted_by = NA_character_,
    organism = organism,
    purpose_of_sampling = NA_character_,
    purpose_of_sequencing = NA_character_,
    stringsAsFactors = FALSE
  )

  # Write table to database
  dbWriteTable(con, "metadata", metadata)

  return(metadata)
}

#' @export
remove_isolates <- function(db_path, isolates) {
  if (!length(isolates)) {
    return(invisible(FALSE))
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)
  placeholders <- paste(rep("?", length(isolates)), collapse = ", ")

  if ("mlst" %in% tables) {
    dbExecute(
      con,
      sprintf("DELETE FROM mlst WHERE souche IN (%s)", placeholders),
      params = as.list(isolates)
    )
    # Remove sequences no longer referenced by any strain
    dbExecute(
      con,
      "DELETE FROM sequences WHERE id NOT IN (SELECT DISTINCT seqid FROM mlst)"
    )
  }

  if ("metadata" %in% tables) {
    dbExecute(
      con,
      sprintf("DELETE FROM metadata WHERE isolate IN (%s)", placeholders),
      params = as.list(isolates)
    )
  }

  invisible(TRUE)
}

#' @export
save_metadata_table <- function(db_path, data) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  dbWriteTable(con, "metadata", data, overwrite = TRUE)
  invisible(TRUE)
}

#' Read the scheme's `targets` (loci) table and enrich it with the number of
#' distinct alleles stored per locus.
#'
#' Returns a data frame with the display columns `Locus`, `Gene`, `Start`,
#' `Length`, `Product`, `Allele Count`, plus an internal `.gene` column that
#' carries the matching `mlst` gene name (the loci-detail queries key on the
#' `mlst` spelling, not the `targets` one). Returns NULL when the database is
#' missing the `targets` or `mlst` table.
#' @export
load_loci_info <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)
  if (isFALSE(all(c("targets", "mlst") %in% tables))) {
    message("Database does not contain 'targets' and 'mlst' tables")
    return(NULL)
  }

  targets <- dbReadTable(con, "targets")

  # Distinct alleles per locus (the synthetic "ref" allele is counted too, as
  # it is a valid scheme allele).
  counts <- dbGetQuery(
    con,
    "SELECT gene, COUNT(DISTINCT seqid) AS n FROM mlst GROUP BY gene"
  )

  idx <- match(.norm_locus(targets$Locus), .norm_locus(counts$gene))

  targets$.gene <- counts$gene[idx]
  allele_count <- counts$n[idx]
  allele_count[is.na(allele_count)] <- 0L
  targets[["Allele Count"]] <- allele_count

  # Replace the raw ".fasta" filename column with the integer count.
  targets$Alleles <- NULL

  targets
}

#' Allele usage for one locus.
#'
#' `gene` is the `mlst` gene name (the `.gene` column of `load_loci_info`).
#' Returns a data frame of every distinct allele stored for the locus with
#' columns `seqid` (integer allele index), `count` (isolates carrying it,
#' excluding the synthetic "ref") and `present` (`count > 0`). Rows are ordered
#' present-first, then by descending count.
#' @export
load_locus_alleles <- function(db_path, gene) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  all_ids <- dbGetQuery(
    con,
    "SELECT DISTINCT seqid FROM mlst WHERE gene = ?",
    params = list(gene)
  )$seqid

  usage <- dbGetQuery(
    con,
    "SELECT seqid, COUNT(*) AS count FROM mlst
       WHERE gene = ? AND souche != 'ref' GROUP BY seqid",
    params = list(gene)
  )

  df <- data.frame(seqid = all_ids, stringsAsFactors = FALSE)
  df$count <- usage$count[match(df$seqid, usage$seqid)]
  df$count[is.na(df$count)] <- 0L
  df$present <- df$count > 0L

  df[order(!df$present, -df$count), , drop = FALSE]
}

#' Nucleotide sequence (character scalar) for a single allele index, or NULL
#' when the index is not stored.
#' @export
load_allele_sequence <- function(db_path, seqid) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  res <- dbGetQuery(
    con,
    "SELECT sequence FROM sequences WHERE id = ?",
    params = list(seqid)
  )$sequence

  if (!length(res)) NULL else res[[1]]
}

#' All alleles of a locus as FASTA text: one `>index` / sequence record per
#' distinct allele stored for `gene`. Returns a character vector (one element
#' per record), or an empty vector when the locus has no alleles.
#' @export
locus_fasta <- function(db_path, gene) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  res <- dbGetQuery(
    con,
    "SELECT DISTINCT s.id AS seqid, s.sequence AS sequence
       FROM mlst m JOIN sequences s ON s.id = m.seqid
      WHERE m.gene = ? ORDER BY s.id",
    params = list(gene)
  )

  if (!nrow(res)) {
    return(character(0))
  }

  paste0(">", res$seqid, "\n", res$sequence)
}
