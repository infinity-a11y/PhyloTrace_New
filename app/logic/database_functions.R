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
    dbExecute
  ],
  RSQLite[SQLite],
)


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
