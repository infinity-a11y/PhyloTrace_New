# dev/backfill_wikipedia.R
#
# Re-fetches only the Wikipedia summaries that failed during the initial
# fetch_species_metadata.R run (e.g. due to transient rate limiting) and
# updates app/logic/data/species_metadata.json in place.
#
# Run from the project root:  Rscript dev/backfill_wikipedia.R

library(curl)
library(jsonlite)
library(xml2)

# Reuse the shared helpers (clean_name, wikipedia_summary, http_get with retry).
source("dev/fetch_species_metadata.R", local = TRUE, echo = FALSE)

records <- fromJSON(out_path, simplifyVector = FALSE)

n_fixed <- 0L
for (i in seq_along(records)) {
  r <- records[[i]]
  has_summary <- !is.null(r$summary) && !is.na(r$summary)
  if (has_summary) next

  query <- r$query_name
  message(sprintf("backfill %s ('%s')", r$species, query))
  wiki <- wikipedia_summary(query)
  Sys.sleep(0.5)
  if (is.null(wiki) || is.null(wiki$summary)) {
    message("  still no result")
    next
  }
  records[[i]]$summary <- wiki$summary
  records[[i]]$thumbnail <- wiki$thumbnail
  records[[i]]$wikipedia_url <- wiki$wikipedia_url
  n_fixed <- n_fixed + 1L
}

writeLines(
  toJSON(records, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  out_path
)
message("\nBackfilled ", n_fixed, " record(s) in ", out_path)
