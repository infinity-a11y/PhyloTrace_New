# app/view/database_browse_entries.R
#
# "Browse Entries" interface of the Database menu. UI and backend live here so
# the panel computes its own state independently of the other menu entries.

box::use(
  shiny[NS, moduleServer, observeEvent, reactive, div, h2],
  bslib[as_fill_carrier],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      div(class = "db-page_header", h2("Browse Entries")),
      div(class = "db-page_body")
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reset module state when the user returns to the startup screen.
    # No local reactive state to clear yet; placeholder for future browse UI.
    observeEvent(session_reset(), {}, ignoreInit = TRUE)

    shiny::observe({
      foo <<- db_path()
    })

    # con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    # db <- RSQLite::dbConnect(foo)

    #dbListTables(con)

    #SELECT DISTINCT column_name
    #FROM table_name;

    #     res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
    # dbFetch(res)
  })
}
