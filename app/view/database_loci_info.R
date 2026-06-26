# app/view/database_loci_info.R
#
# "Loci Info" interface of the Database menu. UI and backend live here so
# the panel computes its own state independently of the other menu entries.

box::use(
  shiny[NS, moduleServer, observeEvent, div, h2],
  bslib[as_fill_carrier],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      div(class = "db-page_header", h2("Loci Info")),
      div(class = "db-page_body")
    )
  )
}

#' @export
server <- function(id, session_reset = shiny::reactive(0L)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reset module state when the user returns to the startup screen.
    # No local reactive state to clear yet; placeholder for future loci UI.
    observeEvent(session_reset(), {}, ignoreInit = TRUE)
  })
}
