# app/view/database_scheme_info.R
#
# "Scheme Info" interface of the Database menu. UI and backend live here so
# the panel computes its own state independently of the other menu entries.

box::use(
  shiny[NS, moduleServer, div, h2],
  bslib[as_fill_carrier],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      div(class = "db-page_header", h2("Scheme Info")),
      div(class = "db-page_body")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
