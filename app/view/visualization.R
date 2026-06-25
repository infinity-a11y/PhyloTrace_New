# app/view/visualization.R

box::use(
  shiny[NS, moduleServer],
  bslib[page_sidebar, sidebar],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Visualization"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
