# app/view/typing.R

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
      title = "Typing"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
