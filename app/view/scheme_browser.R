# app/view/scheme_browser.R

box::use(
  shiny[NS, reactiveValues, moduleServer],
  bslib[page_fillable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    "Scheme Browser"
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    Scheme_Browser <- reactiveValues()

    # Return values
    # reactiveValues(
    #   load_startup = shiny::reactive(input$load_startup)
    # )
  })
}
