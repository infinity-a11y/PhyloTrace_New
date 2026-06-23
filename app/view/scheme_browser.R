# app/view/scheme_browser.R

box::use(
  shiny[NS, reactiveValues, moduleServer, renderUI, uiOutput, req, div],
  bslib[page_sidebar, sidebar, bs_theme, card, card_header, card_body],
  shinyWidgets[pickerInput],
  DT[DTOutput, renderDT, datatable],
)

box::use(
  app / logic / schemes[cgmlst_org_schemes],
  app / logic / scheme_browser[get_scheme_overview]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    div(
      uiOutput(ns("scheme_title")),
      div(
        class = "scheme-browser-main",
        uiOutput(ns("scheme_overview"))
      )
    ),
    sidebar = sidebar(
      uiOutput(ns("scheme_selection")),
      width = "30%",
      position = "left",
      open = NULL,
      id = NULL,
      title = "Scheme Selection",
      bg = NULL,
      fg = NULL,
      class = NULL,
      max_height_mobile = NULL,
      gap = NULL,
      padding = NULL,
      fillable = FALSE,
      resizable = TRUE
    ),
    title = NULL,
    fillable = TRUE,
    fillable_mobile = FALSE,
    theme = bs_theme(),
    window_title = NA,
    lang = NULL
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    Scheme_Browser <- reactiveValues()

    # Render scheme selector
    output$scheme_selection <- renderUI(
      pickerInput(
        ns("scheme_selector"),
        NULL,
        choices = gsub("_", " ", cgmlst_org_schemes$species),
        options = list(
          `live-search` = TRUE
        )
      )
    )

    # Render scheme title
    output$scheme_title <- renderUI({
      req(input$scheme_selector)

      input$scheme_selector
    })

    # Render scheme overview
    output$scheme_overview <- renderUI({
      req(input$scheme_selector)

      scheme_overview <- get_scheme_overview(input$scheme_selector)

      if (is.character(scheme_overview)) {
        content <- div(scheme_overview)
      } else if (is.data.frame(scheme_overview)) {
        Scheme_Browser$scheme_overview <- scheme_overview
        content <- DTOutput(ns("scheme_table"))
      }

      card(
        card_header(
          class = "bg-dark",
          "Scheme Metadata"
        ),
        card_body(content)
      )
    })

    # Render scheme metadata
    output$scheme_table <- renderDT({
      req(Scheme_Browser$scheme_overview)

      datatable(
        Scheme_Browser$scheme_overview,
        class = 'stripe row-border order-column',
        colnames = c("", ""),
        rownames = FALSE,
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      )
    })
  })
}
