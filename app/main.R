box::use(
  shiny[
    stopApp,
    moduleServer,
    NS,
    observeEvent,
    renderUI,
    uiOutput,
    tags,
    div,
    actionButton,
    showModal,
    removeModal,
    modalDialog,
    modalButton,
    icon,
    tagList
  ],
  bslib[
    page_sidebar,
    page_fillable,
    navbar_options,
    bs_theme,
    nav_panel,
    page_navbar,
    nav_spacer,
    nav_item,
    nav_select,
  ],
  shinyjs[runjs],
)
box::use(
  app / view / startup,
  app / view / scheme_browser,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    id = ns("tabs"),
    title = div(
      id = "navbar-title",
      tags$img(
        src = "static/images/PhyloTrace_flat_48.png",
      ),
      div("PhyloTrace")
    ),
    window_title = "PhyloTrace",
    navbar_options = navbar_options(underline = TRUE),
    nav_panel(
      title = "Startup",
      value = "startup_panel",
      startup$ui(ns("startup"))
    ),
    nav_panel(
      title = "Scheme Browser",
      value = "scheme_browser_panel",
      scheme_browser$ui(ns("scheme_browser"))
    ),
    nav_spacer(),
    nav_item("Version 1.6.1"),
    nav_item(
      shiny::tags$a(
        id = "close",
        style = "cursor: pointer;",
        onclick = "Shiny.setInputValue('app-quit', Math.random());",
        icon("power-off")
      )
    ),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Kill server on session end
    session$onSessionEnded(function() {
      stopApp()
    })

    # Shutdown
    observeEvent(input$quit, {
      showModal(
        div(
          class = "start-modal",
          modalDialog(
            "Are you sure you want to stop the application?",
            title = "Close PhyloTrace",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton(
                ns("conf_shutdown"),
                "Close",
                width = "100px"
              )
            )
          )
        )
      )
    })

    # Confirmed shutdown
    observeEvent(input$conf_shutdown, {
      removeModal()
      runjs("window.close();")
      later::later(stopApp, delay = 0.5)
    })

    # Module servers and return values
    STARTUP_vals <- startup$server("startup") # startup module
    SCHEME_BROWSER_vals <- scheme_browser$server("scheme_browser") # scheme_browser module

    # Event show scheme browser UI
    observeEvent(STARTUP_vals$create_scheme(), {
      nav_select(id = "tabs", selected = "scheme_browser_panel")
    })
  })
}
