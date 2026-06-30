# app/view/database_scheme_info.R
#
# "Scheme Info" interface of the Database menu. UI and backend live here so
# the panel computes its own state independently of the other menu entries.

box::use(
  shiny[NS, moduleServer, observeEvent, div, h2, req],
  bslib[as_fill_carrier],
  DT[datatable, renderDT, DTOutput]
)

box::use(
  app / logic / database_functions[load_db_scheme_overview],
  app / logic / functions[render_info]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    DTOutput(ns("local_scheme_table"))
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
    # No local reactive state to clear yet; placeholder for future scheme-info UI.
    observeEvent(session_reset(), {}, ignoreInit = TRUE)

    # Render scheme info table
    output$local_scheme_table <- renderDT({
      req(db_path())
      scheme_overview <- load_db_scheme_overview(db_path())

      test1 <<- scheme_overview

      if (is.null(scheme_overview) || isFALSE(is.data.frame(scheme_overview))) {
        scheme_overview <- data.frame(
          " " = "No entries in this database yet.<br>Add isolates by typing them in the <strong>Allelic Typing</strong> module.",
          check.names = FALSE
        )
      }

      render_info("output$local_scheme_table")

      datatable(
        scheme_overview,
        class = 'stripe row-border order-column',
        colnames = rep("", ncol(scheme_overview)),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      )
    })
  })
}
