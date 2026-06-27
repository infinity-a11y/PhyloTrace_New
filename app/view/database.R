# app/view/database.R

box::use(
  shiny[NS, moduleServer, observeEvent, reactive],
  bslib[page_sidebar, sidebar, navset_hidden, nav_panel],
  shinyjs[useShinyjs, addClass, removeClass],
)
box::use(
  app / logic / functions[sidebar_menu],
  app / view / database_browse_entries,
  app / view / database_scheme_info,
  app / view / database_loci_info,
  app / view / database_distance_matrix,
  app / view / database_missing_values,
)

# Sidebar menu definition
db_menu <- list(
  list(
    value = "browse_entries",
    label = "Browse Entries",
    module = database_browse_entries
  ),
  list(
    value = "scheme_info",
    label = "Scheme Info",
    module = database_scheme_info
  ),
  list(
    value = "loci_info",
    label = "Loci Info",
    module = database_loci_info
  ),
  list(
    value = "distance_matrix",
    label = "Distance Matrix",
    module = database_distance_matrix
  ),
  list(
    value = "missing_values",
    label = "Missing Values",
    module = database_missing_values
  )
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    useShinyjs(),
    fillable = TRUE,
    sidebar = sidebar(
      title = "Database Browser",
      width = 350,
      sidebar_menu(ns, db_menu)
    ),
    # Hidden tabset: one panel per interface, swapped via nav_select(). Each
    # panel hosts its module's UI under the module's own namespace.
    do.call(
      navset_hidden,
      c(
        list(id = ns("pages")),
        lapply(db_menu, function(item) {
          nav_panel(
            title = item$label,
            value = item$value,
            item$module$ui(ns(item$value))
          )
        })
      )
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
    # Switches the visible sub-panel back to Browse Entries and clears the
    # active highlight from all other sidebar buttons.
    observeEvent(
      session_reset(),
      {
        bslib::nav_select("pages", selected = "browse_entries")
        for (item in db_menu) {
          removeClass(paste0("menu_", item$value), "active")
        }
        addClass("menu_browse_entries", "active")
      },
      ignoreInit = TRUE
    )

    # Start each interface module's server under its own namespace and forward
    # session_reset so each sub-module can reset its own state.
    for (item in db_menu) {
      item$module$server(
        item$value,
        db_path = db_path,
        session_reset = session_reset
      )
    }

    # Each menu button switches the main field to its panel and keeps the
    # `active` highlight (hover colour) on the current selection.
    lapply(db_menu, function(item) {
      observeEvent(input[[paste0("menu_", item$value)]], {
        bslib::nav_select("pages", selected = item$value)

        for (other in db_menu) {
          removeClass(paste0("menu_", other$value), "active")
        }
        addClass(paste0("menu_", item$value), "active")
      })
    })
  })
}
