box::use(
  shiny[
    stopApp,
    moduleServer,
    NS,
    observeEvent,
    tags,
    div,
    actionButton,
    showModal,
    removeModal,
    modalDialog,
    modalButton,
    icon,
    tagList,
    reactive,
    isolate
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
    nav_insert,
    nav_remove,
    nav_hide,
    nav_show,
    as_fill_carrier,
  ],
  shinyjs[runjs],
  htmltools[tagQuery],
)
box::use(
  app / logic / functions[render_info],
  app / view / startup,
  app / view / scheme_browser,
  app / view / database,
  app / view / typing,
  app / view / analysis_dashboard,
  app / view / visualization,
  app / view / amr_screening,
)

# nav_insert() does not apply the fillable wrapping that page_navbar() gives
# statically-defined panels, so a panel added at runtime would not fill the
# page. Marking it as a fill carrier reproduces the static behaviour.
fillable_panel <- function(...) {
  as_fill_carrier(nav_panel(...))
}

# shinyFiles ships its client script/styles via singleton(tags$head(...)), which
# Shiny does NOT de-duplicate across nav_insert(). A panel inserted at runtime
# that contains a shinyFiles button (Typing, the Database submodules, ...) would
# therefore re-inject and re-run shinyFiles.js, registering a second
# document-level click handler and opening the file dialog twice for every
# shinyFiles control in the app. The static startup panel already loads these
# assets once at page render, and shinyFiles binds its handlers by delegation on
# `document` (so they drive buttons added later too); we drop the redundant
# copies from inserted panels. tagQuery() walks the whole subtree, so nested
# submodule buttons are covered as well.
strip_shinyfiles_assets <- function(ui) {
  tagQuery(ui)$find("script")$filter(function(node, i) {
    src <- node$attribs$src
    !is.null(src) && grepl("^sF/", src)
  })$remove()$resetSelected()$find("link")$filter(function(node, i) {
    href <- node$attribs$href
    !is.null(href) && grepl("^sF/", href)
  })$remove()$allTags()
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    id = ns("tabs"),
    title = div(
      id = "navbar-title",
      tags$img(
        src = "static/images/PhyloTrace_flat_128.png",
      ),
      div("PhyloTrace")
    ),
    window_title = "PhyloTrace",
    navbar_options = navbar_options(underline = TRUE),
    nav_panel(
      title = "Load Database",
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
    )
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
    SCHEME_BROWSER_vals <- scheme_browser$server("scheme_browser")

    # Database location assembled in the scheme browser, captured on each
    # "Load Database" click and handed to the startup module.
    scheme_browser_db <- reactive({
      SCHEME_BROWSER_vals$load_db()
      isolate(SCHEME_BROWSER_vals$db_location())
    })

    STARTUP_vals <- startup$server("startup", external_db = scheme_browser_db) # startup module

    # Main application module servers
    database$server("database")
    typing$server("typing", db_path = STARTUP_vals$db_path)
    analysis_dashboard$server("analysis_dashboard")
    visualization$server("visualization")
    amr_screening$server("amr_screening")

    # Event show scheme browser UI
    observeEvent(STARTUP_vals$create_scheme(), {
      nav_select(id = "tabs", selected = "scheme_browser_panel")
    })

    # Event return to startup with the freshly downloaded database loaded
    observeEvent(SCHEME_BROWSER_vals$load_db(), {
      nav_select(id = "tabs", selected = "startup_panel")
    })

    # Event swap startup panels for the main application panels once a
    # database has been loaded
    observeEvent(STARTUP_vals$load_database(), {
      app_panels <- list(
        fillable_panel(
          "Database Browser",
          value = "database_panel",
          strip_shinyfiles_assets(database$ui(ns("database")))
        ),
        fillable_panel(
          "Analysis Dashboard",
          value = "analysis_dashboard_panel",
          strip_shinyfiles_assets(analysis_dashboard$ui(ns(
            "analysis_dashboard"
          )))
        ),
        fillable_panel(
          "Visualization",
          value = "visualization_panel",
          strip_shinyfiles_assets(visualization$ui(ns("visualization")))
        ),
        fillable_panel(
          "Allelic Typing",
          value = "typing_panel",
          strip_shinyfiles_assets(typing$ui(ns("typing")))
        ),
        fillable_panel(
          "Resistance Screening",
          value = "amr_screening_panel",
          strip_shinyfiles_assets(amr_screening$ui(ns("amr_screening")))
        )
      )

      targets <- c(
        "scheme_browser_panel",
        "database_panel",
        "analysis_dashboard_panel",
        "visualization_panel",
        "typing_panel"
      )

      for (i in seq_along(app_panels)) {
        nav_insert(
          id = "tabs",
          nav = app_panels[[i]],
          target = targets[i],
          position = "after",
          select = i == 1L
        )
      }

      # Navbar session controls: loaded database name
      db_path <- STARTUP_vals$db_path()
      nav_insert(
        id = "tabs",
        nav = nav_item(
          style = "margin-left: auto;",
          div(
            id = "session-controls",
            if (length(db_path) && !is.na(db_path)) {
              div(
                id = "loaded-db-path",
                title = db_path,
                basename(db_path)
              )
            },
            tags$a(
              id = "reset-session",
              style = "cursor: pointer;",
              title = "Return to the start screen",
              onclick = paste0(
                "Shiny.setInputValue('",
                ns("reset"),
                "', Math.random());"
              ),
              icon("arrow-rotate-left")
            )
          )
        ),
        target = "amr_screening_panel",
        position = "after"
      )

      # Hide ( not remove) the startup-phase panels
      nav_hide(id = "tabs", target = "startup_panel")
      nav_hide(id = "tabs", target = "scheme_browser_panel")
    })

    # Central teardown of everything the main application set up, returning the
    # app to its initial state. The navbar swap is handled by the reset event
    # below; this is the hook for backend state. As the modules grow, reset
    # their reactive values / dynamic UI here
    reset_session <- function() {}

    # Event roll back to the initial startup interface
    observeEvent(input$reset, {
      # Reveal the startup-phase panels that were hidden on load
      nav_show(id = "tabs", target = "startup_panel", select = TRUE)
      nav_show(id = "tabs", target = "scheme_browser_panel")

      # Remove the main application panels
      for (panel in c(
        "database_panel",
        "analysis_dashboard_panel",
        "visualization_panel",
        "typing_panel",
        "amr_screening_panel"
      )) {
        nav_remove(id = "tabs", target = panel)
      }

      # Remove the session controls as nav_remove() cannot target it
      runjs(
        "var el = document.getElementById('session-controls');
         if (el && el.closest('li')) el.closest('li').remove();"
      )

      # Reset backend state
      reset_session()
    })
  })
}
