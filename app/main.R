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
)
box::use(
  app / logic / functions[render_info],
  app / view / startup,
  app / view / scheme_browser,
  app / view / database,
  app / view / typing,
  app / view / visualization,
  app / view / amr_screening,
)

# nav_insert() does not apply the fillable wrapping that page_navbar() gives
# statically-defined panels, so a panel added at runtime would not fill the
# page. Marking it as a fill carrier reproduces the static behaviour.
fillable_panel <- function(...) {
  as_fill_carrier(nav_panel(...))
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
    SCHEME_BROWSER_vals <- scheme_browser$server("scheme_browser") # scheme_browser module

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

    # The main application panels are built exactly once (see below); this
    # tracks whether that has happened yet.
    panels_built <- FALSE
    app_panel_values <- c(
      "database_panel",
      "typing_panel",
      "visualization_panel",
      "amr_screening_panel"
    )

    # Event swap startup panels for the main application panels once a
    # database has been loaded
    observeEvent(STARTUP_vals$load_database(), {
      # Build the main application panels exactly once. On later loads they are
      # only revealed again (the `else` branch). They host shinyFiles buttons
      # (e.g. the Typing file/folder choosers) whose modal containers are
      # appended to <body> and which register a document-level click handler;
      # removing and re-inserting the panels would duplicate that handler and
      # open the file dialog twice. Hiding/showing keeps the bindings intact.
      if (!panels_built) {
        app_panels <- list(
          fillable_panel(
            "Database",
            value = "database_panel",
            database$ui(ns("database"))
          ),
          fillable_panel(
            "Typing",
            value = "typing_panel",
            typing$ui(ns("typing"))
          ),
          fillable_panel(
            "Visualization",
            value = "visualization_panel",
            visualization$ui(ns("visualization"))
          ),
          fillable_panel(
            "AMR Screening",
            value = "amr_screening_panel",
            amr_screening$ui(ns("amr_screening"))
          )
        )
        targets <- c(
          "scheme_browser_panel",
          "database_panel",
          "typing_panel",
          "visualization_panel"
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

        panels_built <<- TRUE
      } else {
        for (panel in app_panel_values) {
          nav_show(
            id = "tabs",
            target = panel,
            select = panel == "database_panel"
          )
        }
      }

      # Right-aligned session controls: the loaded database name (file name
      # only, full path on hover) and a button to roll back to the startup
      # interface. `margin-left: auto` on the item, together with the navbar's
      # trailing spacer, opens a gap between the panels and this block. A single
      # insert keeps placement deterministic and removal trivial on rollback.
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

      # Hide (do not remove) the startup-phase panels. These contain shinyFiles
      # buttons whose modal containers are appended to <body>; removing and
      # later re-inserting the panels would orphan the old modal and create a
      # duplicate with the same id, causing a second file dialog to appear.
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
      # Reveal the startup-phase panels that were hidden on load. They were never
      # removed, so their shinyFiles buttons keep their original bindings (no
      # duplicate file dialog) and their fillable wrapping is intact.
      nav_show(id = "tabs", target = "startup_panel", select = TRUE)
      nav_show(id = "tabs", target = "scheme_browser_panel")

      # Hide (do not remove) the main application panels, for the same reason the
      # startup panels are hidden rather than removed: the Typing panel hosts
      # shinyFiles buttons, and a remove/re-insert cycle would duplicate the
      # shinyFiles document click handler and open the file dialog twice. They
      # are revealed again on the next load.
      for (panel in app_panel_values) {
        nav_hide(id = "tabs", target = panel)
      }

      # Remove the session controls as nav_remove() cannot target it. It carries
      # no shinyFiles dependency, so it is safe to re-create on the next load
      # (which also refreshes the displayed database name).
      runjs(
        "var el = document.getElementById('session-controls');
         if (el && el.closest('li')) el.closest('li').remove();"
      )

      # Reset backend state
      reset_session()
    })
  })
}
