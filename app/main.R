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
    reactiveVal,
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
    toggle_dark_mode
  ],
  shinyjs[runjs],
  htmltools[tagQuery],
  waiter[useWaiter, waiterShowOnLoad],
)
box::use(
  app / logic / functions[render_info],
  app / logic / paths[stat_json, app_local_share_path],
  app / view / startup,
  app / view / scheme_browser,
  app / view / database,
  app / view / typing,
  app / view / analysis_dashboard,
  app / view / visualization,
  app / view / resistance_screening,
)

fillable_panel <- function(...) {
  as_fill_carrier(nav_panel(...))
}

# shinyFiles asset stripping logic
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

  tagList(
    useWaiter(),
    waiterShowOnLoad(
      html = div(
        class = "waiter-splash",
        tags$img(
          src = "static/images/PhyloTrace_flat_256.png",
          width = "200px",
          height = "200px"
        ),
        div(class = "waiter-splash-title", "PhyloTrace")
      )
    ),
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
      # Baseline Bootstrap 5 theme; the navbar button toggles its light/dark mode.
      theme = bs_theme(version = 5, preset = "shiny"),
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
      nav_item("v1.6.1"),
      nav_item(
        actionButton(
          inputId = ns("toggle_dark"),
          label = NULL,
          icon = icon("moon"),
          title = "Toggle Light/Dark Mode"
        )
      ),
      nav_item(
        actionButton(
          inputId = ns("quit"),
          label = NULL,
          icon = icon("power-off"),
          title = "Turn Off"
        )
      )
    )
  ) # close tagList
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Kill server on session end
    session$onSessionEnded(function() {
      stopApp()
    })

    # Light/dark mode toggle
    observeEvent(input$toggle_dark, {
      toggle_dark_mode()
    })

    # Close application
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

    observeEvent(input$conf_shutdown, {
      removeModal()
      runjs("window.close();")
      later::later(stopApp, delay = 0.5)
    })

    # Shared reset signal: incremented each time the user resets the session.
    # Modules observe it with ignoreInit = TRUE and tear down their own state.
    session_reset <- reactiveVal(0L)

    SCHEME_BROWSER_vals <- scheme_browser$server(
      "scheme_browser",
      session_reset = session_reset
    )

    scheme_browser_db <- reactive({
      SCHEME_BROWSER_vals$load_db()
      isolate(SCHEME_BROWSER_vals$db_location())
    })

    STARTUP_vals <- startup$server(
      "startup",
      external_db = scheme_browser_db,
      session_reset = session_reset
    )

    database$server(
      "database",
      db_path = STARTUP_vals$db_path,
      session_reset = session_reset
    )
    typing$server(
      "typing",
      db_path = STARTUP_vals$db_path,
      session_reset = session_reset
    )
    analysis_dashboard$server(
      "analysis_dashboard",
      db_path = STARTUP_vals$db_path,
      session_reset = session_reset
    )
    visualization$server(
      "visualization",
      db_path = STARTUP_vals$db_path,
      session_reset = session_reset
    )
    resistance_screening$server(
      "resistance_screening",
      db_path = STARTUP_vals$db_path,
      session_reset = session_reset
    )

    observeEvent(STARTUP_vals$create_scheme(), {
      nav_select(id = "tabs", selected = "scheme_browser_panel")
    })

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
          value = "resistance_screening_panel",
          strip_shinyfiles_assets(resistance_screening$ui(ns(
            "resistance_screening"
          )))
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

      db_path <- STARTUP_vals$db_path()

      nav_insert(
        id = "tabs",
        nav = nav_item(
          actionButton(
            inputId = ns("reset"),
            label = NULL,
            icon = icon("arrow-rotate-left"),
            title = "Return to the start screen"
          )
        ),
        target = "resistance_screening_panel",
        position = "after"
      )

      nav_insert(
        id = "tabs",
        nav = nav_item(
          style = "margin-left: auto;",
          if (length(db_path) && !is.na(db_path)) {
            div(
              id = "loaded-db-path",
              title = db_path,
              basename(db_path)
            )
          }
        ),
        target = "resistance_screening_panel",
        position = "after"
      )

      nav_hide(id = "tabs", target = "startup_panel")
      nav_hide(id = "tabs", target = "scheme_browser_panel")

      if (!is.null(stat_json$last_db) && file.exists(stat_json$last_db)) {
        stat_json$last_db <- db_path
      } else {
        stat_json <- list(last_db = db_path)
      }
      jsonlite::write_json(
        stat_json,
        file.path(app_local_share_path, "state.json"),
        pretty = TRUE,
        auto_unbox = TRUE
      )
    })

    # Increment the shared reset signal so every subscribed module observer
    # fires and tears down its own reactive state (files, results, processes).
    reset_session <- function() {
      session_reset(session_reset() + 1L)
    }

    observeEvent(input$reset, {
      nav_show(id = "tabs", target = "startup_panel", select = TRUE)
      nav_show(id = "tabs", target = "scheme_browser_panel")

      for (panel in c(
        "database_panel",
        "analysis_dashboard_panel",
        "visualization_panel",
        "typing_panel",
        "resistance_screening_panel"
      )) {
        nav_remove(id = "tabs", target = panel)
      }

      # Remove the two dynamically inserted navbar items (reset button and
      # db-path display) by finding their known HTML element ids and walking
      # up to the enclosing <li class="nav-item"> Bootstrap generates.
      runjs(sprintf(
        "['%s','loaded-db-path'].forEach(function(id){
           var el=document.getElementById(id);
           if(el){var li=el.closest('li.nav-item');if(li)li.remove();}
         });",
        ns("reset")
      ))

      reset_session()
    })
  })
}
