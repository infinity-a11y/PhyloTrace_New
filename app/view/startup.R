# app/view/startup.R

box::use(
  shiny[
    NS,
    actionButton,
    icon,
    p,
    tagList,
    moduleServer,
    h4,
    req,
    reactiveValues,
    reactiveVal,
    div,
    imageOutput,
    observe,
    renderImage,
    uiOutput,
    renderUI,
    observeEvent,
    modalDialog,
    modalButton,
    showModal,
    selectInput,
    tags,
  ],
  waiter[Waiter, spin_3, transparent, spin_flower],
  fs[dir_ls, path_home],
  jsonlite[fromJSON],
  bslib[page_fillable],
  shinyFiles[
    shinyFileChoose,
    shinyFilesButton,
    parseFilePaths
  ],
  DT[DTOutput, renderDT, datatable],
  shinyjs[useShinyjs, disabled]
)

box::use(
  app / logic / functions[render_info],
  app / logic / paths[stat_json, app_local_share_path],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    useShinyjs(),
    imageOutput(ns("phylotrace_large")),
    div(
      class = "startup-ui",
      div(
        id = "loading-instructions",
        div(
          id = "instructions",
          'Proceed by loading a compatible local database or create a new one.'
        ),
        div(
          id = "loading-inputs",
          shinyFilesButton(
            ns("db_location"),
            "Select Database",
            icon = icon("folder-open"),
            title = "Choose a database",
            buttonType = "default",
            root = path_home(),
            multiple = FALSE
          ),
          actionButton(
            ns("create_new_db"),
            "Create New",
            icon = icon("plus"),
            title = "Choose location for new database",
            buttonType = "default",
            root = path_home()
          )
        ),
        uiOutput(ns("database_selection"))
      )
    )
  )
}

#' @export
server <- function(
  id,
  external_db = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    Startup <- reactiveValues(db_location = NULL)

    # Combined load trigger: incremented by the UI button and by the external
    # db observer so that both paths share one downstream observeEvent.
    load_trigger <- reactiveVal(NULL)
    fire_load <- function() {
      n <- load_trigger()
      load_trigger(if (is.null(n)) 1L else n + 1L)
    }

    # Incrementing this forces the db-location observe() to re-run after reset,
    # even when input$db_location hasn't changed.
    db_location_trigger <- reactiveVal(0L)

    observeEvent(
      session_reset(),
      {
        load_trigger(NULL)
        db_location_trigger(db_location_trigger() + 1L)
      },
      ignoreInit = TRUE
    )

    # Observe a database location
    observeEvent(external_db(), {
      db_path <- external_db()
      req(!is.null(db_path), length(db_path), is.character(db_path))

      Startup$db_location <- if (file.exists(db_path)) {
        c("Currently selected:", db_path)
      } else {
        c("Currently selected:", NA)
      }

      if (!is.na(Startup$db_location[2])) fire_load()
    })

    observeEvent(input$load_database, {
      fire_load()
    })

    # Render PhyloTrace logo
    output$phylotrace_large <- renderImage(
      {
        render_info("output$phylotrace_large")

        list(
          src = file.path(getwd(), "app/static/images/PhyloTrace_flat_512.png")
        )
      },
      deleteFile = FALSE
    )

    # Present DB location dir choose
    shinyFileChoose(
      input,
      "db_location",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      filetypes = c('db'),
      session = session
    )

    # Observe current database path.
    # Depends on db_location_trigger so it also re-runs after a session reset,
    # even when input$db_location has not changed.
    # Falls back to disk (not in-memory stat_json) so it picks up whatever
    # main.R wrote during this session.
    observe({
      db_location_trigger()

      db_location <- NULL

      location_input <- parseFilePaths(
        roots = c(Home = path_home(), Root = "/"),
        input$db_location
      )$datapath

      if (length(location_input)) {
        if (is.character(location_input) && file.exists(location_input)) {
          db_location <- c("Currently selected:", location_input)
        } else {
          db_location <- c("Currently selected:", NA)
        }
      } else {
        state_file <- file.path(app_local_share_path, "state.json")
        disk_stat <- tryCatch(
          if (file.exists(state_file)) fromJSON(state_file) else NULL,
          error = function(e) NULL
        )
        if (
          !is.null(disk_stat) &&
            length(disk_stat$last_db) &&
            file.exists(disk_stat$last_db) &&
            endsWith(disk_stat$last_db, ".db")
        ) {
          db_location <- c("Last used:", disk_stat$last_db)
        }
      }

      Startup$db_location <- db_location
    })

    # Render database selection interface
    output$database_selection <- renderUI({
      req(Startup$db_location)

      render_info("output$database_selection")

      db_unselected <- is.na(Startup$db_location[2])

      load_button <- actionButton(
        ns("load_database"),
        "Load Database"
      )

      if (db_unselected) {
        # Case no database selected
        table <- "No database selected"
        load_button <- disabled(load_button)
      } else {
        # Case valid database selected
        table <- DTOutput(ns("selected_database"))
      }

      div(
        p(Startup$db_location[1]),
        table,
        load_button
      )
    })

    # Render selected database
    output$selected_database <- renderDT({
      render_info("output$selected_database")

      # Get database metadata
      db_path <- Startup$db_location[2]
      req(is.character(db_path) && !is.na(db_path) && file.exists(db_path))
      db_metadata <- file.info(db_path)
      db_name <- basename(db_path)
      db_time <- format(db_metadata$mtime, "%Y-%m-%d %H:%M:%S")
      db_size <- if (db_metadata$size >= 1000^3) {
        paste0(round(db_metadata$size / 1000^3, 2), " GB")
      } else if (db_metadata$size >= 1000^2) {
        paste0(round(db_metadata$size / 1000^2, 2), " MB")
      } else if (db_metadata$size >= 1000) {
        paste0(round(db_metadata$size / 1000, 2), " KB")
      } else {
        paste0(db_metadata$size, " Bytes")
      }

      # Render database metadata table
      datatable(
        data.frame(
          Property = c("Name", "Location", "Size", "Last Changed"),
          Value = c(
            db_name,
            db_path,
            db_size,
            db_time
          )
        ),
        class = 'stripe row-border order-column',
        colnames = c("", ""),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", ordering = FALSE)
      )
    })

    # Return values
    list(
      create_scheme = shiny::reactive(input$create_new_db),
      load_database = shiny::reactive(load_trigger()),
      db_path = shiny::reactive(Startup$db_location[2])
    )
  })
}
