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
  ],
  waiter[Waiter, spin_3, transparent, spin_flower],
  fs[dir_ls, path_home],
  bslib[page_fillable],
  shinyFiles[
    shinyFileChoose,
    shinyFilesButton,
    parseFilePaths
  ],
  DT[DTOutput, renderDT, datatable],
)

box::use(
  app / logic / functions[render_info],
  app / logic / paths[app_local_share_path, stat_json],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
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
server <- function(id, external_db = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    Startup <- reactiveValues(db_location = NULL)

    # Observe a database location supplied from outside (e.g. a freshly
    # downloaded scheme from the scheme browser) as an alternative to the
    # shinyFileChoose selection.
    observeEvent(external_db(), {
      db_path <- external_db()
      req(!is.null(db_path), length(db_path), is.character(db_path))

      Startup$db_location <- if (file.exists(db_path)) {
        c("Currently selected:", db_path)
      } else {
        c("Currently selected:", NA)
      }
    })

    # Render PhyloTrace logo
    output$phylotrace_large <- renderImage(
      {
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

    # Observe current database path
    observe({
      db_location <- NULL

      location_input <- parseFilePaths(
        roots = c(Home = path_home(), Root = "/"),
        input$db_location
      )$datapath

      if (length(location_input)) {
        # Case valid database input
        if (is.character(location_input) && file.exists(location_input)) {
          # Case currently selected database is valid
          db_location <- c("Currently selected:", location_input)
        } else {
          # Case currently selected database invalid
          db_location <- c("Currently selected:", NA)
        }
      } else if (
        !is.null(stat_json) &&
          length(stat_json$last_db) &&
          file.exists(stat_json$last_db) &&
          endsWith(stat_json$last_db, ".db")
      ) {
        db_location <- c("Last used:", stat_json$last_db)
      }

      Startup$db_location <- db_location
    })

    # Render database selection interface
    output$database_selection <- renderUI({
      req(Startup$db_location)

      if (is.na(Startup$db_location[2])) {
        # Case no database selected
        table <- "No database selected"
      } else {
        # Case valid database selected
        table <- DTOutput(ns("selected_database"))
      }

      div(
        p(Startup$db_location[1]),
        table
      )
    })

    # Render selected database
    output$selected_database <- renderDT({
      # Get database metadata
      db_path <- Startup$db_location[2]
      req(is.character(db_path) && !is.na(db_path) && file.exists(db_path))
      db_metadata <- file.info(db_path)
      db_name <- basename(db_path)
      db_time <- format(db_metadata$mtime, "%Y-%m-%d %H:%M:%S")
      db_size <- if (db_metadata$size >= 1024^3) {
        paste0(round(db_metadata$size / 1024^3, 2), " GB")
      } else if (db_metadata$size >= 1024^2) {
        paste0(round(db_metadata$size / 1024^2, 2), " MB")
      } else if (db_metadata$size >= 1024) {
        paste0(round(db_metadata$size / 1024, 2), " KB")
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
    reactiveValues(
      create_scheme = shiny::reactive(input$create_new_db)
    )
  })
}
