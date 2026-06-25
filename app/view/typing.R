# app/view/typing.R

box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    reactiveVal,
    reactiveValues,
    observe,
    observeEvent,
    req,
    invalidateLater,
    renderUI,
    uiOutput,
    renderText,
    textOutput,
    verbatimTextOutput,
    div,
    p,
    span,
    icon,
    actionButton,
    numericInput,
  ],
  bslib[
    page_sidebar,
    sidebar,
    card,
    card_header,
    card_body,
    layout_columns,
    accordion,
    accordion_panel,
  ],
  shinyFiles[
    shinyFilesButton,
    shinyFileChoose,
    parseFilePaths,
    shinyDirButton,
    shinyDirChoose,
    parseDirPath,
  ],
  shinyWidgets[progressBar, updateProgressBar, show_toast],
  shinyjs[useShinyjs, disabled, toggleState],
  DT[DTOutput, renderDT, datatable],
  fs[path_home],
)
box::use(
  app / logic / functions[render_info],
  app / logic / pymlst[start_typing, parse_typing_log, existing_strains],
)

# Accepted assembly extensions, mirroring the glob in loop-pymlst.sh.
genome_pattern <- "\\.(fasta|fa|fna)$"

# Bootstrap badge class per typing outcome. Bootstrap 5 (shipped with bslib)
# provides these `text-bg-*` utilities, so no custom CSS is needed.
status_badge <- function(status) {
  cls <- switch(
    status,
    Added = "text-bg-success",
    Duplicate = "text-bg-secondary",
    Incompatible = "text-bg-danger",
    Error = "text-bg-danger",
    Running = "text-bg-info",
    Pending = "text-bg-light text-dark",
    New = "text-bg-success",
    "Already present" = "text-bg-warning",
    "text-bg-light"
  )
  sprintf('<span class="badge %s">%s</span>', cls, status)
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    useShinyjs(),
    fillable = TRUE,
    sidebar = sidebar(
      title = "Allelic Typing",
      width = 320,
      p(
        class = "text-muted",
        "Select a single assembled genome (.fasta, .fa, .fna) or a folder of ",
        "assemblies to type against the loaded scheme."
      ),
      shinyFilesButton(
        ns("genome_file"),
        "Select File",
        title = "Choose a genome assembly",
        icon = icon("file-lines"),
        buttonType = "default",
        multiple = FALSE,
        root = path_home()
      ),
      shinyDirButton(
        ns("genome_dir"),
        "Select Folder",
        title = "Choose a folder of assemblies",
        icon = icon("folder-open"),
        buttonType = "default",
        root = path_home()
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          "Parameters",
          icon = icon("sliders"),
          numericInput(
            ns("identity"),
            "Min. identity",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01
          ),
          numericInput(
            ns("coverage"),
            "Min. coverage",
            value = 0.9,
            min = 0,
            max = 1,
            step = 0.01
          )
        )
      ),
      disabled(actionButton(
        ns("start"),
        "Start Typing",
        icon = icon("play"),
        class = "btn-primary"
      )),
      disabled(actionButton(
        ns("terminate"),
        "Terminate",
        icon = icon("stop"),
        class = "btn-danger"
      )),
      uiOutput(ns("status_badge"))
    ),
    div(
      progressBar(
        ns("progress"),
        value = 0,
        total = 1,
        display_pct = TRUE,
        status = "primary",
        striped = TRUE,
        title = "Typing progress"
      ),
      textOutput(ns("current_strain"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark",
          "Selected Genomes"
        ),
        card_body(DTOutput(ns("selection_table")))
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark",
          "Typing Log"
        ),
        card_body(
          max_height = 320,
          verbatimTextOutput(ns("log"))
        )
      )
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "bg-dark",
        "Results"
      ),
      card_body(DTOutput(ns("results_table")))
    )
  )
}

#' @export
server <- function(id, db_path = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    Typing <- reactiveValues(
      genome_input = NULL,
      files = character(0),
      strains = character(0),
      proc = NULL,
      log_file = NULL,
      status = "idle",
      results = NULL,
      terminated = FALSE,
      refresh = 0L
    )
    log_text <- reactiveVal("")

    roots <- c(Home = path_home(), Root = "/")

    or_default <- function(x, default) {
      if (is.null(x) || length(x) != 1 || is.na(x)) default else x
    }

    valid_db <- reactive({
      path <- db_path()
      !is.null(path) &&
        length(path) == 1 &&
        is.character(path) &&
        !is.na(path) &&
        file.exists(path)
    })

    # A selection (file or folder) resolves to the genome_input handed to the
    # typing script plus the ordered list of assemblies / strain names it will
    # produce (strain name = file name without extension, as in loop-pymlst.sh).
    set_selection <- function(path) {
      if (identical(Typing$status, "running")) {
        return(invisible())
      }

      files <- if (dir.exists(path)) {
        list.files(path, pattern = genome_pattern, full.names = TRUE)
      } else {
        path
      }

      Typing$genome_input <- path
      Typing$files <- files
      Typing$strains <- sub("\\.[^.]*$", "", basename(files))
      Typing$results <- NULL
      log_text("")
      updateProgressBar(session, "progress", value = 0, total = 1)
    }

    # File / folder choosers
    shinyFileChoose(
      input,
      "genome_file",
      roots = roots,
      defaultRoot = "Home",
      filetypes = c("fasta", "fa", "fna"),
      session = session
    )
    shinyDirChoose(
      input,
      "genome_dir",
      roots = roots,
      defaultRoot = "Home",
      session = session
    )

    observeEvent(input$genome_file, {
      path <- parseFilePaths(roots, input$genome_file)$datapath
      req(length(path), is.character(path), file.exists(path))
      set_selection(path)
    })

    observeEvent(input$genome_dir, {
      path <- parseDirPath(roots, input$genome_dir)
      req(length(path), is.character(path), dir.exists(path))
      set_selection(path)
    })

    # Strains already in the loaded database; re-queried after each run so the
    # selection table's "Already present" flags stay current.
    existing <- reactive({
      Typing$refresh
      existing_strains(db_path())
    })

    # Enable typing only with a valid database, at least one assembly, and no
    # run in flight. Terminate mirrors the running state.
    observe({
      ready <- isTRUE(valid_db()) &&
        length(Typing$strains) > 0 &&
        !identical(Typing$status, "running")
      toggleState("start", condition = ready)
      toggleState("terminate", condition = identical(Typing$status, "running"))
    })

    # Selection overview
    output$selection_table <- renderDT({
      render_info("output$selection_table")

      if (!length(Typing$files)) {
        return(datatable(
          data.frame(" " = "No genomes selected yet.", check.names = FALSE),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      present <- Typing$strains %in% existing()
      table <- data.frame(
        File = basename(Typing$files),
        Strain = Typing$strains,
        Status = vapply(
          ifelse(present, "Already present", "New"),
          status_badge,
          character(1)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      datatable(
        table,
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "tp", pageLength = 8, ordering = FALSE)
      )
    })

    # Live log
    output$log <- renderText({
      text <- log_text()
      if (!nzchar(text)) "No typing run yet." else text
    })

    # Current strain / phase line under the progress bar
    output$current_strain <- renderText({
      results <- Typing$results
      if (is.null(results)) {
        return("")
      }
      running <- results$strain[results$status == "Running"]
      if (length(running)) {
        paste("Typing:", running[1])
      } else if (identical(Typing$status, "done")) {
        "All genomes processed."
      } else {
        ""
      }
    })

    # Status badge in the sidebar
    output$status_badge <- renderUI({
      spec <- switch(
        Typing$status,
        idle = c("secondary", "Idle"),
        running = c("info", "Running ..."),
        done = c("success", "Complete"),
        terminated = c("warning", "Terminated"),
        failed = c("danger", "Failed"),
        c("secondary", Typing$status)
      )
      div(
        class = "mt-2",
        span(class = paste0("badge text-bg-", spec[1]), spec[2])
      )
    })

    # Per-strain results
    output$results_table <- renderDT({
      render_info("output$results_table")

      results <- Typing$results
      if (is.null(results) || !nrow(results)) {
        return(datatable(
          data.frame(" " = "No results yet.", check.names = FALSE),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      table <- data.frame(
        Strain = results$strain,
        Status = vapply(results$status, status_badge, character(1)),
        `Genes found` = ifelse(
          is.na(results$genes_found),
          "-",
          as.character(results$genes_found)
        ),
        Detail = results$detail,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      datatable(
        table,
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "tp", pageLength = 8, ordering = FALSE)
      )
    })

    # Start typing
    observeEvent(input$start, {
      req(valid_db(), length(Typing$strains) > 0)
      if (identical(Typing$status, "running")) {
        return()
      }

      Typing$log_file <- tempfile(fileext = ".log")
      file.create(Typing$log_file)
      log_text("")
      Typing$terminated <- FALSE
      # Seed an all-Pending table so the queue is visible immediately.
      Typing$results <- parse_typing_log(character(0), Typing$strains)

      proc <- tryCatch(
        start_typing(
          db_path = db_path(),
          genome_input = Typing$genome_input,
          log_file = Typing$log_file,
          identity = or_default(input$identity, 0.95),
          coverage = or_default(input$coverage, 0.9),
          env = "pymlst"
        ),
        error = function(e) e
      )

      if (inherits(proc, "error")) {
        Typing$status <- "failed"
        show_toast(
          title = "Error",
          text = paste("Could not start typing:", conditionMessage(proc)),
          type = "error",
          timer = 6000
        )
        return()
      }

      Typing$proc <- proc
      Typing$status <- "running"
      updateProgressBar(
        session,
        "progress",
        value = 0,
        total = length(Typing$strains)
      )
      show_toast(
        title = "Typing started",
        text = paste(length(Typing$strains), "genome(s) queued."),
        type = "info",
        timer = 3000
      )
    })

    # Terminate a running batch
    observeEvent(input$terminate, {
      req(identical(Typing$status, "running"), !is.null(Typing$proc))
      Typing$terminated <- TRUE
      tryCatch(Typing$proc$kill(), error = function(e) NULL)
    })

    # Poll the background process: tail the log, refresh the results table and
    # the progress bar, and finalise once the process exits. invalidateLater
    # returns control to the event loop each tick so updates reach the browser
    # live and the UI stays responsive while typing runs.
    observe({
      if (!identical(Typing$status, "running")) {
        return(NULL)
      }

      proc <- Typing$proc
      alive <- !is.null(proc) && proc$is_alive()

      lines <- if (!is.null(Typing$log_file) && file.exists(Typing$log_file)) {
        readLines(Typing$log_file, warn = FALSE)
      } else {
        character(0)
      }
      log_text(paste(lines, collapse = "\n"))

      results <- parse_typing_log(lines, Typing$strains)
      Typing$results <- results
      done <- sum(
        results$status %in% c("Added", "Duplicate", "Incompatible", "Error")
      )
      updateProgressBar(
        session,
        "progress",
        value = done,
        total = max(1L, length(Typing$strains))
      )

      if (alive) {
        invalidateLater(700, session)
        return(NULL)
      }

      # Finished (completed or killed)
      Typing$status <- if (isTRUE(Typing$terminated)) "terminated" else "done"
      Typing$refresh <- Typing$refresh + 1L

      added <- sum(results$status == "Added")
      duplicate <- sum(results$status == "Duplicate")
      failed <- sum(results$status %in% c("Incompatible", "Error"))
      show_toast(
        title = if (identical(Typing$status, "terminated")) {
          "Typing terminated"
        } else {
          "Typing complete"
        },
        text = sprintf(
          "%d added, %d duplicate, %d failed.",
          added,
          duplicate,
          failed
        ),
        type = if (failed > 0) "warning" else "success",
        timer = 6000
      )
    })
  })
}
