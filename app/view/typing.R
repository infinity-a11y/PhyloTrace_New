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
    outputOptions,
    req,
    invalidateLater,
    renderUI,
    uiOutput,
    renderText,
    isolate,
    textOutput,
    verbatimTextOutput,
    div,
    p,
    span,
    tags,
    icon,
    actionButton,
    numericInput,
    showNotification,
    HTML
  ],
  stats[setNames],
  bslib[
    page_sidebar,
    sidebar,
    accordion,
    accordion_panel,
    accordion_panel_close,
    as_fill_item,
    tooltip,
  ],
  shinyFiles[
    shinyFilesButton,
    shinyFileChoose,
    parseFilePaths,
    shinyDirButton,
    shinyDirChoose,
    parseDirPath,
  ],
  shinyWidgets[progressBar, updateProgressBar],
  shinyjs[
    runjs,
    useShinyjs,
    disabled,
    toggleState,
    disable,
    enable,
    addClass,
    removeClass
  ],
  DT[DTOutput, renderDT, datatable, dataTableProxy, replaceData],
  fs[path_home],
)
box::use(
  app / logic / functions[render_info],
  app /
    logic /
    pymlst[
      start_typing,
      parse_typing_log,
      existing_strains,
      scheme_size,
    ],
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

# Scheme-completeness (QC) badge: share of the scheme's loci called for a
# strain. Green when near-complete, amber for a mild shortfall, red when a large
# fraction is missing (a sign of a poor assembly or species mismatch).
completeness_badge <- function(pct) {
  if (is.na(pct)) {
    return("—")
  }
  cls <- if (pct >= 99) {
    "text-bg-success"
  } else if (pct >= 90) {
    "text-bg-warning"
  } else {
    "text-bg-danger"
  }
  sprintf('<span class="badge %s">%.1f%%</span>', cls, pct)
}

# Human-readable per-strain analysis duration: seconds -> "3.9s" or "1m 04s".
format_elapsed <- function(secs) {
  if (is.na(secs)) {
    return("—")
  }
  if (secs < 60) {
    return(sprintf("%.1fs", secs))
  }
  sprintf("%dm %02ds", secs %/% 60, round(secs %% 60))
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    useShinyjs(),
    fillable = TRUE,
    sidebar = sidebar(
      title = div(
        class = "typing-sidebar-title",
        div(class = "sidebar-title", "Allelic Typing"),
        tooltip(
          icon("circle-info", class = "text-muted"),
          paste(
            "Select a single assembled genome (.fasta, .fa, .fna) or a folder",
            "of assemblies to type against the loaded scheme."
          ),
          placement = "right"
        )
      ),
      width = 320,
      uiOutput(ns("scheme_info")),
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
        open = TRUE,
        accordion_panel(
          "Parameters",
          icon = icon("sliders"),
          numericInput(
            ns("identity"),
            tooltip(
              span("Min. identity ", icon("circle-info", class = "text-muted")),
              paste(
                "Minimum sequence identity for BLAT to call a locus: the",
                "fraction of identical bases between the assembly and the",
                "reference allele (passed to BLAT as -minIdentity). Raise it for",
                "stricter matches; lower it to recover more divergent alleles."
              )
            ),
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01
          ),
          numericInput(
            ns("coverage"),
            tooltip(
              span("Min. coverage ", icon("circle-info", class = "text-muted")),
              paste(
                "Minimum fraction of a reference locus the alignment must span",
                "to keep a hit (aligned length / locus length). Hits below this",
                "are discarded; partial hits above it are aligned and gap-filled."
              )
            ),
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
        icon = icon("play")
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
    as_fill_item(
      accordion(
        id = ns("typing_accordion"),
        open = "Selected Genomes",
        multiple = FALSE,
        class = "typing-accordion",
        accordion_panel(
          "Selected Genomes",
          icon = icon("dna"),
          DTOutput(ns("selection_table"))
        ),
        accordion_panel(
          "Typing Log",
          icon = icon("terminal"),
          verbatimTextOutput(ns("log"))
        ),
        accordion_panel(
          "Typing Results",
          icon = icon("table"),
          DTOutput(ns("results_table"))
        )
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

    # Typing reactive values
    Typing <- reactiveValues(
      genome_input = NULL,
      files = character(0),
      strains = character(0),
      # Subset of files / strains actually piped into a run: assemblies already
      # present in the database are dropped, so only new genomes are typed.
      queued_files = character(0),
      queued_strains = character(0),
      proc = NULL,
      log_file = NULL,
      status = "idle",
      results = NULL,
      terminated = FALSE,
      refresh = 0L
    )
    log_text <- reactiveVal("")

    # Reset server reactives on session reset
    observeEvent(
      session_reset(),
      {
        if (!is.null(Typing$proc) && Typing$proc$is_alive()) {
          Typing$terminated <- TRUE
          tryCatch(Typing$proc$kill(), error = function(e) NULL)
        }
        Typing$genome_input <- NULL
        Typing$files <- character(0)
        Typing$strains <- character(0)
        Typing$queued_files <- character(0)
        Typing$queued_strains <- character(0)
        Typing$proc <- NULL
        Typing$log_file <- NULL
        Typing$status <- "idle"
        Typing$results <- NULL
        Typing$terminated <- FALSE
        Typing$refresh <- 0L
        log_text("")
        updateProgressBar(session, "progress", value = 0, total = 1)
        runjs(sprintf(
          "var el = document.getElementById('%s'); if (el) el.classList.remove('is-animating');",
          ns("progress")
        ))
        runjs(sprintf(
          "(function(){
           var acc = document.getElementById('%s');
           if (!acc) return;
           var item = acc.querySelector('[data-value=\"Selected Genomes\"]');
           if (!item) return;
           var btn = item.querySelector('.accordion-button.collapsed');
           if (btn) btn.click();
         })();",
          ns("typing_accordion")
        ))
      },
      ignoreInit = TRUE
    )

    # Define roots
    roots <- c(Home = path_home(), Root = "/")

    # Condition helper
    or_default <- function(x, default) {
      if (is.null(x) || length(x) != 1 || is.na(x)) default else x
    }

    # Valid db checker reactive
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

    # Total loci in the loaded scheme (denominator of the completeness metric).
    scheme_total <- reactive(scheme_size(db_path()))

    # Loaded-scheme summary shown to the user (total number of loci).
    output$scheme_info <- renderUI({
      total <- scheme_total()
      if (is.na(total) || total == 0) {
        return(NULL)
      }
      div(
        class = "text-muted small",
        "Scheme size: ",
        tags$strong(format(total, big.mark = ",")),
        " loci"
      )
    })

    # Enable/disable controls based on current status.
    # While running: lock file/folder pickers (needs both disable() + CSS class
    # because shinyFiles buttons are not standard inputs) and parameter inputs.
    # Terminate mirrors the running state; Start requires a valid ready state.
    observe({
      running <- identical(Typing$status, "running")
      ready <- isTRUE(valid_db()) &&
        length(Typing$strains) > 0 &&
        !running
      toggleState("start", condition = ready)
      toggleState("terminate", condition = running)

      if (running) {
        disable("genome_file")
        addClass("genome_file", "custom-disable")
        disable("genome_dir")
        addClass("genome_dir", "custom-disable")
        disable("identity")
        disable("coverage")
      } else {
        enable("genome_file")
        removeClass("genome_file", "custom-disable")
        enable("genome_dir")
        removeClass("genome_dir", "custom-disable")
        enable("identity")
        enable("coverage")
      }
    })

    # Builds the display data frame for the Selected Genomes table. Called by
    # both the initial renderDT and the live replaceData observer so the column
    # structure is always identical.
    build_selection_df <- function(files, strains, results, existing_strains) {
      is_present <- strains %in% existing_strains
      if (!is.null(results) && nrow(results) > 0) {
        status_map <- setNames(results$status, results$strain)
        statuses <- status_map[strains]
        statuses[is.na(statuses)] <- "Pending"
        # Already-present genomes are excluded from the run, so they never
        # appear in `results`; keep their flag rather than showing "Pending".
        statuses[is_present] <- "Already present"
      } else {
        statuses <- ifelse(is_present, "Already present", "New")
      }
      data.frame(
        File = basename(files),
        Status = vapply(statuses, status_badge, character(1)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    # TRUE while at least one file is selected; updated only when the
    # presence of files changes so renderDT is not re-triggered by polling.
    selection_has_files <- reactiveVal(FALSE)
    observe({
      has <- length(Typing$files) > 0
      if (!identical(has, isolate(selection_has_files()))) {
        selection_has_files(has)
      }
    })

    # Structural render only — fires when files appear or disappear.
    # All data reads are wrapped in isolate() so 700 ms polling ticks don't
    # cause a full re-render; live status updates are pushed via replaceData.
    output$selection_table <- renderDT({
      render_info("output$selection_table")

      if (!selection_has_files()) {
        return(datatable(
          data.frame(" " = "No genomes selected yet.", check.names = FALSE),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      datatable(
        isolate(
          build_selection_df(
            Typing$files,
            Typing$strains,
            Typing$results,
            existing()
          )
        ),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      )
    })

    selection_proxy <- dataTableProxy("selection_table", session = session)

    # Push status updates in-place so the accordion body's scroll position
    # is preserved across polling ticks and post-run refreshes.
    observe({
      req(selection_has_files())
      replaceData(
        selection_proxy,
        build_selection_df(
          Typing$files,
          Typing$strains,
          Typing$results,
          existing()
        ),
        resetPaging = FALSE,
        rownames = FALSE
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

    # Builds the display data frame for the results table. Kept as a local
    # function so both the initial renderDT and the live replaceData observer
    # produce identical column structure.
    build_results_df <- function(results, total) {
      completeness <- if (!is.na(total) && total > 0) {
        round(results$found / total * 100, 1)
      } else {
        rep(NA_real_, nrow(results))
      }
      completeness[results$status != "Added"] <- NA_real_

      dash <- function(x) ifelse(is.na(x), "—", as.character(x))

      data.frame(
        Strain = results$strain,
        Status = vapply(results$status, status_badge, character(1)),
        `Loci found` = dash(results$found),
        `Alleles added` = dash(results$added),
        Partial = dash(results$partial),
        Filled = dash(results$filled),
        Removed = dash(results$removed),
        Completeness = vapply(completeness, completeness_badge, character(1)),
        Finished = dash(results$finished),
        Elapsed = vapply(results$elapsed, format_elapsed, character(1)),
        Detail = results$detail,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    # TRUE while results exist (typing has started or has run); updated only
    # when results transition NULL → non-NULL or back, so renderDT below is
    # not re-triggered by the 700 ms polling updates.
    results_initialized <- reactiveVal(FALSE)
    observe({
      has <- !is.null(Typing$results) && nrow(Typing$results) > 0
      if (!identical(has, isolate(results_initialized()))) {
        results_initialized(has)
      }
    })

    # Per-strain results — structural render only.
    # Fires exactly twice per typing run: once when results are first seeded
    # (results_initialized flips TRUE) to build the correct column layout, and
    # once on reset (flips FALSE) to show the placeholder. Live data is pushed
    # via replaceData below, which preserves scroll position.
    output$results_table <- renderDT({
      render_info("output$results_table")

      if (!results_initialized()) {
        return(datatable(
          data.frame(" " = "No results yet.", check.names = FALSE),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      datatable(
        isolate(build_results_df(Typing$results, scheme_total())),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE
        )
      )
    })

    results_proxy <- dataTableProxy("results_table", session = session)

    # Push live data updates without a full re-render so scroll position is
    # preserved across polling ticks.
    observe({
      results <- Typing$results
      req(!is.null(results), nrow(results) > 0)
      replaceData(
        results_proxy,
        build_results_df(results, scheme_total()),
        resetPaging = FALSE,
        rownames = FALSE
      )
    })

    # Keep selection_table and results_table reactive while their panel is absent
    # from the DOM (after nav_remove on reset). Without this, outputs suspend when
    # the panel is removed and still hold the stale pre-reset cached value; when
    # the panel is re-inserted Shiny re-sends that stale cache first, causing a
    # one-frame flicker before the reset state arrives.
    outputOptions(output, "selection_table", suspendWhenHidden = FALSE)
    outputOptions(output, "results_table", suspendWhenHidden = FALSE)
    outputOptions(output, "log", suspendWhenHidden = FALSE)
    outputOptions(output, "status_badge", suspendWhenHidden = FALSE)

    # Start typing
    observeEvent(input$start, {
      req(valid_db(), length(Typing$strains) > 0)
      if (identical(Typing$status, "running")) {
        return()
      }

      # Type only assemblies that are not already in the database; their
      # `wgMLST add` would otherwise be rejected as a duplicate, wasting time.
      keep <- !(Typing$strains %in% existing())
      Typing$queued_files <- Typing$files[keep]
      Typing$queued_strains <- Typing$strains[keep]

      if (length(Typing$queued_files) == 0) {
        showNotification(
          "All selected genomes are already present in the database.",
          type = "warning",
          duration = 5
        )
        return()
      }

      # Click the "Typing Results" accordion button
      runjs(sprintf(
        "(function(){
           var acc = document.getElementById('%s');
           if (!acc) return;
           var item = acc.querySelector('[data-value=\"Typing Results\"]');
           if (!item) return;
           var btn = item.querySelector('.accordion-button.collapsed');
           if (btn) btn.click();
         })();",
        ns("typing_accordion")
      ))

      Typing$log_file <- tempfile(fileext = ".log")
      file.create(Typing$log_file)
      log_text("")
      Typing$terminated <- FALSE
      # Seed an all-Pending table so the queue is visible immediately.
      Typing$results <- parse_typing_log(character(0), Typing$queued_strains)

      proc <- tryCatch(
        start_typing(
          db_path = db_path(),
          genome_files = Typing$queued_files,
          log_file = Typing$log_file,
          identity = or_default(input$identity, 0.95),
          coverage = or_default(input$coverage, 0.9),
          env = "pymlst"
        ),
        error = function(e) e
      )

      if (inherits(proc, "error")) {
        Typing$status <- "failed"
        showNotification(
          paste("Could not start typing:", conditionMessage(proc)),
          type = "error",
          duration = 6
        )
        return()
      }

      Typing$proc <- proc
      Typing$status <- "running"
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.add('is-animating');",
        ns("progress")
      ))
      updateProgressBar(
        session,
        "progress",
        value = 0,
        total = length(Typing$queued_strains)
      )
      skipped <- length(Typing$strains) - length(Typing$queued_strains)
      showNotification(
        paste0(
          length(Typing$queued_strains),
          " genome(s) queued.",
          if (skipped > 0) {
            sprintf(" %d already present, skipped.", skipped)
          }
        ),
        type = "message",
        duration = 3
      )
    })

    # Terminate a running batch
    observeEvent(input$terminate, {
      req(identical(Typing$status, "running"), !is.null(Typing$proc))
      Typing$terminated <- TRUE
      tryCatch(Typing$proc$kill(), error = function(e) NULL)
    })

    # Expose the current typing status for other modules to react to.
    typing_status <- reactive(Typing$status)

    # Incremented whenever a typing run (completed or terminated) has written at
    # least one new strain to the database
    db_updated <- reactiveVal(0L)

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

      results <- parse_typing_log(lines, Typing$queued_strains)
      Typing$results <- results
      done <- sum(
        results$status %in% c("Added", "Duplicate", "Incompatible", "Error")
      )
      updateProgressBar(
        session,
        "progress",
        value = done,
        total = max(1L, length(Typing$queued_strains))
      )

      if (alive) {
        invalidateLater(700, session)
        return(NULL)
      }

      # Finished (completed or killed)
      Typing$status <- if (isTRUE(Typing$terminated)) "terminated" else "done"
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.remove('is-animating');",
        ns("progress")
      ))
      Typing$refresh <- Typing$refresh + 1L

      added <- sum(results$status == "Added")
      duplicate <- sum(results$status == "Duplicate")
      failed <- sum(results$status %in% c("Incompatible", "Error"))

      # Signal other modules that the DB has new data.
      if (added > 0L) {
        db_updated(db_updated() + 1L)
      }

      showNotification(
        HTML(paste(
          if (identical(Typing$status, "terminated")) {
            "Typing terminated"
          } else {
            "Typing complete"
          },
          sprintf(
            "%d added, %d duplicate, %d failed.",
            added,
            duplicate,
            failed
          ),
          collapse = "<br>"
        )),
        type = if (failed > 0) "warning" else "message",
        duration = 5
      )
    })

    list(typing_status = typing_status, db_updated = db_updated)
  })
}
