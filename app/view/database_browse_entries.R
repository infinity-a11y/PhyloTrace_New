# app/view/database_browse_entries.R

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    reactive,
    reactiveVal,
    div,
    h2,
    reactiveValues,
    showNotification,
    actionButton,
    icon,
    req,
    uiOutput,
    renderUI,
    modalDialog,
    modalButton,
    showModal,
    removeModal,
    tagList
  ],
  bslib[as_fill_carrier],
  shinyjs[disabled, disable, enable, addClass, removeClass],
  shinyWidgets[pickerInput, pickerOptions],
  DT[
    DTOutput,
    renderDT,
    datatable,
    editData,
    dataTableProxy,
    replaceData,
    showCols,
    hideCols
  ],
)

box::use(
  app /
    logic /
    database_functions[
      make_metadata_table,
      save_metadata_table,
      remove_isolates
    ]
)

# Maps to the column order returned by make_metadata_table()
PRETTY_NAMES <- c(
  "Isolate",
  "Lab Sample ID",
  "Specimen Source",
  "Collection Date",
  "Country",
  "Province / State",
  "Collected By",
  "Submitted By",
  "Organism",
  "Purpose of Sampling",
  "Purpose of Sequencing"
)

# Columns the user can toggle; "Isolate" is always visible (DT col index 0)
OPTIONAL_COLS <- PRETTY_NAMES[-1]

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      div(
        class = "db-page_header help-header",
        div(
          class = "db-browse-controls",
          div(
            class = "control-group",
            div(class = "control-group-label", "Column Selection"),
            div(
              class = "control-group-items",
              pickerInput(
                ns("col_picker"),
                label = NULL,
                choices = OPTIONAL_COLS,
                selected = OPTIONAL_COLS,
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  title = "Show fields …",
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} / 10 fields",
                  liveSearch = TRUE,
                  liveSearchPlaceholder = "Search fields ..."
                )
              )
            )
          ),
          div(
            class = "control-group",
            div(class = "control-group-label", "Edit"),
            div(
              class = "control-group-items",
              disabled(
                actionButton(
                  ns("discard"),
                  "Discard",
                  icon = icon("rotate-left")
                )
              ),
              disabled(
                actionButton(
                  ns("save"),
                  "Save Changes",
                  class = "btn-success",
                  icon = icon("floppy-disk")
                )
              )
            )
          ),
          div(
            class = "control-group",
            div(class = "control-group-label", "Remove isolates"),
            div(
              class = "control-group-items",
              uiOutput(ns("remove_picker_ui")),
              disabled(
                actionButton(
                  ns("remove_btn"),
                  "Remove",
                  class = "btn-danger",
                  icon = icon("trash")
                )
              )
            )
          )
        )
      ),
      as_fill_carrier(
        div(
          class = "db-page_body",
          DTOutput(ns("metadata_table"), fill = TRUE)
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    State <- reactiveValues(data = NULL, pending = FALSE)

    # Incremented only when the user explicitly requests a data reload (via
    # the "Reload Database" notification button, which calls reset_session()).
    # This is the sole mechanism that invalidates metadata_base's cache after
    # the initial load — db_updated() is intentionally NOT a dependency so
    # that background typing never wipes pending client-side edits.
    reload_token <- reactiveVal(0L)

    metadata_base <- reactive({
      reload_token()
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(NULL)
      }
      df <- make_metadata_table(path)
      if (is.null(df) || !nrow(df)) {
        return(NULL)
      }
      df[is.na(df)] <- ""
      df
    })

    observeEvent(metadata_base(), {
      State$data <- metadata_base()
      State$pending <- FALSE
    })

    output$remove_picker_ui <- renderUI({
      df <- metadata_base()
      choices <- if (!is.null(df)) df$isolate else character(0)
      pickerInput(
        ns("remove_picker"),
        label = NULL,
        choices = choices,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Select isolates to remove …",
          selectedTextFormat = "count > 2",
          countSelectedText = "{0} isolates selected",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search isolates ..."
        )
      )
    })

    observeEvent(
      session_reset(),
      {
        State$data <- NULL
        State$pending <- FALSE
        reload_token(reload_token() + 1L)
      },
      ignoreInit = TRUE
    )

    output$metadata_table <- renderDT({
      df <- metadata_base()

      if (is.null(df)) {
        return(datatable(
          data.frame(
            " " = "No entries in this database yet.<br>Add isolates by typing them in the <strong>Allelic Typing</strong> module.",
            check.names = FALSE
          ),
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      datatable(
        df,
        rownames = FALSE,
        colnames = PRETTY_NAMES,
        filter = "top",
        editable = list(target = "cell", disable = list(columns = c(0L, 8L))),
        selection = "none",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(className = "dt-left", targets = "_all"),
            list(className = "col-readonly", targets = c(0L, 8L))
          ),
          initComplete = DT::JS(
            "function(settings) {
              var api = this.api();
              var tableNode = api.table().node();

              $(tableNode).on('keyup', 'input', function(e) {
                if (e.key === 'Enter') this.blur();
              });

              api.on('column-visibility.dt', function() {
                api.columns.adjust().draw(false);
              });

              // Swap the default text input with a native date picker for
              // Collection Date (data column index 3).
              var observer = new MutationObserver(function(mutations) {
                for (var i = 0; i < mutations.length; i++) {
                  var added = mutations[i].addedNodes;
                  for (var j = 0; j < added.length; j++) {
                    var node = added[j];
                    if (node.tagName !== 'INPUT') continue;
                    var idx = api.cell(node.parentNode).index();
                    if (idx && idx.column === 3) {
                      node.type = 'date';
                      var v = (node.value || '').trim();
                      if (v) {
                        var d = new Date(v);
                        if (!isNaN(d.getTime()))
                          node.value = d.toISOString().split('T')[0];
                      }
                      node.focus();
                    }
                  }
                }
              });
              observer.observe(tableNode, { childList: true, subtree: true });
            }"
          )
        )
      )
    })

    proxy <- dataTableProxy("metadata_table", session = session)

    # Apply cell edit to in-memory state and push to table without full re-render
    observeEvent(input$metadata_table_cell_edit, {
      req(is.data.frame(State$data))
      info <- input$metadata_table_cell_edit
      State$data <- editData(State$data, info, rownames = FALSE)
      State$pending <- TRUE
      replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
    })

    # Enable/disable both action buttons together based on pending edits
    observeEvent(State$pending, {
      if (isTRUE(State$pending)) {
        enable("save")
        addClass("save", "btn-attention")
        enable("discard")
      } else {
        disable("save")
        removeClass("save", "btn-attention")
        disable("discard")
      }
    })

    observeEvent(input$save, {
      save_metadata_table(db_path(), State$data)
      State$pending <- FALSE
      showNotification(
        "Database changes saved.",
        type = "message",
        duration = 3
      )
    })

    # Discard: re-fetch from DB and push back to the table without re-render
    observeEvent(input$discard, {
      fresh <- metadata_base()
      State$data <- fresh
      State$pending <- FALSE
      replaceData(proxy, fresh, resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent(
      input$remove_picker,
      {
        if (length(input$remove_picker) > 0) {
          enable("remove_btn")
        } else {
          disable("remove_btn")
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$remove_btn, {
      isolates <- input$remove_picker
      req(length(isolates) > 0)
      showModal(modalDialog(
        title = "Remove isolates",
        if (length(isolates) == 1) {
          paste0(
            'Permanently remove "',
            isolates,
            '" from the database? This cannot be undone.'
          )
        } else {
          paste0(
            "Permanently remove ",
            length(isolates),
            " isolates from the database? This cannot be undone."
          )
        },
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove"), "Remove", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_remove, {
      isolates <- input$remove_picker
      removeModal()
      remove_isolates(db_path(), isolates)
      reload_token(reload_token() + 1L)
      showNotification(
        paste0(length(isolates), " isolate(s) removed from the database."),
        type = "message",
        duration = 3
      )
    })

    # Column visibility: OPTIONAL_COLS[i] lives at DT 0-based column index i
    # (Isolate is at 0; optional columns follow at 1..length(OPTIONAL_COLS))
    observeEvent(
      input$col_picker,
      {
        selected <- input$col_picker
        show_idx <- which(OPTIONAL_COLS %in% selected)
        hide_idx <- which(!OPTIONAL_COLS %in% selected)
        if (length(show_idx)) {
          showCols(proxy, show_idx, reset = FALSE)
        }
        if (length(hide_idx)) hideCols(proxy, hide_idx, reset = FALSE)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    list(pending = reactive(State$pending))
  })
}
