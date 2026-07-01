# app/view/database_loci_info.R
#
# "Loci Info" interface of the Database menu. Shows the scheme's `targets`
# (loci) table with single-row selection; selecting a locus drives an allele
# selector (with in-database usage frequency) and a colour-coded sequence
# viewer on the right. UI and backend live here so the panel computes its own
# state independently of the other menu entries.

box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    reactiveVal,
    observeEvent,
    req,
    div,
    span,
    icon,
    actionButton,
    downloadButton,
    downloadHandler,
    renderUI,
    uiOutput,
    HTML
  ],
  bslib[
    as_fill_carrier,
    as_fill_item,
    as_fillable_container,
    card,
    card_header,
    card_body
  ],
  DT[DTOutput, renderDT, datatable],
  shinyWidgets[pickerInput, pickerOptions, updatePickerInput],
  shinyjs[runjs],
  jsonlite[toJSON],
  utils[write.csv]
)

box::use(
  app /
    logic /
    database_functions[
      load_loci_info,
      load_locus_alleles,
      load_allele_sequence,
      locus_fasta
    ],
  app / logic / functions[render_info]
)

# Columns of the loci table shown to the user (the internal `.gene` column that
# `load_loci_info` adds is intentionally excluded).
display_cols <- c("Locus", "Gene", "Start", "Length", "Product", "Allele Count")

# Wrap each nucleotide in a span so the viewer can colour it via SCSS.
color_sequence <- function(sequence) {
  sequence <- gsub("A", "<span class='base-a'>A</span>", sequence)
  sequence <- gsub("T", "<span class='base-t'>T</span>", sequence)
  sequence <- gsub("G", "<span class='base-g'>G</span>", sequence)
  sequence <- gsub("C", "<span class='base-c'>C</span>", sequence)
  sequence
}

# Push text to the client-side clipboard. toJSON turns the value into a safely
# escaped JS string literal.
copy_to_clipboard <- function(text) {
  runjs(sprintf(
    "navigator.clipboard.writeText(%s);",
    toJSON(text, auto_unbox = TRUE)
  ))
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    # Flex row: the loci table fills the available width, the allele controls
    # stay at a fixed width on the right.
    div(
      class = "loci-info-layout",
      as_fill_carrier(
        div(
          class = "loci-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header(
                class = "loci-info-header",
                "Loci",
                downloadButton(
                  ns("export_csv"),
                  "Export CSV",
                  icon = icon("file-csv"),
                  class = "btn-sm loci-export-btn"
                )
              ),
              # Plain (non-fillable) body carrying the DataTables fill/scroll
              # rules, mirroring the Browse Entries table so the table fills the
              # card and scrolls internally instead of paginating.
              card_body(
                class = "loci-table-body",
                fillable = FALSE,
                DTOutput(ns("db_loci"), fill = TRUE)
              )
            )
          )
        )
      ),
      # Fillable column so the sequence card grows into the remaining height
      # while the selector card keeps its natural size.
      as_fillable_container(
        div(
          class = "loci-controls",
          card(
            fill = FALSE,
            card_header("Select Allele"),
            card_body(
              # Static picker whose choices are refreshed in place via
              # updatePickerInput on row selection. Rebuilding the whole widget
              # with renderUI left a stale, open dropdown out of sync with its
              # label (and fought the body-rendered menu below).
              pickerInput(
                ns("allele_select"),
                label = NULL,
                choices = character(0),
                options = pickerOptions(
                  liveSearch = TRUE,
                  size = 10,
                  liveSearchPlaceholder = "Search alleles ...",
                  # Render the menu in <body> so it is not clipped by the
                  # card's overflow when it extends past the card's edges.
                  container = "body"
                )
              )
            )
          ),
          card(
            fill = TRUE,
            full_screen = TRUE,
            # Card header hosts the allele title and its control buttons (the
            # bslib idiom for card-level actions).
            card_header(
              class = "loci-allele-header",
              uiOutput(ns("allele_title")),
              div(
                class = "loci-allele-actions",
                actionButton(
                  ns("copy_seq"),
                  "Sequence",
                  icon = icon("copy"),
                  class = "btn-sm"
                ),
                actionButton(
                  ns("copy_idx"),
                  "Index",
                  icon = icon("hashtag"),
                  class = "btn-sm"
                ),
                downloadButton(
                  ns("download_locus"),
                  "Locus",
                  icon = icon("download"),
                  class = "btn-sm"
                )
              )
            ),
            # FASTA record rendered straight into the (scrolling) card body; the
            # `sequence` class supplies the monospace/pre-wrap styling that used
            # to live on a <pre>. `fillable = FALSE` keeps the body a normal
            # block (it still fills the card height) so the sequence text flows
            # and wraps inline instead of being stacked by the flex layout.
            card_body(
              class = "sequence",
              fillable = FALSE,
              uiOutput(ns("allele_sequence"))
            )
          )
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

    # Sequence of the currently displayed allele, cached so the "Sequence" copy
    # button does not have to re-query the database.
    seq_cache <- reactiveVal(NULL)

    # Reset module state when the user returns to the startup screen.
    observeEvent(session_reset(), seq_cache(NULL), ignoreInit = TRUE)

    # Loci table (targets enriched with per-locus allele counts).
    loci_info <- reactive({
      req(db_path())
      load_loci_info(db_path())
    })

    output$db_loci <- renderDT({
      li <- loci_info()

      if (is.null(li)) {
        return(datatable(
          data.frame(
            " " = paste(
              "No 'targets' table found. Re-download the scheme to",
              "populate loci info."
            ),
            check.names = FALSE
          ),
          rownames = FALSE,
          colnames = "",
          selection = "none",
          options = list(dom = "t", ordering = FALSE, paging = FALSE)
        ))
      }

      render_info("output$db_loci")

      # Same setup as the Browse Entries table: per-column search row
      # (filter = "top"), no pagination/length menu/global search (dom = "ti"),
      # and the scrollY = "1px" + scrollCollapse trick so the body scrolls and
      # fills the card (see the .loci-table-body CSS). Single-row selection is
      # kept because it drives the allele panel.
      datatable(
        li[, display_cols, drop = FALSE],
        rownames = FALSE,
        filter = "top",
        selection = list(mode = "single", selected = 1),
        class = "stripe row-border order-column",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
      )
    })

    # The selected row's `mlst` gene name (used to key the detail queries) and
    # its display Locus (used for the FASTA download name).
    selected_row <- reactive({
      li <- loci_info()
      req(li)
      row <- input$db_loci_rows_selected
      req(length(row) == 1)
      li[row, , drop = FALSE]
    })

    # Distinct alleles of the selected locus with in-database usage.
    alleles <- reactive({
      req(db_path())
      load_locus_alleles(db_path(), selected_row()$.gene)
    })

    # Refresh the allele dropdown in place whenever the selected locus changes.
    # Present alleles are listed first (with usage stats), then alleles stored
    # but not carried by any isolate.
    observeEvent(alleles(), {
      df <- alleles()
      req(nrow(df) > 0)

      total <- sum(df$count)
      labels <- ifelse(
        df$present,
        sprintf(
          "Allele %s - %d times in DB (%.1f%%)",
          df$seqid,
          df$count,
          100 * df$count / total
        ),
        sprintf("Allele %s - not present", df$seqid)
      )

      updatePickerInput(
        session,
        "allele_select",
        choices = stats::setNames(as.character(df$seqid), labels),
        selected = as.character(df$seqid[1])
      )
    })

    output$allele_title <- renderUI({
      req(input$allele_select)
      span(paste("Allele", input$allele_select))
    })

    # Colour-coded allele in FASTA form: a ">index" header line followed by the
    # sequence. The card body carries `white-space: pre-wrap` so the newline
    # renders without a <pre> wrapper.
    output$allele_sequence <- renderUI({
      req(db_path(), input$allele_select)

      sequence <- load_allele_sequence(
        db_path(),
        as.integer(input$allele_select)
      )
      req(sequence)
      seq_cache(sequence)

      render_info("output$allele_sequence")

      HTML(paste0(
        "<span class=\"fasta-header\">&gt;",
        input$allele_select,
        "</span>\n",
        color_sequence(sequence)
      ))
    })

    # Copy actions -----------------------------------------------------------
    observeEvent(input$copy_seq, {
      req(seq_cache())
      copy_to_clipboard(seq_cache())
    })

    observeEvent(input$copy_idx, {
      req(input$allele_select)
      copy_to_clipboard(input$allele_select)
    })

    # Downloads --------------------------------------------------------------
    output$download_locus <- downloadHandler(
      filename = function() paste0(selected_row()$Locus, ".fasta"),
      content = function(file) {
        writeLines(locus_fasta(db_path(), selected_row()$.gene), file)
      }
    )

    output$export_csv <- downloadHandler(
      filename = function() "loci_info.csv",
      content = function(file) {
        li <- loci_info()
        req(li)
        write.csv(li[, display_cols, drop = FALSE], file, row.names = FALSE)
      }
    )
  })
}
