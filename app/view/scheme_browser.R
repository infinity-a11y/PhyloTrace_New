# app/view/scheme_browser.R

box::use(
  shiny[
    NS,
    reactive,
    moduleServer,
    renderUI,
    uiOutput,
    req,
    div,
    p,
    span,
    em,
    a,
    imageOutput,
    renderImage,
    actionButton,
    icon
  ],
  bslib[
    navset_card_tab,
    page_fillable,
    nav_panel,
    sidebar,
    bs_theme,
    layout_columns,
    card,
    card_header,
    card_body,
    card_title,
    as_fill_item,
    as_fillable_container,
    as_fill_carrier
  ],
  shinyWidgets[pickerInput],
  DT[DTOutput, renderDT, datatable],
  waiter[autoWaiter, spin_3],
)

box::use(
  app / logic / schemes[cgmlst_org_schemes],
  app /
    logic /
    scheme_browser[get_scheme_overview, get_species_img, get_species_details]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    autoWaiter(
      id = ns("scheme_table"),
      html = div(
        class = "scheme-waiter",
        spin_3(),
        p("Fetching metadata", class = "scheme-waiter_text")
      ),
      color = "black"
    ),
    as_fill_carrier(
      navset_card_tab(
        full_screen = FALSE,
        title = NULL,
        nav_panel(
          "Scheme Download",
          div(
            id = "scheme-download-selection",
            uiOutput(ns("scheme_selection")),
            actionButton(
              ns("scheme_download"),
              "Download Scheme",
              icon = icon("download"),
            )
          ),
          layout_columns(
            card(
              full_screen = TRUE,
              card_header(
                class = "bg-dark",
                "Scheme Metadata"
              ),
              card_body(as_fill_item(uiOutput(ns("scheme_overview"))))
            ),
            as_fillable_container(
              as_fill_item(
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "bg-dark",
                    "Details"
                  ),
                  card_body(
                    fillable = FALSE,
                    # Newspaper-style flow: image floats, text wraps around it
                    div(
                      class = "species-card_article",
                      div(
                        class = "species-card_media",
                        imageOutput(ns("species_img"), height = "auto")
                      ),
                      uiOutput(ns("species_details")),
                      uiOutput(ns("species_summary"))
                    )
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "Custom Scheme",
          "Coming soon ..."
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Sidebar UI elements

    # Render scheme selector
    output$scheme_selection <- renderUI(
      pickerInput(
        ns("scheme_selector"),
        NULL,
        choices = gsub("_", " ", cgmlst_org_schemes$species),
        choicesOpt = list(
          subtext = rep("cgMLST", nrow(cgmlst_org_schemes))
        ),
        options = list(
          `live-search` = TRUE,
          size = 10,
          showSubtext = TRUE
        )
      )
    )

    # Fetch scheme metadata from cgmlst.org
    scheme_overview <- reactive({
      req(input$scheme_selector)

      get_scheme_overview(input$scheme_selector)
    })

    # Render scheme metadata
    output$scheme_overview <- renderUI({
      overview <- scheme_overview()

      if (is.character(overview)) {
        div(overview)
      } else if (is.data.frame(overview)) {
        DTOutput(ns("scheme_table"))
      }
    })

    output$scheme_table <- renderDT({
      req(is.data.frame(scheme_overview()))

      datatable(
        scheme_overview(),
        class = 'stripe row-border order-column',
        colnames = c("", ""),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      )
    })

    output$species_img <- renderImage(
      {
        req(input$scheme_selector)

        list(src = get_species_img(input$scheme_selector))
      },
      deleteFile = FALSE
    )

    # Enriched species metadata (taxonomy + description), looked up once
    species_record <- reactive({
      req(input$scheme_selector)

      get_species_details(input$scheme_selector)
    })

    # Render title + taxonomy (sits left of the image)
    output$species_details <- renderUI({
      details <- species_record()

      if (is.null(details)) {
        return(div(
          class = "species-details_empty",
          "No metadata available for this species."
        ))
      }

      # Taxonomy ladder: one chip per known rank (skips missing ranks)
      ranks <- c("phylum", "class", "order", "family", "genus")
      chips <- lapply(ranks, function(rank) {
        value <- details$lineage[[rank]]
        if (is.null(value)) {
          return(NULL)
        }
        div(
          class = "species-details_chip",
          span(toupper(rank), class = "species-details_chip-rank"),
          span(value, class = "species-details_chip-value")
        )
      })

      ncbi_url <- paste0(
        "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
        details$ncbi_taxid
      )

      div(
        class = "species-details",
        # Title row: scientific name, taxonomic rank, NCBI TaxID
        div(
          class = "species-details_header",
          span(em(input$scheme_selector), class = "species-details_name"),
          span(details$rank, class = "species-details_rank"),
          a(
            href = ncbi_url,
            target = "_blank",
            class = "species-details_taxid",
            paste0("NCBI:txid", details$ncbi_taxid)
          )
        ),
        # Taxonomy ladder
        div(class = "species-details_lineage", chips)
      )
    })

    # Render description in its own full-width row below the title/image
    output$species_summary <- renderUI({
      details <- species_record()
      req(!is.null(details), !is.null(details$summary))

      p(details$summary, class = "species-details_summary")
    })
  })
}
