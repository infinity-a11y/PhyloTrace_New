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
    icon,
    reactiveValues,
    observeEvent,
    tagList,
    h5,
    observe,
    textInput
  ],
  shinyjs[disabled, useShinyjs, enable, disable],
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
  shinyWidgets[pickerInput, show_toast],
  DT[DTOutput, renderDT, datatable],
  waiter[autoWaiter, spin_flower, Waiter, transparent],
  fs[path_home],
  shinyFiles[shinyDirButton, shinyDirChoose, parseDirPath]
)

box::use(
  app / logic / schemes[cgmlst_org_schemes],
  app / logic / pymlst[download_cgmlst_scheme],
  app /
    logic /
    scheme_browser[get_scheme_overview, get_species_img, get_species_details]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    useShinyjs(),
    autoWaiter(
      id = ns("scheme_table"),
      html = div(
        class = "scheme-waiter",
        spin_flower(),
        p("Fetching metadata", class = "scheme-waiter_text")
      ),
      color = "black"
    ),
    as_fill_carrier(
      div(
        id = ns("scheme-download-container"),
        navset_card_tab(
          full_screen = FALSE,
          title = NULL,
          nav_panel(
            "Scheme Download",
            div(
              id = "scheme-download-selection",
              uiOutput(ns("scheme_selection")),
              shinyDirButton(
                ns("download_location"),
                "Select Location",
                icon = icon("folder-open"),
                title = "Choose a location for a new database",
                buttonType = "default",
                root = path_home(),
                multiple = FALSE
              ),
              uiOutput(ns("db_name_input")),
              uiOutput(ns("selected_dir")),
              disabled(
                actionButton(
                  ns("scheme_download"),
                  "Download Scheme",
                  icon = icon("download")
                )
              ),
              div(
                id = ns("download_status_ui"),
                uiOutput(ns("download_status"))
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
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    Scheme_Browser <- reactiveValues(download_status = "")

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
          `show-subtext` = TRUE
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

    # Render scheme info table
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

    # Render species img
    output$species_img <- renderImage(
      {
        req(input$scheme_selector)

        list(src = get_species_img(input$scheme_selector))
      },
      deleteFile = FALSE
    )

    # Enriched species metadata (taxonomy + description)
    species_record <- reactive({
      req(input$scheme_selector)

      get_species_details(input$scheme_selector)
    })

    # Render title + taxonomy
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

      div(
        class = "species-details",
        div(
          class = "species-details_header",
          span(em(input$scheme_selector), class = "species-details_name"),
          span(details$rank, class = "species-details_rank"),
          a(
            href = paste0(
              "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
              details$ncbi_taxid
            ),
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

    # Download location directory chooser
    shinyDirChoose(
      input,
      "download_location",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session
    )

    output$db_name_input <- renderUI({
      textInput(
        ns("db_name"),
        "Choose name",
        value = gsub(" ", "_", gsub("/", "_", input$scheme_selector))
      )
    })

    output$selected_dir <- renderUI({
      req(input$db_name)

      download_path <- parseDirPath(
        roots = c(Home = path_home(), Root = "/"),
        input$download_location
      )

      if (
        !length(download_path) ||
          !is.character(download_path)
      ) {
        "No location selected ..."
      } else {
        paste(
          "Target location:",
          file.path(download_path, paste0(input$db_name, ".db"))
        )
      }
    })

    # Observe download button status
    observe({
      message(paste(Sys.time(), TRUE))

      download_path <- parseDirPath(
        roots = c(Home = path_home(), Root = "/"),
        input$download_location
      )

      if (
        !is.null(input$db_name) &&
          length(input$db_name) &&
          length(download_path) &&
          is.character(download_path)
      ) {
        message("ENABLE")
        enable("scheme_download")
      } else {
        message("DISABLE")
        disable("scheme_download")
      }
    }) |>
      shiny::bindEvent(list(input$db_name, input$download_location))

    # Event scheme download
    observeEvent(input$scheme_download, {
      download_path <- parseDirPath(
        roots = c(Home = path_home(), Root = "/"),
        input$download_location
      )

      db_location <- file.path(download_path, paste0(input$db_name, ".db"))

      # If database already exists exit
      if (file.exists(db_location)) {
        show_toast(
          title = NULL,
          text = paste(db_location, "already exists"),
          type = "error",
          timer = 5000,
          timerProgressBar = TRUE
        )
        return()
      }

      Scheme_Browser$download_status <- "Downloading ..."

      waiting_screen <- div(
        class = "spinner-custom",
        spin_flower(),
        div(
          h5("Downloading ..."),
          div(id = "scheme-load", input$scheme_selector)
        )
      )

      # Define spinner
      w <- Waiter$new(
        id = ns("scheme-download-container"),
        html = waiting_screen
      )
      w$show()
      on.exit(w$hide())

      # # Define DB location
      # db_name <- gsub(" ", "_", gsub("/", "_", input$scheme_selector))
      # db_location <- file.path(
      #   tempdir(),
      #   paste0(db_name, ".db")
      # )
      # n <- 1
      # while (file.exists(db_location)) {
      #   db_location <- file.path(
      #     tempdir(),
      #     paste0(input$scheme_selector, n, ".db")
      #   )
      #   n <- n + 1
      # }

      # Run download
      status <- download_cgmlst_scheme(
        input$scheme_selector,
        db_location,
        env_name = "pymlst"
      )

      # Check download process status
      if (status$status == 1 | isFALSE(file.exists(db_location))) {
        # Case download has exit status 1
        download_status <- paste(
          "Download of",
          input$scheme_selector,
          "failed"
        )
      } else if (status$status == 0) {
        # Case download has exit status 0
        download_status <- paste(
          "Download of",
          input$scheme_selector,
          "was successful."
        )
      }

      # Return status
      show_toast(
        title = NULL,
        text = download_status,
        type = ifelse(status$status == 0, "success", "error"),
        timer = 5000,
        timerProgressBar = TRUE
      )
      output$download_status <- renderUI(download_status)
    })
  })
}
