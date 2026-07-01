# app/view/visualization_mst.R
#
# Minimum Spanning Tree (visNetwork) visualization submodule. Owns its own
# control panel, the visNetwork render, and all MST-specific reactive state.
# Mounted by app/view/visualization.R inside a navset_hidden panel; the shared
# Generate button, plot type, na_handling and per-isolate metadata are forwarded
# in as reactives.

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    outputOptions,
    renderUI,
    uiOutput,
    reactive,
    reactiveVal,
    req,
    updateSelectInput,
    downloadHandler,
    downloadButton,
    div,
    p,
    tags,
    icon,
    sliderInput,
    selectInput,
    numericInput,
  ],
  bslib[
    sidebar,
    layout_sidebar,
    card,
    card_body,
    accordion,
    accordion_panel,
    navset_tab,
    nav_panel,
    input_switch,
    as_fill_carrier,
  ],
  visNetwork[visNetworkOutput, renderVisNetwork],
)
box::use(
  app / logic / functions[render_info],
  app / logic / phylo[compute_mst, build_mst_visnetwork, save_mst_html],
  app / logic / viz_helpers[meta_vars, viz_color, export_panel],
)

# --- MST control tabs --------------------------------------------------------

mst_controls <- function(ns) {
  navset_tab(
    # Labels -----------------------------------------------------------------
    nav_panel(
      "Labels",
      icon = icon("tag"),
      input_switch(ns("mst_show_label"), "Show node labels", TRUE),
      selectInput(
        ns("mst_node_label"),
        "Label source",
        c("Assembly Name", "Isolation Date", "Host", "Country", "City")
      )
    ),
    # Variable mapping -------------------------------------------------------
    nav_panel(
      "Mapping",
      icon = icon("palette"),
      input_switch(ns("mst_color_var"), "Map variable to node colour", FALSE),
      selectInput(ns("mst_col_var"), "Variable", meta_vars),
      selectInput(ns("mst_col_scale"), "Colour scale", c("Viridis", "Rainbow"))
    ),
    # Colours ----------------------------------------------------------------
    nav_panel(
      "Colours",
      icon = icon("fill-drip"),
      div(
        class = "viz-color-grid",
        viz_color(ns, "mst_text_color", "Text", "#000000"),
        viz_color(ns, "mst_color_node", "Nodes", "#B2FACA"),
        viz_color(ns, "mst_color_edge", "Edges", "#000000"),
        viz_color(ns, "mst_edge_font_color", "Edge Font", "#000000"),
        viz_color(ns, "mst_background_color", "Background", "#ffffff"),
        input_switch(
          ns("mst_background_transparent"),
          "Transparent background",
          FALSE
        )
      )
    ),
    # Sizing -----------------------------------------------------------------
    nav_panel(
      "Sizing",
      icon = icon("up-down"),
      accordion(
        open = "Nodes",
        accordion_panel(
          "Nodes",
          icon = icon("circle"),
          input_switch(ns("mst_scale_nodes"), "Scale by duplicates", TRUE),
          sliderInput(ns("mst_node_size"), "Size", 1, 100, 30, ticks = FALSE)
        ),
        accordion_panel(
          "Edges",
          icon = icon("grip-lines"),
          input_switch(ns("mst_scale_edges"), "Scale allelic distance", TRUE),
          sliderInput(
            ns("mst_edge_length_scale"),
            "Multiplier",
            1,
            40,
            15,
            ticks = FALSE
          ),
          sliderInput(
            ns("mst_edge_font_size"),
            "Font size",
            8,
            30,
            18,
            ticks = FALSE
          )
        ),
        accordion_panel(
          "Labels",
          icon = icon("font"),
          sliderInput(
            ns("mst_node_label_fontsize"),
            "Font size",
            8,
            30,
            14,
            ticks = FALSE
          )
        )
      )
    ),
    # Layout -----------------------------------------------------------------
    nav_panel(
      "Layout",
      icon = icon("sliders"),
      accordion(
        open = "Dimensions",
        accordion_panel(
          "Dimensions",
          icon = icon("up-right-and-down-left-from-center"),
          sliderInput(
            ns("mst_aspect_ratio"),
            "Aspect ratio",
            0.5,
            2,
            0.6,
            step = 0.1,
            ticks = FALSE
          )
        ),
        accordion_panel(
          "Node Shapes",
          icon = icon("shapes"),
          input_switch(ns("mst_shadow"), "Show shadow", TRUE),
          selectInput(
            ns("mst_node_shape"),
            "Shape",
            list(
              `Label inside` = c(Circle = "circle", Box = "box", Text = "text"),
              `Label outside` = c(
                Diamond = "diamond",
                Hexagon = "hexagon",
                Dot = "dot",
                Square = "square"
              )
            ),
            selected = "dot"
          )
        ),
        accordion_panel(
          "Clustering",
          icon = icon("circle-nodes"),
          input_switch(ns("mst_show_clusters"), "Show clusters", FALSE),
          numericInput(
            ns("mst_cluster_threshold"),
            "Threshold",
            value = 10,
            min = 1,
            max = 99
          ),
          selectInput(
            ns("mst_cluster_col_scale"),
            "Colour scale",
            c("Viridis", "Rainbow")
          ),
          selectInput(ns("mst_cluster_type"), "Type", c("Area", "Skeleton")),
          sliderInput(
            ns("mst_cluster_width"),
            "Skeleton width",
            1,
            50,
            24,
            ticks = FALSE
          )
        ),
        accordion_panel(
          "Legend",
          icon = icon("list"),
          selectInput(
            ns("mst_legend_ori"),
            "Orientation",
            c(Left = "left", Right = "right")
          ),
          sliderInput(
            ns("mst_font_size"),
            "Font size",
            15,
            30,
            18,
            ticks = FALSE
          ),
          sliderInput(
            ns("mst_symbol_size"),
            "Key size",
            10,
            30,
            20,
            ticks = FALSE
          )
        )
      )
    ),
    export_panel(ns, "mst", c("html", "png", "jpeg", "bmp"))
  )
}

#' @export
ui <- function(id, generate_id) {
  ns <- NS(id)

  layout_sidebar(
    id = "plot-sidebar",
    border = FALSE,
    sidebar = sidebar(
      id = ns("controls_sidebar"),
      class = "viz-controls-sidebar",
      position = "right",
      width = 380,
      open = TRUE,
      fillable = TRUE,
      as_fill_carrier(div(class = "viz-nav-wrap", mst_controls(ns)))
    ),
    shinyjs::useShinyjs(),
    # Loads waiter.js so the flower spinner used in the loading overlay is styled.
    waiter::useWaiter(),
    # Loading overlay: shown the moment Generate (parent namespace) is clicked,
    # scoped to this engine's own stage id. Unlike the Tree, the visNetwork keeps
    # running a client-side physics layout after its value arrives, so it is
    # cleared by the network's stabilization event (see build_mst_visnetwork,
    # which removes `.is-loading`). The timeout here is a safety net.
    tags$script(
      shiny::HTML(
        paste0(
          "(function(){",
          "var gen='",
          generate_id,
          "';var out='",
          ns("mst_plot"),
          "';var stage='",
          ns("plot_stage"),
          "';var timer;",
          "function set(on){var s=document.getElementById(stage);if(!s)return;",
          # Ignore Generate clicks while this engine's panel is hidden (the
          # sibling engine is active) — offsetParent is null when display:none.
          "if(on&&s.offsetParent===null)return;",
          "s.classList.toggle('is-loading',on);",
          "if(on){var p=s.querySelector('.viz-plot-prompt');if(p)p.style.display='none';",
          "clearTimeout(timer);timer=setTimeout(function(){set(false);},45000);}",
          "else{clearTimeout(timer);}}",
          "$(document).on('click','#'+gen.replace(/([:.])/g,'\\\\$1'),",
          "function(){set(true);});",
          "$(document).on('shiny:error',",
          "function(e){if(e.target.id===out)set(false);});",
          "})();"
        )
      )
    ),
    card(
      full_screen = TRUE,
      class = "plot-card",
      card_body(uiOutput(ns("plot_area")))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  viz_metadata = shiny::reactive(NULL),
  na_handling = shiny::reactive("ignore_na"),
  generate = shiny::reactive(0L),
  plot_type = shiny::reactive("MST")
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Whether a plot has been generated for this engine. Retained across
    # plot-type switches (only session reset clears it).
    generated <- reactiveVal(FALSE)

    # The computed MST graph. Held in a reactiveVal (not an eventReactive) so a
    # Generate for the *other* engine — which also ticks the shared `generate()`
    # — leaves this engine's last result untouched. It is (re)computed only by
    # the guarded Generate observer below when MST is the active engine.
    mst_obj <- reactiveVal(NULL)

    # Resolved MST control values, gathered once so the live render and the
    # HTML export share an identical configuration.
    mst_opts <- reactive(
      list(
        show_label = input$mst_show_label,
        field = input$mst_node_label,
        node_font_color = input$mst_text_color,
        node_font_size = input$mst_node_label_fontsize,
        node_color = input$mst_color_node,
        node_size = input$mst_node_size,
        scale_nodes = input$mst_scale_nodes,
        shape = input$mst_node_shape,
        shadow = input$mst_shadow,
        edge_color = input$mst_color_edge,
        edge_font_color = input$mst_edge_font_color,
        edge_font_size = input$mst_edge_font_size,
        scale_edges = input$mst_scale_edges,
        edge_length_scale = input$mst_edge_length_scale,
        background = input$mst_background_color,
        transparent = input$mst_background_transparent,
        # Variable pie-chart colouring + legend.
        color_var = input$mst_color_var,
        col_var = input$mst_col_var,
        col_scale = input$mst_col_scale,
        legend_ori = input$mst_legend_ori,
        legend_font_size = input$mst_font_size,
        legend_symbol_size = input$mst_symbol_size,
        # Clustering.
        show_clusters = input$mst_show_clusters,
        cluster_threshold = input$mst_cluster_threshold,
        cluster_col_scale = input$mst_cluster_col_scale,
        cluster_type = input$mst_cluster_type,
        cluster_width = input$mst_cluster_width
      )
    )

    # The visNetwork widget: rebuilt live as controls change, but never
    # recomputes the (expensive) MST itself.
    mst_widget <- reactive({
      req(mst_obj())
      build_mst_visnetwork(mst_obj(), viz_metadata(), mst_opts())
    })

    observeEvent(
      session_reset(),
      {
        generated(FALSE)
      },
      ignoreInit = TRUE
    )

    observeEvent(generate(), {
      if (!identical(plot_type(), "MST")) {
        return()
      }

      # Populate the metadata-backed selects (no heavy compute here; the MST is
      # computed lazily by its output so the waiter can cover it).
      fields <- names(viz_metadata())

      updateSelectInput(
        session,
        "mst_node_label",
        choices = fields,
        selected = if (isTRUE(input$mst_node_label %in% fields)) {
          input$mst_node_label
        } else {
          "isolate"
        }
      )
      # Default the colour variable to the first non-isolate field, where one
      # exists, so a freshly enabled mapping is meaningful.
      non_isolate <- setdiff(fields, "isolate")
      updateSelectInput(
        session,
        "mst_col_var",
        choices = fields,
        selected = if (isTRUE(input$mst_col_var %in% fields)) {
          input$mst_col_var
        } else if (length(non_isolate)) {
          non_isolate[1]
        } else {
          fields[1]
        }
      )

      # Compute the MST (heavy work is covered by the client-side loading
      # overlay, which the visNetwork stabilization event clears).
      graph <- tryCatch(
        compute_mst(db_path(), na_handling()),
        error = function(e) {
          shiny::showNotification(
            paste("MST computation failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(graph)) {
        shiny::showNotification(
          "Could not build an MST: need at least 2 isolates in the database.",
          type = "warning"
        )
      }
      mst_obj(graph)

      generated(TRUE)
    })

    # The plot output element is kept mounted so that each Generate re-renders
    # the *same* output. The "press Generate" prompt is an overlay toggled
    # separately.
    output$plot_area <- renderUI({
      render_info("visualization_mst plot_area")
      prompt <- div(
        id = ns("viz_prompt"),
        class = "viz-plot-prompt",
        style = if (isTRUE(shiny::isolate(generated()))) {
          "display:none;"
        } else {
          NULL
        },
        icon("circle-nodes", class = "viz-plot-icon"),
        p(
          "Configure the MST options, then press ",
          tags$strong("Generate Plot"),
          "."
        )
      )

      # Loading overlay, shown/hidden client-side via the `.is-loading` class.
      loading <- div(
        class = "viz-loading",
        div(
          class = "spinner-custom",
          waiter::spin_flower(),
          tags$h5("Generating plot …", class = "viz-loading_text")
        )
      )

      # Canvas width derives from the panel height and the aspect-ratio control;
      # the height is only known after a first render, so fall back until the
      # browser reports it.
      aspect <- if (is.null(input$mst_aspect_ratio)) {
        0.6
      } else {
        input$mst_aspect_ratio
      }
      h <- session$clientData[[paste0("output_", ns("mst_plot"), "_height")]]
      width <- if (!is.null(h)) {
        as.integer(h * (1 / aspect))
      } else {
        as.integer(500 * aspect)
      }
      div(
        class = "viz-plot-stage",
        id = ns("plot_stage"),
        prompt,
        loading,
        visNetworkOutput(
          ns("mst_plot"),
          height = "100%",
          width = paste0(width, "px")
        ),
        # Hidden target the export action button clicks to start the download.
        div(
          style = "display:none;",
          downloadButton(ns("mst_html"), "Download HTML")
        )
      )
    })

    # Hide the prompt overlay once a plot has been generated.
    observeEvent(
      generated(),
      {
        shinyjs::toggle(id = "viz_prompt", condition = !isTRUE(generated()))
      },
      ignoreNULL = FALSE
    )

    output$mst_plot <- renderVisNetwork({
      render_info("visualization_mst mst_plot")
      mst_widget()
    })

    # Serialise the current MST as a self-contained HTML file.
    output$mst_html <- downloadHandler(
      filename = function() paste0(Sys.Date(), "_MST.html"),
      content = function(file) {
        bg <- if (isTRUE(input$mst_background_transparent)) {
          "rgba(0,0,0,0)"
        } else {
          input$mst_background_color
        }
        save_mst_html(mst_widget(), file, bg)
      }
    )

    # The export tab uses an action button; route it to the hidden download
    # link. Only HTML export is wired for now.
    observeEvent(input$mst_download, {
      if (identical(input$mst_filetype, "html")) {
        shinyjs::click("mst_html")
      } else {
        shiny::showNotification(
          "Only HTML export is available currently.",
          type = "message"
        )
      }
    })

    # Keep the outputs reactive while hidden: the panel is nav_remove'd on
    # session reset AND the inactive engine's panel is display:none-hidden by
    # navset_hidden.
    outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    outputOptions(output, "mst_plot", suspendWhenHidden = FALSE)
  })
}
