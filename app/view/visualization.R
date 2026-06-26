# app/view/visualization.R

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    renderUI,
    uiOutput,
    reactiveVal,
    div,
    p,
    tags,
    icon,
    hr,
    sliderInput,
    selectInput,
    textInput,
    numericInput,
    actionButton,
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
    navset_card_tab,
    nav_panel,
    input_switch,
    tooltip,
    as_fill_carrier,
  ],
  shinyWidgets[radioGroupButtons, prettyRadioButtons, colorPickr, pickerInput],
)
box::use(
  app / logic / functions[render_info],
)

# --- shared option sets (placeholders; populated from the database in backend) -

# Metadata columns mappable to plot aesthetics.
meta_vars <- c("Isolation Date", "Host", "Country", "City", "Database")
# Sources for the isolate (tip) label.
label_vars <- c("Assembly Name", "Assembly ID", meta_vars)
# Sources for branch labels (allelic distance is the numeric default).
branch_vars <- c("Allelic Distance", meta_vars)

fontfaces <- c(
  Plain = "plain",
  Bold = "bold",
  Italic = "italic",
  `Bold Italic` = "bold.italic"
)
point_shapes <- c(
  Circle = "circle",
  Square = "square",
  Diamond = "diamond",
  Triangle = "triangle",
  Cross = "cross",
  Asterisk = "asterisk"
)
# ColorBrewer / viridis palettes grouped for the colour-scale selects.
color_scales <- list(
  Qualitative = c(
    "Set1",
    "Set2",
    "Set3",
    "Pastel1",
    "Paired",
    "Dark2",
    "Accent"
  ),
  Sequential = c(
    "Blues",
    "Greens",
    "Reds",
    "Purples",
    "Oranges",
    "Greys",
    "YlGnBu"
  ),
  Gradient = c(
    "viridis",
    "magma",
    "plasma",
    "inferno",
    "cividis",
    "turbo",
    "mako"
  ),
  Diverging = c("Spectral", "RdYlGn", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
)

# --- small UI helpers --------------------------------------------------------

# A labelled colour picker laid out as one row (label left, swatch right).
viz_color <- function(ns, id, label, value) {
  div(
    class = "viz-color-row",
    tags$span(label, class = "viz-color-label"),
    div(
      class = "viz-color-pick",
      colorPickr(
        inputId = ns(id),
        label = NULL,
        selected = value,
        update = "changestop",
        interaction = list(clear = FALSE, save = FALSE),
        position = "right-start",
        width = "100%"
      )
    )
  )
}

# Grouped colour-scale select used by every variable mapping.
scale_select <- function(ns, id) {
  selectInput(ns(id), "Colour scale", choices = color_scales, width = "100%")
}

# Export tab, shared by both engines (prefix keeps input ids unique per engine).
export_panel <- function(ns, prefix, formats) {
  nav_panel(
    "Export",
    icon = icon("download"),
    div(
      class = "viz-export",
      selectInput(ns(paste0(prefix, "_filetype")), "File format", formats),
      actionButton(
        ns(paste0(prefix, "_download")),
        "Save plot",
        icon = icon("download")
      ),
      hr(),
      actionButton(
        ns(paste0(prefix, "_report")),
        "Print report",
        icon = icon("file-lines")
      )
    )
  )
}

# Plot preview: the engine's example image once generated, otherwise a prompt.
plot_placeholder <- function(type, generated) {
  img <- if (type == "MST") {
    "static/images/mst_visualization.png"
  } else {
    "static/images/tree_visualization.png"
  }

  if (isTRUE(generated)) {
    div(
      class = "viz-plot-stage",
      tags$img(src = img, class = "viz-plot-img", alt = paste(type, "plot"))
    )
  } else {
    div(
      class = "viz-plot-empty",
      icon(
        if (type == "MST") "circle-nodes" else "diagram-project",
        class = "viz-plot-icon"
      ),
      p(
        "Configure the ",
        type,
        " options, then press ",
        tags$strong("Generate Plot"),
        "."
      )
    )
  }
}

# --- Tree (NJ / UPGMA) control tabs ------------------------------------------

tree_controls <- function(ns) {
  navset_card_tab(
    # Labels -----------------------------------------------------------------
    nav_panel(
      "Labels",
      icon = icon("tag"),
      accordion(
        open = "Isolate Labels",
        accordion_panel(
          "Isolate Labels",
          icon = icon("tag"),
          input_switch(ns("nj_tiplab_show"), "Show isolate labels", TRUE),
          selectInput(ns("nj_tiplab"), "Label source", label_vars),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(
              ns("nj_tiplab_size"),
              "Size",
              1,
              10,
              4,
              step = 0.1,
              ticks = FALSE
            ),
            sliderInput(
              ns("nj_tiplab_alpha"),
              "Opacity",
              0.1,
              1,
              1,
              step = 0.05,
              ticks = FALSE
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("nj_tiplab_fontface"), "Font", fontfaces),
            sliderInput(
              ns("nj_tiplab_angle"),
              "Angle",
              -90,
              90,
              0,
              ticks = FALSE
            )
          ),
          input_switch(ns("nj_align"), "Align labels", TRUE)
        ),
        accordion_panel(
          "Branch Labels",
          icon = icon("code-branch"),
          input_switch(ns("nj_show_branch_label"), "Show branch labels", FALSE),
          selectInput(ns("nj_branch_label"), "Label source", branch_vars),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(
              ns("nj_branch_size"),
              "Size",
              2,
              10,
              4,
              step = 0.5,
              ticks = FALSE
            ),
            sliderInput(
              ns("nj_branchlabel_cutoff"),
              "Cutoff",
              0,
              100,
              10,
              ticks = FALSE
            )
          )
        ),
        accordion_panel(
          "Title & Subtitle",
          icon = icon("heading"),
          textInput(ns("nj_title"), "Title", placeholder = "Plot title"),
          sliderInput(
            ns("nj_title_size"),
            "Title size",
            15,
            40,
            30,
            ticks = FALSE
          ),
          textInput(
            ns("nj_subtitle"),
            "Subtitle",
            placeholder = "Plot subtitle"
          ),
          sliderInput(
            ns("nj_subtitle_size"),
            "Subtitle size",
            15,
            40,
            30,
            ticks = FALSE
          )
        )
      )
    ),
    # Variable mapping -------------------------------------------------------
    nav_panel(
      "Mapping",
      icon = icon("palette"),
      accordion(
        open = "Tip Label Colour",
        accordion_panel(
          "Tip Label Colour",
          icon = icon("font"),
          input_switch(ns("nj_mapping_show"), "Map variable to colour", FALSE),
          selectInput(ns("nj_color_mapping"), "Variable", meta_vars),
          scale_select(ns, "nj_tiplab_scale")
        ),
        accordion_panel(
          "Tip Point Colour",
          icon = icon("circle"),
          input_switch(
            ns("nj_tipcolor_mapping_show"),
            "Map variable to colour",
            FALSE
          ),
          selectInput(ns("nj_tipcolor_mapping"), "Variable", meta_vars),
          scale_select(ns, "nj_tippoint_scale")
        ),
        accordion_panel(
          "Tip Point Shape",
          icon = icon("shapes"),
          input_switch(
            ns("nj_tipshape_mapping_show"),
            "Map variable to shape",
            FALSE
          ),
          selectInput(ns("nj_tipshape_mapping"), "Variable", meta_vars)
        ),
        accordion_panel(
          "Tiles",
          icon = icon("table-cells"),
          selectInput(ns("nj_tile_num"), "Tile", as.character(1:5)),
          input_switch(ns("nj_tiles_show"), "Show tile", FALSE),
          selectInput(ns("nj_fruit_variable"), "Variable", meta_vars),
          scale_select(ns, "nj_tiles_scale")
        ),
        accordion_panel(
          "Heatmap",
          icon = icon("border-all"),
          input_switch(ns("nj_heatmap_show"), "Show heatmap", FALSE),
          actionButton(
            ns("nj_heatmap_button"),
            "Select variables",
            icon = icon("list-check")
          ),
          scale_select(ns, "nj_heatmap_scale")
        )
      )
    ),
    # Colours ----------------------------------------------------------------
    nav_panel(
      "Colours",
      icon = icon("fill-drip"),
      div(
        class = "viz-color-grid",
        viz_color(ns, "nj_color", "Lines / Text", "#000000"),
        viz_color(ns, "nj_bg", "Background", "#ffffff"),
        viz_color(ns, "nj_title_color", "Title", "#000000"),
        viz_color(ns, "nj_tiplab_color", "Tip Label", "#000000"),
        viz_color(ns, "nj_tiplab_fill", "Label Panel", "#84D9A0"),
        viz_color(ns, "nj_branch_color", "Branch Label", "#000000"),
        viz_color(ns, "nj_branch_label_color", "Branch Panel", "#FFB7B7"),
        viz_color(ns, "nj_tippoint_color", "Tip Point", "#3A4657"),
        viz_color(ns, "nj_nodepoint_color", "Node Point", "#3A4657")
      )
    ),
    # Elements ---------------------------------------------------------------
    nav_panel(
      "Elements",
      icon = icon("shapes"),
      accordion(
        open = "Tip Points",
        accordion_panel(
          "Tip Points",
          icon = icon("circle"),
          input_switch(ns("nj_tippoint_show"), "Show tip points", FALSE),
          selectInput(ns("nj_tippoint_shape"), "Shape", point_shapes),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(
              ns("nj_tippoint_alpha"),
              "Opacity",
              0.1,
              1,
              0.5,
              step = 0.05,
              ticks = FALSE
            ),
            sliderInput(
              ns("nj_tippoint_size"),
              "Size",
              1,
              20,
              4,
              step = 0.5,
              ticks = FALSE
            )
          )
        ),
        accordion_panel(
          "Node Points",
          icon = icon("circle-dot"),
          input_switch(ns("nj_nodepoint_show"), "Show node points", FALSE),
          selectInput(ns("nj_nodepoint_shape"), "Shape", point_shapes),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(
              ns("nj_nodepoint_alpha"),
              "Opacity",
              0.1,
              1,
              1,
              step = 0.05,
              ticks = FALSE
            ),
            sliderInput(
              ns("nj_nodepoint_size"),
              "Size",
              1,
              20,
              2.5,
              step = 0.5,
              ticks = FALSE
            )
          )
        ),
        accordion_panel(
          "Tiles",
          icon = icon("table-cells"),
          selectInput(ns("nj_tile_number"), "Tile", as.character(1:5)),
          sliderInput(
            ns("nj_fruit_alpha"),
            "Opacity",
            0.1,
            1,
            1,
            step = 0.05,
            ticks = FALSE
          ),
          sliderInput(
            ns("nj_fruit_width"),
            "Width",
            0.1,
            10,
            2,
            step = 0.1,
            ticks = FALSE
          ),
          sliderInput(
            ns("nj_fruit_offset"),
            "Position",
            -0.6,
            0.6,
            0.05,
            step = 0.01,
            ticks = FALSE
          )
        ),
        accordion_panel(
          "Clade Highlight",
          icon = icon("highlighter"),
          input_switch(ns("nj_nodelabel_show"), "Toggle node view", FALSE),
          pickerInput(
            ns("nj_parentnode"),
            "Nodes",
            choices = character(0),
            multiple = TRUE,
            options = list(`live-search` = TRUE, size = 10)
          ),
          viz_color(ns, "nj_clade_scale", "Highlight colour", "#D0F221"),
          selectInput(
            ns("nj_clade_type"),
            "Form",
            c(Rounded = "roundrect", Rectangular = "rect")
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
            ns("nj_aspect_ratio"),
            "Aspect ratio",
            0.5,
            2,
            0.6,
            step = 0.1,
            ticks = FALSE
          ),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(
              ns("nj_v"),
              "Vertical",
              -0.5,
              0.5,
              0,
              step = 0.01,
              ticks = FALSE
            ),
            sliderInput(
              ns("nj_h"),
              "Horizontal",
              -0.5,
              0.5,
              -0.05,
              step = 0.01,
              ticks = FALSE
            )
          ),
          sliderInput(
            ns("nj_zoom"),
            "Zoom",
            0.5,
            1.5,
            0.95,
            step = 0.05,
            ticks = FALSE
          )
        ),
        accordion_panel(
          "Tree Rooting",
          icon = icon("seedling"),
          selectInput(ns("nj_root_isolate"), "Outgroup", c("Automatic"))
        ),
        accordion_panel(
          "Layout",
          icon = icon("project-diagram"),
          selectInput(
            ns("nj_layout"),
            "Layout",
            list(
              Linear = c(
                Rectangular = "rectangular",
                Roundrect = "roundrect",
                Slanted = "slanted",
                Ellipse = "ellipse"
              ),
              Circular = c(Circular = "circular", Inward = "inward")
            )
          ),
          input_switch(ns("nj_ladder"), "Ladderize", TRUE),
          input_switch(ns("nj_rootedge_show"), "Root edge", TRUE),
          input_switch(ns("nj_treescale_show"), "Tree scale", TRUE)
        ),
        accordion_panel(
          "Legend",
          icon = icon("list"),
          radioGroupButtons(
            ns("nj_legend_orientation"),
            "Orientation",
            c(Vertical = "vertical", Horizontal = "horizontal"),
            justified = TRUE
          ),
          sliderInput(ns("nj_legend_size"), "Size", 5, 25, 10, ticks = FALSE),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(
              ns("nj_legend_x"),
              "Horizontal",
              -0.9,
              1.9,
              0.9,
              step = 0.1,
              ticks = FALSE
            ),
            sliderInput(
              ns("nj_legend_y"),
              "Vertical",
              -1.5,
              1.5,
              0.2,
              step = 0.1,
              ticks = FALSE
            )
          )
        )
      )
    ),
    export_panel(ns, "nj", c("png", "jpeg", "bmp", "svg"))
  )
}

# --- MST control tabs --------------------------------------------------------

mst_controls <- function(ns) {
  navset_card_tab(
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
        viz_color(ns, "mst_background_color", "Background", "#ffffff")
      ),
      input_switch(
        ns("mst_background_transparent"),
        "Transparent background",
        FALSE
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
            )
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
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = div(
        class = "viz-sidebar-title",
        div(class = "sidebar-title", "Visualization"),
        tooltip(
          icon("circle-info", class = "text-muted"),
          paste(
            "Generate a Minimum Spanning Tree (MST) or a hierarchical tree from",
            "the loaded database, then refine its appearance with the controls."
          )
        )
      ),
      width = 320,
      radioGroupButtons(
        ns("plot_type"),
        label = "Plot type",
        choices = c("MST", "Tree"),
        selected = "MST",
        justified = TRUE
      ),
      uiOutput(ns("algo_ui")),
      actionButton(
        ns("generate"),
        "Generate Plot",
        icon = icon("play")
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          "Options",
          icon = icon("gear"),
          selectInput(
            ns("na_handling"),
            "Missing values",
            choices = c(
              "Ignore for pairwise comparison" = "ignore_na",
              "Omit loci with missing values" = "omit",
              "Treat missing as allele variant" = "category"
            )
          )
        )
      ),
      actionButton(
        ns("reset"),
        "Reset settings",
        icon = icon("rotate-left")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_body(uiOutput(ns("plot_area")))
      ),
      # Controls swap with the engine. A fill-carrier output lets the rendered
      # navset_card_tab stretch to the column height and scroll on overflow.
      as_fill_carrier(uiOutput(ns("controls")))
    ),
    # MutationObserver: as soon as a .viz-nav-wrap mounts in the DOM (controls
    # swap on engine change), copy each icon-only tab's text content to its
    # title attribute so the browser shows a native hover tooltip.
    tags$script(
      "(function(){
         function labelTabs(wrap){
           wrap.querySelectorAll('.nav-link').forEach(function(a){
             if(!a.title) a.title=a.textContent.trim();
           });
         }
         var mo=new MutationObserver(function(muts){
           muts.forEach(function(m){
             m.addedNodes.forEach(function(n){
               if(n.nodeType!==1)return;
               if(n.classList&&n.classList.contains('viz-nav-wrap')){labelTabs(n);}
               else if(n.querySelector){var w=n.querySelector('.viz-nav-wrap');if(w)labelTabs(w);}
             });
           });
         });
         mo.observe(document.body,{childList:true,subtree:true});
       })();"
    )
  )
}

#' @export
server <- function(id, session_reset = shiny::reactive(0L)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Whether a plot has been generated for the current engine. Drives the
    # preview between its prompt and the (placeholder) rendered image.
    generated <- reactiveVal(FALSE)

    observeEvent(session_reset(), {
      generated(FALSE)
    }, ignoreInit = TRUE)

    # Algorithm picker only applies to the hierarchical Tree engine.
    output$algo_ui <- renderUI({
      render_info("visualization algo_ui")
      if (identical(input$plot_type, "Tree")) {
        prettyRadioButtons(
          ns("algo"),
          "Algorithm",
          choices = c("Neighbour-Joining", "UPGMA")
        )
      }
    })

    # Switching engine clears the stale preview; the controls output re-renders.
    observeEvent(input$plot_type, {
      generated(FALSE)
    })

    observeEvent(input$generate, {
      generated(TRUE)
    })

    # Engine-specific control panel (re-rendered on engine switch).
    # Wrapped in a fill-carrier div so the CSS side-nav transformation can
    # target it without breaking the fill chain.
    output$controls <- renderUI({
      render_info("visualization controls")
      as_fill_carrier(
        div(
          class = "viz-nav-wrap",
          if (identical(input$plot_type, "Tree")) tree_controls(ns) else mst_controls(ns)
        )
      )
    })

    output$plot_area <- renderUI({
      render_info("visualization plot_area")
      type <- if (is.null(input$plot_type)) "MST" else input$plot_type
      plot_placeholder(type, generated())
    })
  })
}
