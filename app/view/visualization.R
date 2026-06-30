# app/view/visualization.R

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    outputOptions,
    renderUI,
    uiOutput,
    renderPlot,
    plotOutput,
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
    hr,
    sliderInput,
    selectInput,
    textInput,
    numericInput,
    actionButton,
  ],
  bslib[
    sidebar,
    layout_sidebar,
    card,
    card_header,
    card_body,
    layout_columns,
    accordion,
    accordion_panel,
    navset_tab,
    nav_panel,
    input_switch,
    tooltip,
    as_fill_carrier,
  ],
  shinyWidgets[radioGroupButtons, prettyRadioButtons, colorPickr, pickerInput],
  visNetwork[visNetworkOutput, renderVisNetwork],
)
box::use(
  app / logic / functions[render_info],
  app /
    logic /
    phylo[
      compute_phylo_tree,
      compute_mst,
      build_mst_visnetwork,
      save_mst_html,
    ],
  app / logic / tree_plot[build_tree_ggtree, save_tree_plot],
  app / logic / database_functions[make_metadata_table],
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
  navset_tab(
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
ui <- function(id) {
  ns <- NS(id)

  # Nested sidebars: left = plot setup, right = engine controls, plot in between.
  layout_sidebar(
    fillable = TRUE,
    border_radius = FALSE,
    class = "p-0",
    sidebar = sidebar(
      id = ns("sidebar"),
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
    layout_sidebar(
      id = "plot-sidebar",
      border = FALSE,
      sidebar = sidebar(
        id = ns("controls_sidebar"),
        position = "right",
        width = 380,
        open = TRUE,
        as_fill_carrier(uiOutput(ns("controls")))
      ),
      shinyjs::useShinyjs(),
      card(
        full_screen = TRUE,
        class = "plot-card",
        card_body(uiOutput(ns("plot_area")))
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
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  typing_status = shiny::reactive("idle"),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Whether a plot has been generated for the current engine. Drives the
    # preview between its prompt and the (placeholder) rendered image.
    generated <- reactiveVal(FALSE)

    # The computed phylo tree for the Tree engine (NULL until generated).
    tree_obj <- reactiveVal(NULL)

    # The computed MST igraph object for the MST engine (NULL until generated).
    mst_obj <- reactiveVal(NULL)

    # Per-isolate metadata (cached until the database changes); feeds both
    # engines' labels, mappings, and metadata-backed select choices.
    viz_metadata <- reactive({
      req(db_path())
      make_metadata_table(db_path())
    })

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

    # --- Tree (ggtree) state --------------------------------------------------

    # Settings for the five metadata tile strips. The Mapping and Elements tabs
    # each edit one strip (selected by nj_tile_num / nj_tile_number).
    nj_tiles <- reactiveVal(
      replicate(
        5,
        list(
          show = FALSE,
          variable = NULL,
          scale = "viridis",
          alpha = 1,
          width = 2,
          offset = 0.05
        ),
        simplify = FALSE
      )
    )

    # Metadata columns selected for the heatmap annotation (via its modal).
    nj_heatmap_select <- reactiveVal(character(0))

    # Resolved Tree control values, shared by the live render and the export.
    tree_opts <- reactive(
      list(
        # Layout / rooting.
        root = input$nj_root_isolate,
        layout = input$nj_layout,
        ladderize = input$nj_ladder,
        line_color = input$nj_color,
        bg = input$nj_bg,
        # Tip labels.
        tiplab_show = input$nj_tiplab_show,
        tiplab = input$nj_tiplab,
        tiplab_size = input$nj_tiplab_size,
        tiplab_alpha = input$nj_tiplab_alpha,
        tiplab_fontface = input$nj_tiplab_fontface,
        tiplab_position = input$nj_tiplab_position %||% 0,
        tiplab_angle = input$nj_tiplab_angle,
        align = input$nj_align,
        label_panel = input$nj_geom %||% FALSE,
        tiplab_color = input$nj_tiplab_color,
        tiplab_fill = input$nj_tiplab_fill,
        # Tip-label colour mapping.
        mapping_show = input$nj_mapping_show,
        color_mapping = input$nj_color_mapping,
        tiplab_scale = input$nj_tiplab_scale,
        # Branch labels.
        branch_show = input$nj_show_branch_label,
        branch_label = input$nj_branch_label,
        branch_size = input$nj_branch_size,
        branch_cutoff = input$nj_branchlabel_cutoff,
        branch_color = input$nj_branch_color,
        branch_label_color = input$nj_branch_label_color,
        # Tip / node points.
        tippoint_show = input$nj_tippoint_show,
        tippoint_alpha = input$nj_tippoint_alpha,
        tippoint_size = input$nj_tippoint_size,
        tippoint_color = input$nj_tippoint_color,
        tippoint_shape = input$nj_tippoint_shape,
        tipcolor_mapping_show = input$nj_tipcolor_mapping_show,
        tipcolor_mapping = input$nj_tipcolor_mapping,
        tippoint_scale = input$nj_tippoint_scale,
        tipshape_mapping_show = input$nj_tipshape_mapping_show,
        tipshape_mapping = input$nj_tipshape_mapping,
        nodepoint_show = input$nj_nodepoint_show,
        nodepoint_alpha = input$nj_nodepoint_alpha,
        nodepoint_color = input$nj_nodepoint_color,
        nodepoint_shape = input$nj_nodepoint_shape,
        nodepoint_size = input$nj_nodepoint_size,
        # Clade highlights.
        nodelabel_show = input$nj_nodelabel_show,
        parentnodes = input$nj_parentnode,
        clade_color = input$nj_clade_scale,
        clade_type = input$nj_clade_type,
        # Tiles / heatmap.
        tiles = nj_tiles(),
        heatmap_show = input$nj_heatmap_show,
        heatmap_select = nj_heatmap_select(),
        # Elements toggles.
        rootedge_show = input$nj_rootedge_show,
        treescale_show = input$nj_treescale_show,
        # Titles.
        title = input$nj_title,
        subtitle = input$nj_subtitle,
        title_color = input$nj_title_color,
        title_size = input$nj_title_size,
        subtitle_size = input$nj_subtitle_size,
        # Dimensions / legend.
        zoom = input$nj_zoom,
        h = input$nj_h,
        v = input$nj_v,
        legend_orientation = input$nj_legend_orientation,
        legend_x = input$nj_legend_x,
        legend_y = input$nj_legend_y,
        legend_size = input$nj_legend_size
      )
    )

    # The ggtree plot: rebuilt live as controls change; the phylo itself is
    # never recomputed here.
    tree_plot_built <- reactive({
      req(tree_obj())
      build_tree_ggtree(tree_obj(), viz_metadata(), tree_opts())
    })

    observeEvent(
      session_reset(),
      {
        generated(FALSE)
      },
      ignoreInit = TRUE
    )

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
      # Collapse the sidebar to give the freshly generated plot full width.
      # `sidebar_toggle` sends an input message, which the module session
      # namespaces itself, so pass the bare (un-namespaced) id here.
      bslib::toggle_sidebar(id = "sidebar", open = FALSE, session = session)

      # Both engines compute a real graphic from the loaded database: the Tree
      # engine an NJ/UPGMA phylo tree, the MST engine a minimum spanning tree.
      if (identical(input$plot_type, "Tree")) {
        tree <- tryCatch(
          compute_phylo_tree(db_path(), input$na_handling, input$algo),
          error = function(e) {
            shiny::showNotification(
              paste("Tree computation failed:", conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )

        if (is.null(tree)) {
          shiny::showNotification(
            "Could not build a tree: need at least 3 isolates in the database.",
            type = "warning"
          )
          tree_obj(NULL)
          generated(FALSE)
          return()
        }

        tree_obj(tree)

        # Drive the metadata-backed selects from the real fields, populate the
        # outgroup list with tip names, and the clade picker with node numbers.
        fields <- names(viz_metadata())
        keep <- function(id, choices, default) {
          updateSelectInput(
            session,
            id,
            choices = choices,
            selected = if (isTRUE(input[[id]] %in% choices)) {
              input[[id]]
            } else {
              default
            }
          )
        }
        keep("nj_tiplab", fields, "isolate")
        keep("nj_color_mapping", fields, fields[1])
        keep("nj_tipcolor_mapping", fields, fields[1])
        keep("nj_tipshape_mapping", fields, fields[1])
        keep("nj_fruit_variable", fields, fields[1])
        keep(
          "nj_branch_label",
          c("Allelic Distance", fields),
          "Allelic Distance"
        )

        tips <- tree$tip.label
        updateSelectInput(
          session,
          "nj_root_isolate",
          choices = c("Automatic", tips),
          selected = if (isTRUE(input$nj_root_isolate %in% tips)) {
            input$nj_root_isolate
          } else {
            "Automatic"
          }
        )
        n_tip <- length(tips)
        nodes <- as.character(seq.int(n_tip + 1, n_tip + tree$Nnode))
        shinyWidgets::updatePickerInput(
          session,
          "nj_parentnode",
          choices = nodes,
          selected = intersect(input$nj_parentnode, nodes)
        )
      } else {
        graph <- tryCatch(
          compute_mst(db_path(), input$na_handling),
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
          mst_obj(NULL)
          generated(FALSE)
          return()
        }

        mst_obj(graph)

        # Drive the label-source and variable selects from the real metadata
        # fields rather than the placeholder choices.
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
      }

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
          if (identical(input$plot_type, "Tree")) {
            tree_controls(ns)
          } else {
            mst_controls(ns)
          }
        )
      )
    })

    output$plot_area <- renderUI({
      render_info("visualization plot_area")
      type <- if (is.null(input$plot_type)) "MST" else input$plot_type
      # Once generated, each engine renders its live plot; before that (or on a
      # failed build) the placeholder/prompt is shown.
      if (
        identical(type, "Tree") && isTRUE(generated()) && !is.null(tree_obj())
      ) {
        div(
          class = "viz-plot-stage",
          plotOutput(ns("tree_plot")),
          # Hidden target the export action button clicks to start the download.
          div(
            style = "display:none;",
            downloadButton(ns("download_nj"), "Download plot")
          )
        )
      } else if (
        identical(type, "MST") && isTRUE(generated()) && !is.null(mst_obj())
      ) {
        # Canvas width derives from the panel height and the aspect-ratio
        # control; the height is only known after a first render, so fall back
        # until the browser reports it.
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
      } else {
        plot_placeholder(type, generated())
      }
    })

    # The ggtree plot, sized from the panel width and the aspect-ratio control
    # (circular/inward layouts are square), rendered at print resolution.
    output$tree_plot <- renderPlot(
      {
        render_info("visualization tree_plot")
        tree_plot_built()
      },
      height = function() {
        w <- session$clientData[[paste0("output_", ns("tree_plot"), "_width")]]
        aspect <- if (
          identical(input$nj_layout, "circular") ||
            identical(input$nj_layout, "inward")
        ) {
          1
        } else {
          input$nj_aspect_ratio %||% 0.6
        }
        if (!is.null(w)) as.integer(w * aspect) else 600L
      },
      res = 192
    )

    output$mst_plot <- renderVisNetwork({
      render_info("visualization mst_plot")
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
        shinyjs::click(ns("mst_html"))
      } else {
        shiny::showNotification(
          "Only HTML export is available currently.",
          type = "message"
        )
      }
    })

    # --- Tree tiles, heatmap, and export -------------------------------------

    # Persist edits to the currently selected tile (Mapping tab → nj_tile_num).
    observeEvent(
      list(input$nj_tiles_show, input$nj_fruit_variable, input$nj_tiles_scale),
      {
        i <- as.integer(input$nj_tile_num)
        tiles <- nj_tiles()
        tiles[[i]]$show <- input$nj_tiles_show
        tiles[[i]]$variable <- input$nj_fruit_variable
        tiles[[i]]$scale <- input$nj_tiles_scale
        nj_tiles(tiles)
      },
      ignoreInit = TRUE
    )
    # Persist edits to the currently selected tile (Elements tab → nj_tile_number).
    observeEvent(
      list(input$nj_fruit_alpha, input$nj_fruit_width, input$nj_fruit_offset),
      {
        i <- as.integer(input$nj_tile_number)
        tiles <- nj_tiles()
        tiles[[i]]$alpha <- input$nj_fruit_alpha
        tiles[[i]]$width <- input$nj_fruit_width
        tiles[[i]]$offset <- input$nj_fruit_offset
        nj_tiles(tiles)
      },
      ignoreInit = TRUE
    )
    # Restore the stored settings into the controls when the tile selector moves.
    observeEvent(input$nj_tile_num, {
      tile <- nj_tiles()[[as.integer(input$nj_tile_num)]]
      bslib::update_switch("nj_tiles_show", value = tile$show)
      updateSelectInput(session, "nj_fruit_variable", selected = tile$variable)
      updateSelectInput(session, "nj_tiles_scale", selected = tile$scale)
    })
    observeEvent(input$nj_tile_number, {
      tile <- nj_tiles()[[as.integer(input$nj_tile_number)]]
      shiny::updateSliderInput(session, "nj_fruit_alpha", value = tile$alpha)
      shiny::updateSliderInput(session, "nj_fruit_width", value = tile$width)
      shiny::updateSliderInput(session, "nj_fruit_offset", value = tile$offset)
    })

    # Heatmap column picker modal.
    observeEvent(input$nj_heatmap_button, {
      fields <- setdiff(names(viz_metadata()), "isolate")
      shiny::showModal(shiny::modalDialog(
        title = "Heatmap variables",
        shiny::checkboxGroupInput(
          ns("nj_heatmap_cols"),
          NULL,
          choices = fields,
          selected = nj_heatmap_select()
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          actionButton(ns("nj_heatmap_apply"), "Apply")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$nj_heatmap_apply, {
      nj_heatmap_select(input$nj_heatmap_cols %||% character(0))
      shiny::removeModal()
    })

    # Render the current tree to a file at the configured aspect ratio.
    output$download_nj <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_tree.", input$nj_filetype)
      },
      content = function(file) {
        aspect <- if (
          identical(input$nj_layout, "circular") ||
            identical(input$nj_layout, "inward")
        ) {
          1
        } else {
          input$nj_aspect_ratio %||% 0.6
        }
        save_tree_plot(tree_plot_built(), file, input$nj_filetype, aspect)
      }
    )
    observeEvent(input$nj_download, {
      shinyjs::click(ns("download_nj"))
    })

    # Keep all rendered outputs reactive while the visualization panel is absent
    # from the DOM (removed by nav_remove on session reset) so reset-triggered
    # reactive changes propagate before the panel is re-inserted.
    outputOptions(output, "algo_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "controls", suspendWhenHidden = FALSE)
    outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    outputOptions(output, "tree_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "mst_plot", suspendWhenHidden = FALSE)
  })
}
