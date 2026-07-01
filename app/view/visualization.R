# app/view/visualization.R
#
# Visualization coordinator. Hosts the shared "setup" sidebar (plot type,
# Generate, options, reset) and swaps between two self-contained plot-engine
# submodules — app/view/visualization_mst.R and app/view/visualization_tree.R —
# inside a navset_hidden. Each engine owns its own control panel and plot area;
# this module only forwards the shared reactives (db_path, session_reset, the
# per-isolate metadata, na_handling, the Generate tick and the selected plot
# type) down to both and toggles which panel is visible.

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    reactive,
    req,
    div,
    icon,
    actionButton,
    selectInput,
    tags,
  ],
  bslib[
    sidebar,
    layout_sidebar,
    accordion,
    accordion_panel,
    navset_hidden,
    nav_panel,
    tooltip,
    as_fill_carrier,
  ],
  shinyWidgets[radioGroupButtons, prettyRadioButtons],
)
box::use(
  app / logic / database_functions[make_metadata_table],
  app / view / visualization_mst,
  app / view / visualization_tree,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  # Left = plot setup; the engine submodules (right controls + plot area) live
  # in the hidden tabset swapped by `plot_type`.
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
      # Algorithm is a Tree-only *computation* input (feeds compute_phylo_tree,
      # applied on Generate — like the missing-value handling below), so it lives
      # here in the setup sidebar. Hidden for MST via shinyjs; forwarded to the
      # Tree submodule as a reactive.
      shinyjs::hidden(
        div(
          id = ns("algo_wrap"),
          prettyRadioButtons(
            ns("algo"),
            "Algorithm",
            choices = c("Neighbour-Joining", "UPGMA")
          )
        )
      ),
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
    shinyjs::useShinyjs(),
    # One panel per engine (values match the plot_type choices verbatim). Each
    # panel hosts a submodule's full inner layout_sidebar under its namespace;
    # the shared Generate button id is passed through so the engine's loading
    # overlay can hook it.
    navset_hidden(
      id = ns("engine"),
      as_fill_carrier(nav_panel(
        title = "MST",
        value = "MST",
        visualization_mst$ui(ns("mst"), generate_id = ns("generate"))
      )),
      as_fill_carrier(nav_panel(
        title = "Tree",
        value = "Tree",
        visualization_tree$ui(ns("tree"), generate_id = ns("generate"))
      ))
    ),
    # MutationObserver: as soon as a .viz-nav-wrap mounts in the DOM (the panels
    # are inserted with the visualization tab after the DB loads), copy each
    # icon-only tab's text content to its title attribute so the browser shows a
    # native hover tooltip. One observer covers both engines' control panels.
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
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  typing_status = shiny::reactive("idle"),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Per-isolate metadata (cached until the database changes); computed once
    # here and shared with both engines' labels, mappings, and select choices so
    # the database is read at most once per invalidation.
    viz_metadata <- reactive({
      req(db_path())
      make_metadata_table(db_path())
    })

    # Shared reactives forwarded to both engine submodules.
    shared <- list(
      db_path = db_path,
      session_reset = session_reset,
      viz_metadata = viz_metadata,
      na_handling = reactive(input$na_handling),
      generate = reactive(input$generate),
      plot_type = reactive(input$plot_type)
    )
    do.call(visualization_mst$server, c(list("mst"), shared))
    do.call(
      visualization_tree$server,
      c(list("tree"), shared, list(algo = reactive(input$algo)))
    )

    # Swap the visible engine panel when the plot type changes, and show the
    # Tree-only algorithm picker only while Tree is selected.
    observeEvent(input$plot_type, {
      bslib::nav_select("engine", selected = input$plot_type)
      shinyjs::toggle(
        "algo_wrap",
        condition = identical(input$plot_type, "Tree")
      )
    })

    # Collapse the setup sidebar to give the freshly generated plot full width.
    # `toggle_sidebar` sends an input message the module session namespaces
    # itself, so pass the bare (un-namespaced) id here.
    observeEvent(input$generate, {
      bslib::toggle_sidebar(id = "sidebar", open = FALSE, session = session)
    })

    # On session reset, return the engine selector to the default.
    observeEvent(
      session_reset(),
      {
        bslib::nav_select("engine", selected = "MST")
      },
      ignoreInit = TRUE
    )
  })
}
