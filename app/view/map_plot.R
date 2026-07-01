# app/view/map_plot.R
#
# Geographic (leaflet) visualization submodule. Owns its own right-hand control
# sidebar, the map output and all map-specific reactive state. Mounted by
# app/view/visualization.R inside a navset_hidden panel; the shared Generate
# button, plot type, session reset and per-isolate metadata are forwarded in as
# reactives — the same contract the MST and Tree engines use. Isolate
# coordinates are derived from the metadata's spatial fields
# (geo_loc_name_state_province + geo_loc_name_country), geocoded once per
# distinct place via OSM/Nominatim on Generate; the sidebar controls then
# restyle the markers client-side without re-geocoding.

box::use(
  shiny[
    NS,
    moduleServer,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    req,
    div,
    icon,
    selectInput,
    sliderInput,
    checkboxGroupInput,
    updateSelectInput,
    bootstrapPage,
    shinyApp,
  ],
  bslib[
    sidebar,
    layout_sidebar,
    card,
    card_body,
    navset_tab,
    nav_panel,
    input_switch,
    as_fill_carrier,
  ],
  leaflet[
    leaflet,
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    addTiles,
    addProviderTiles,
    addCircleMarkers,
    addLegend,
    clearMarkers,
    clearMarkerClusters,
    clearControls,
    clearTiles,
    markerClusterOptions,
    colorFactor,
    fitBounds,
    setView,
  ],
  tidygeocoder[geocode],
)
box::use(
  app / logic / viz_helpers[viz_color, scale_select],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- coordinate resolution ---------------------------------------------------

# Build a geocoded coordinate table from the isolate metadata. State/province
# and country are joined into a single place string; the distinct places are
# geocoded once (not per isolate) and merged back onto every isolate. All
# metadata columns are retained (so any field can drive marker colour). Rows
# with no spatial fields, or that fail to geocode, are dropped. Returns the
# metadata columns plus place, longitude and latitude, ordered by collection
# date, or NULL when nothing is mappable.
build_map_coords <- function(meta) {
  if (is.null(meta) || !nrow(meta)) {
    return(NULL)
  }

  state <- meta$geo_loc_name_state_province
  country <- meta$geo_loc_name_country

  # One place string per isolate: the non-empty spatial parts, most specific
  # first ("Bavaria, Germany"), so Nominatim gets an unambiguous query.
  place <- vapply(
    seq_len(nrow(meta)),
    function(i) {
      parts <- trimws(c(state[i], country[i]))
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste(parts, collapse = ", ")
    },
    character(1)
  )

  df <- meta
  df$place <- place
  df <- df[nzchar(df$place), , drop = FALSE]
  if (!nrow(df)) {
    return(NULL)
  }

  # Geocode each distinct place once, then merge back onto the isolates.
  places <- data.frame(place = unique(df$place), stringsAsFactors = FALSE)
  located <- geocode(
    places,
    place,
    method = "osm",
    lat = "latitude",
    long = "longitude"
  )

  out <- merge(df, located, by = "place", all.x = TRUE)
  out <- out[!is.na(out$longitude) & !is.na(out$latitude), , drop = FALSE]
  if (!nrow(out)) {
    return(NULL)
  }
  if ("sample_collection_date" %in% names(out)) {
    out <- out[order(out$sample_collection_date), , drop = FALSE]
  }
  out
}

# Build a per-marker HTML popup from the selected metadata fields.
build_popup <- function(coords, fields) {
  labels <- c(
    isolate = "Isolate",
    place = "Location",
    sample_collection_date = "Date",
    specimen_source_id = "Specimen",
    purpose_of_sampling = "Purpose"
  )
  fields <- intersect(fields, names(coords))
  if (!length(fields)) {
    return(NULL)
  }
  rows <- lapply(fields, function(f) {
    lab <- if (f %in% names(labels)) labels[[f]] else f
    paste0("<b>", lab, ":</b> ", coords[[f]])
  })
  Reduce(function(a, b) paste(a, b, sep = "<br>"), rows)
}

# --- control sidebar ---------------------------------------------------------

# Tabbed control panel (mirrors mst_controls / the Tree controls): basemap,
# marker styling, variable colouring and popup contents.
map_controls <- function(ns) {
  navset_tab(
    # Basemap ----------------------------------------------------------------
    nav_panel(
      "Basemap",
      icon = icon("layer-group"),
      selectInput(
        ns("map_tiles"),
        "Base map",
        choices = c(
          "OpenStreetMap" = "OpenStreetMap",
          "Carto Light" = "CartoDB.Positron",
          "Carto Dark" = "CartoDB.DarkMatter",
          "Satellite" = "Esri.WorldImagery",
          "Topographic" = "Esri.WorldTopoMap"
        )
      )
    ),
    # Markers ----------------------------------------------------------------
    nav_panel(
      "Markers",
      icon = icon("location-dot"),
      input_switch(ns("map_cluster"), "Cluster nearby markers", TRUE),
      sliderInput(
        ns("map_radius"),
        "Marker size",
        min = 3,
        max = 18,
        value = 7,
        step = 1
      ),
      sliderInput(
        ns("map_opacity"),
        "Fill opacity",
        min = 0.1,
        max = 1,
        value = 0.85,
        step = 0.05
      ),
      viz_color(ns, "map_marker_color", "Marker colour", "#2c7fb8")
    ),
    # Variable colouring -----------------------------------------------------
    nav_panel(
      "Colour",
      icon = icon("palette"),
      input_switch(ns("map_color_var"), "Colour by variable", FALSE),
      selectInput(ns("map_col_var"), "Variable", choices = NULL),
      scale_select(ns, "map_col_scale")
    ),
    # Popup contents ---------------------------------------------------------
    nav_panel(
      "Labels",
      icon = icon("tag"),
      checkboxGroupInput(
        ns("map_popup"),
        "Popup fields",
        choices = c(
          "Isolate" = "isolate",
          "Location" = "place",
          "Date" = "sample_collection_date",
          "Specimen" = "specimen_source_id",
          "Purpose" = "purpose_of_sampling"
        ),
        selected = c("isolate", "place", "sample_collection_date")
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    id = ns("plot_sidebar"),
    border = FALSE,
    sidebar = sidebar(
      id = ns("controls_sidebar"),
      class = "viz-controls-sidebar",
      position = "right",
      width = 380,
      open = TRUE,
      fillable = TRUE,
      as_fill_carrier(div(class = "viz-nav-wrap", map_controls(ns)))
    ),
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    card(
      full_screen = TRUE,
      class = "plot-card",
      card_body(
        # Wrap the leaflet output (a shiny.tag.list) in a real div so bslib's
        # fill machinery has a tag() to bind, and so the map fills the card.
        div(
          class = "html-fill-container html-fill-item",
          leafletOutput(ns("map"), height = "100%")
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
  viz_metadata = shiny::reactive(NULL),
  na_handling = shiny::reactive("ignore_na"),
  generate = shiny::reactive(0L),
  plot_type = shiny::reactive("MST")
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Geocoded coordinates for the currently generated map. Held in a
    # reactiveVal (not eventReactive) so a Generate for another engine — which
    # also ticks the shared generate() — leaves this engine's result untouched.
    map_coords <- reactiveVal(NULL)

    # Spinner shown over the map while geocoding (a blocking network call).
    waiter <- waiter::Waiter$new(
      id = ns("map"),
      html = shiny::tagList(
        waiter::spin_flower(),
        shiny::div(style = "margin-top:1rem;", "Geocoding locations…")
      ),
      color = "rgba(255,255,255,0.7)"
    )

    # Base map: world tiles centred on Central Europe until markers arrive. The
    # initial tile layer is drawn here (so the map is never blank); the basemap
    # observer below only swaps it when the control changes.
    output$map <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        setView(lng = 10, lat = 50, zoom = 4)
    })

    # Leaflet initialises at zero size while its panel is hidden inside the
    # navset_hidden and never recomputes when shown, leaving a grey box with no
    # markers. Dispatching a window resize makes Leaflet (trackResize = TRUE)
    # call invalidateSize(); do it whenever the Map engine becomes visible.
    nudge_resize <- function(delay = 250) {
      shinyjs::runjs(sprintf(
        "setTimeout(function(){window.dispatchEvent(new Event('resize'));}, %d);",
        delay
      ))
    }
    observeEvent(plot_type(), {
      if (identical(plot_type(), "Map")) {
        nudge_resize()
      }
    })

    # Geocode + populate the colour-variable choices only when Map is the active
    # engine and Generate is clicked (mirrors the MST/Tree guard). The heavy
    # geocoding is covered by the waiter spinner.
    observeEvent(generate(), {
      if (!identical(plot_type(), "Map")) {
        return()
      }
      meta <- viz_metadata()
      req(meta)

      # Metadata-backed colour variable choices (same approach as the MST/Tree
      # engines populating their selects on Generate).
      fields <- setdiff(names(meta), "isolate")
      updateSelectInput(
        session,
        "map_col_var",
        choices = fields,
        selected = if (isTRUE(input$map_col_var %in% fields)) {
          input$map_col_var
        } else {
          fields[1]
        }
      )

      waiter$show()
      on.exit(waiter$hide(), add = TRUE)

      coords <- tryCatch(
        build_map_coords(meta),
        error = function(e) {
          shiny::showNotification(
            paste("Could not geocode isolate locations:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )

      if (is.null(coords) || !nrow(coords)) {
        shiny::showNotification(
          "No mappable locations found in the metadata (country / state fields).",
          type = "warning"
        )
      }
      map_coords(coords)
      # The setup sidebar collapses on Generate (parent), changing the map's
      # width — recompute the Leaflet size once the markers are drawn.
      nudge_resize(350)
    })

    # Basemap tiles: swapped live from the Basemap control. The initial tile
    # layer is drawn by renderLeaflet, so only *changes* are handled here.
    observeEvent(
      input$map_tiles,
      {
        proxy <- leafletProxy("map") |> clearTiles()
        if (identical(input$map_tiles, "OpenStreetMap")) {
          proxy |> addTiles()
        } else {
          proxy |> addProviderTiles(input$map_tiles)
        }
      },
      ignoreInit = TRUE
    )

    # Markers: redrawn whenever the coordinates or any styling control change —
    # no re-geocoding, just a client-side restyle via the proxy.
    observe({
      coords <- map_coords()
      opts <- list(
        cluster = input$map_cluster,
        radius = input$map_radius %||% 7,
        opacity = input$map_opacity %||% 0.85,
        marker_color = input$map_marker_color %||% "#2c7fb8",
        color_var = isTRUE(input$map_color_var),
        col_var = input$map_col_var,
        col_scale = input$map_col_scale %||% "viridis",
        popup_fields = input$map_popup
      )

      proxy <- leafletProxy("map") |>
        clearMarkers() |>
        clearMarkerClusters() |>
        clearControls()
      req(!is.null(coords) && nrow(coords) > 0)

      # Colour by a metadata variable (with a legend) or a single fixed colour.
      use_var <- opts$color_var &&
        !is.null(opts$col_var) &&
        opts$col_var %in% names(coords)
      if (use_var) {
        vals <- coords[[opts$col_var]]
        pal <- colorFactor(opts$col_scale, domain = vals, na.color = "#808080")
        fill <- pal(vals)
      } else {
        fill <- opts$marker_color
      }

      proxy |>
        addCircleMarkers(
          data = coords,
          lng = ~longitude,
          lat = ~latitude,
          radius = opts$radius,
          stroke = TRUE,
          color = "#333333",
          weight = 1,
          fillColor = fill,
          fillOpacity = opts$opacity,
          popup = build_popup(coords, opts$popup_fields),
          clusterOptions = if (isTRUE(opts$cluster)) {
            markerClusterOptions()
          } else {
            NULL
          }
        )

      if (use_var) {
        proxy |>
          addLegend(
            position = "bottomright",
            pal = pal,
            values = vals,
            title = opts$col_var,
            opacity = opts$opacity
          )
      }
    })

    # Fit the view to the isolates once, when a fresh coordinate set arrives
    # (not on every styling tweak, which would keep re-zooming the map).
    observeEvent(map_coords(), {
      coords <- map_coords()
      req(!is.null(coords) && nrow(coords) > 0)
      leafletProxy("map") |>
        fitBounds(
          lng1 = min(coords$longitude),
          lat1 = min(coords$latitude),
          lng2 = max(coords$longitude),
          lat2 = max(coords$latitude)
        )
    })

    # On session reset, clear the markers and cached coordinates.
    observeEvent(
      session_reset(),
      {
        map_coords(NULL)
        leafletProxy("map") |>
          clearMarkers() |>
          clearMarkerClusters() |>
          clearControls()
      },
      ignoreInit = TRUE
    )
  })
}

# Standalone demo (not used by the app): renders the map from a small hardcoded
# metadata table so the module can be exercised outside the visualization shell.
plot_map_demo <- function() {
  demo_meta <- data.frame(
    isolate = paste0("isolate_", 1:5),
    geo_loc_name_country = c(
      "Austria",
      "Germany",
      "Switzerland",
      "Switzerland",
      "Germany"
    ),
    geo_loc_name_state_province = c(
      "Styria",
      "Bavaria",
      "Bern",
      "Schaffhausen",
      "Baden-Wuerttemberg"
    ),
    sample_collection_date = as.character(
      seq(as.Date("2026-01-01"), by = "month", length.out = 5)
    ),
    specimen_source_id = c(
      "Sputum",
      "Blood",
      "Urine",
      "Wound swab",
      "Sputum"
    ),
    purpose_of_sampling = "Research study",
    stringsAsFactors = FALSE
  )

  demo_ui <- bootstrapPage(ui("demo"))
  demo_server <- function(input, output, session) {
    server(
      "demo",
      viz_metadata = shiny::reactive(demo_meta),
      generate = shiny::reactive(1L),
      plot_type = shiny::reactive("Map")
    )
  }
  shinyApp(demo_ui, demo_server)
}
