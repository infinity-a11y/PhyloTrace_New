box::use(
  dplyr[arrange, mutate],
  leaflet[addMarkers, addTiles, fitBounds, leaflet, leafletOutput, leafletProxy, renderLeaflet],
  shiny[bootstrapPage, moduleServer, NS, shinyApp],
  tidygeocoder[geocode],
)


## TO BE REMOVED - TESTING PURPOSES ONLY
locations <- data.frame(
  place = c("Graz, Austria", "Munich, Germany", "Bern, Swiss", "Schaffhausen, Swiss", "Villingen-Schwenningen, Germany")
  # place = c("North Macedonia", "Serbia", "Croatia", "Slovenia", "Austria", "Germany")
)

coords <- locations |>
  geocode(place, method = "osm", lat = latitude, long = longitude) |>
  mutate(date = seq(as.Date("2026-01-01"), by = "month", length.out = nrow(locations))) |>
  arrange(date)
## TO BE REMOVED

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    leafletOutput(ns("map"))
  )
}

#' @export
server <- function(id)  {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() |> 
        addTiles() |> 
        fitBounds(
          lng1 = min(coords$longitude),
          lat1 = min(coords$latitude),
          lng2 = max(coords$longitude),
          lat2 = max(coords$latitude)
        )
    })

    proxy <- leafletProxy(session$ns("map"))
    proxy |> 
      addMarkers(data = coords)
  })
}

plot_map_demo <- function() {
  demo_ui <- ui("demo")
  demo_server <- function(input, output, session) {
    server("demo")
    
  }
  shinyApp(demo_ui, demo_server)
}