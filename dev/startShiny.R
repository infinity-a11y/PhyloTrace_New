startShiny <- function() {
  paths <- c(
    "/usr/bin/brave-browser"
  )

  options(
    browser = paths[which(file.exists(paths))]
  )

  rhino::build_sass()

  shiny::runApp(launch.browser = TRUE)
}
