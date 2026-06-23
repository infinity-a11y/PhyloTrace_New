startShiny <- function(port = 3000, host = "127.0.0.1") {
  rhino::build_sass()

  launch_browser <- function(url) {
    chrome <- Sys.which(c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser"))
    chrome <- chrome[nzchar(chrome)]
    if (length(chrome) > 0) {
      system2(chrome[[1]], args = c("--new-window", paste0("--app=", url)), wait = FALSE)
    } else {
      utils::browseURL(url)
    }
  }

  shiny::runApp(
    rhino::app(),
    port = port,
    host = host,
    launch.browser = launch_browser
  )
}
