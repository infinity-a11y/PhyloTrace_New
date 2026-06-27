# app/logic/functions.R

box::use(
  shiny[div, actionButton, icon],
)

#' Build a vertical sidebar menu of action buttons.
#'
#' Turns a menu definition into the clickable button list used in module
#' sidebars (e.g. the Database menu). Each button id is `menu_<value>`,
#' namespaced through `ns`, so the calling module observes
#' `input[["menu_<value>"]]`. The first entry is marked `active` so the default
#' panel is highlighted on load. The icon is fixed for every item.
#'
#' @param ns Namespace function of the calling module (`session$ns` or `NS(id)`).
#' @param items List of menu entries, each a list with at least `value` (button
#'   id suffix / panel id) and `label` (visible text).
#' @return A `div.sidebar-menu` containing one action button per entry.
#'
#' @export
sidebar_menu <- function(ns, items) {
  div(
    class = "sidebar-menu",
    lapply(seq_along(items), function(i) {
      item <- items[[i]]
      actionButton(
        ns(paste0("menu_", item$value)),
        label = item$label,
        icon = icon("caret-right"),
        # The first item is the default panel, so mark it active on load.
        class = paste("db-menu-item", if (i == 1L) "active")
      )
    })
  )
}

#' @export
render_info <- function(output) {
  message(
    format(Sys.time(), digits = 3L),
    " | ",
    "----- Rendering '",
    output,
    "' UI"
  )
}
