# app/logic/viz_helpers.R
#
# Namespace-agnostic UI helpers and option-set constants shared by the two
# visualization submodules (app/view/visualization_tree.R and
# app/view/visualization_mst.R). Every helper takes the caller's `ns` and
# returns tags, so it is safe to reuse across module namespaces (same pattern
# as `sidebar_menu` in app/logic/functions.R).

box::use(
  shiny[div, selectInput, actionButton, icon, hr, tags],
  bslib[nav_panel],
  shinyWidgets[colorPickr],
)

# --- shared option sets ------------------------------------------------------

# Metadata columns mappable to plot aesthetics.
#' @export
meta_vars <- c("Isolation Date", "Host", "Country", "City", "Database")

# Sources for the isolate (tip) label.
#' @export
label_vars <- c("Assembly Name", "Assembly ID", meta_vars)

# Sources for branch labels (allelic distance is the numeric default).
#' @export
branch_vars <- c("Allelic Distance", meta_vars)

#' @export
fontfaces <- c(
  Plain = "plain",
  Bold = "bold",
  Italic = "italic",
  `Bold Italic` = "bold.italic"
)

#' @export
point_shapes <- c(
  Circle = "circle",
  Square = "square",
  Diamond = "diamond",
  Triangle = "triangle",
  Cross = "cross",
  Asterisk = "asterisk"
)

# ColorBrewer / viridis palettes grouped for the colour-scale selects.
#' @export
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
#' @export
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
#' @export
scale_select <- function(ns, id) {
  selectInput(ns(id), "Colour scale", choices = color_scales, width = "100%")
}

# Export tab, shared by both engines (prefix keeps input ids unique per engine).
#' @export
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
