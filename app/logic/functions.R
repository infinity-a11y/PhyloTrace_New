# app/logic/functions.R

box::use()

#' @export
render_info <- function(output) {
  message(
    format(Sys.time(), digits = 3L),
    " | ",
    "-------------------------- Rendering '",
    output,
    "' UI"
  )
}
