define_screen <- function(...) {

  elements <- base::list(...)

  shiny::fluidPage(
    purrr::map(.x = elements, ~place_horizontal(.x))
    )
}


#' Define Screen - in Progress
#'
#' @param list_input Takes a list of tags at the moment.
#'
#' @return
#' @export
#'
#' @examples
define_screen_working <- function(list_input) {
  shiny::fillPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "taskdesignr.css")
    ),
        div(class = "grid",
            list_input)
  )
}

#' Add text to a screen
#'
#' @param text Text to display.
#' @param tag_type Tag types from \link[shiny]{tags}. Default is "h2".
#' @param position One of the following screen positions (character string):
#' * upper-left
#' * upper-center
#' * upper-right
#' * middle-left
#' * middle-center
#' * middle-right
#' * lower-left
#' * lower-center
#' * lower-right
#' @param ... Additional arguments such as style
#'
#' @return Text UI for Shiny application
#' @export
#'
#' @examples
#'
#' \dontrun{
#' add_text("Hello, World.", tag_type = "h3", style = "color: #4d3a7d;")
#' }
#'
add_text <- function(text, tag_type = "h2", position = "upper center", ...) {

  tag <- switch(tag_type,
                h1 = shiny::h1(text, ...),
                h2 = shiny::h2(text, ...),
                h3 = shiny::h3(text, ...),
                h4 = shiny::h4(text, ...),
                h5 = shiny::h5(text, ...),
                h6 = shiny::h6(text, ...),
                p = shiny::p(text, ...))

  return(base::list(ui = tag,
                    position = position))

}

#' Add images
#'
#' @param file The name of the file in www directory.
#' @param position
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_image <- function(file, position = "middle center", ...) {

  tag <- shiny::img(src = file, ...)

  return(base::list(ui = tag,
                    position = position))

}

#' Add text-based sliders
#'
#' Wrapper around shinyWidgets sliderTextInput.
#'
#' @param inputId
#' @param label
#' @param choices
#' @param position...
#'
#' @return
#' @export
#'

add_slider <- function(inputId, label, choices, position, ...) {
  tag <- shinyWidgets::sliderTextInput(inputId = inputId,
                                       label = label,
                                       choices = choices, ...)

  return(base::list(ui = tag,
                    position = position))
}

#' Place UI elements in the horizontal position for trial screens
#'
#' @param object The output of \code{\link{add_text}} and other taskdesignr functions.
#'
#' @return Positioned HTML tag.
#'
place_horizontal <- function(object) {

  ui_element <- object$ui
  position <- object$position
  if (base::grepl(pattern = "\\-left", position)) {
    shiny::tagList(
      shiny::column(width = 4,
                    offset = 0,
                    ui_element)
    )
  } else if (base::grepl(pattern = "\\-center", position)) {
    shiny::tagList(
      shiny::column(width = 4,
                    offset = 4,
                    ui_element)
    )
  } else if (base::grepl(pattern = "\\-right", position)) {
    shiny::tagList(
      shiny::column(width = 4,
                    offset = 8,
                    ui_element)
    )
  }
}


#' Place UI elements in the horizontal position for trial screens
#'
#' @param object The output of \code{\link{add_text}} and other taskdesignr functions.
#'
#' @return Positioned HTML tag.
#'
# place_vertical <- function(object) {
#
#   ui_element <- object$ui
#   position <- object$position
#
#   if (base::grepl(pattern = "\\-left", position)) {
#     shiny::tagList(
#       shiny::column(width = 4,
#                     offset = 0,
#                     ui_element)
#     )
#   } else if (base::grepl(pattern = "\\-center", position)) {
#     shiny::tagList(
#       shiny::column(width = 4,
#                     offset = 4,
#                     ui_element)
#     )
#   } else if (base::grepl(pattern = "\\-right", position)) {
#     shiny::tagList(
#       shiny::column(width = 4,
#                     offset = 8,
#                     ui_element)
#     )
#   }
# }
#
# #
#
# if (base::grepl(pattern = "upper\\-", pattern)) {
#   shiny::tagList(
#     shiny::fluidRow(
#
#     )
#   )
# }
