
define_trial_internal <- function(screen_definition) {

  list(screen_type = screen_definition$screen_type,
       ui = screen_definition$screen)

}

#' Define templates for individual screens
#'
#' This function defines what occurs on the individual screens within a trial.
#'
#' @param screen_type A string that describes the component of the trial this
#'   applies to. For example, "presentation", "choice", or "outcome".
#' @param screen_duration A time (milliseconds) that this screen will be
#'   displayed for. Users may also supply a function that returns a numeric
#'   (e.g. rpois(1,6000)).
#' @param ui A shiny UI object or definition
#'
#' @return A list defining the template for the screen type.
#' @export
#'
#' @examples
screen_template <- function(screen_type, screen_duration, ui) {

  output <- list(screen = substitute(ui),
                 screen_type = screen_type,
                 screen_duration = substitute(screen_duration))

  return(output)
}

#
# a <- purrr::map(list(test_presentation, test_choice, test_outcome), ~define_trial_internal(.x))
#
#
#
# purrr::map2_df(.x = a,
#                .y = c(3000, 2000, 1000),
#                ~ tibble::tibble(ui = .x$ui,
#                                 screen_type = .x$screen_type,
#                                 screen_duration = "hi"))
#
#
# test_presentation
