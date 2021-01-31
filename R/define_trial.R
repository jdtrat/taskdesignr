#' Define a template for trials
#'
#' @param screen_temps A list of screen templates defined with \code{\link{screen_template}}.
#' @return
#' @export
#'
#' @examples
trial_template <- function(screen_temps) {

  names <- purrr::map_chr(1:length(screen_temps), ~ paste0("screen_", .x))

  trial_temp <- screen_temps %>%
    purrr::set_names(names)

  return(trial_temp)
}
