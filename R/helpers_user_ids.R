#' Get possible values for random string generation
#'
#' @param digits Should digits be an option (0-9). TRUE by default.
#' @param upperalpha Should capital letters be an option (A-Z). TRUE by default.
#' @param loweralpha Should lowercase letters be an option (a-z). TRUE by default.
#' @keywords internal
#'
#' @return A character vector of some combination of digits, uppercase, and lowercase letters.
#'
#' @examples
#'
#' get_possible_values()
#'
get_possible_values <- function(digits = TRUE,
                                upperalpha = TRUE,
                                loweralpha = TRUE) {


  if (digits == TRUE) {digits <- as.character(seq(0,9))
  } else if (digits != TRUE) {digits <- NULL}

  if (upperalpha == TRUE) {upperalpha <- LETTERS
  } else if (upperalpha != TRUE) {upperalpha <- NULL}

  if (loweralpha == TRUE) {loweralpha <- letters
  } else if (loweralpha != TRUE) {loweralpha <- NULL}

  possible_values <- c(digits, loweralpha, upperalpha)

  return(possible_values)

}
