
#' Get Unique Questions in a Nested Dataframe
#'
#' @param df A user supplied dataframe in the format of teaching_r_questions.
#'
#' @return A nested dataframe.
#'
#' @importFrom rlang .data
#'
nestUniqueQuestions <- function(df) {
  # nest the sample data
  # replace any NA with "placeholder"
  # I've added the question_number but there has to be a better way to do this...
  df %>%
    dplyr::mutate(option = tidyr::replace_na(.data$option, "placeholder")) %>%
    dplyr::group_by(.data$question) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(question_number = dplyr::row_number(), .before = .data$question) %>%
    tidyr::unnest(.data$data) %>%
    ## Remove this when you figure out dependence !
    dplyr::filter(is.na(.data$dependence)) %>%
    dplyr::group_by(.data$question_number) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
}

#' Generate the UI Code for demoraphic questions
#'
#' @param df A nested dataframe.
#'
#' @return UI Code for a Shiny App.
#'
getUICode_individual <- function(df) {

  inputType <- base::unique(df$input_type)

  if (inputType ==  "select") {
    output <-
      shinyWidgets::pickerInput(
        inputId = base::unique(df$label),
        label = base::unique(df$question),
        choices = df$option,
        options = list(
          title = "Placeholder")
      )
  } else if (inputType == "numeric") {

    output <-
      shinyWidgets::numericInputIcon(
        inputId = base::unique(df$label),
        label = base::unique(df$question),
        value = df$option,
        icon = list(
          #make the df$label sentence case in base R
          base::paste0(base::toupper(base::substring(base::unique(df$label), 1,1)),
                       base::tolower(base::substring(base::unique(df$label), 2)))
        )
      )

  } else if (inputType == "mc") {

    output <-
      shiny::radioButtons(
        inputId = base::unique(df$label),
        label = base::unique(df$question),
        choices = df$option
      )
  } else if (inputType == "text") {

    output <-
      shiny::textInput(inputId = base::unique(df$label),
                label = base::unique(df$question),
                value = df$option)

  } else if (inputType == "y/n") {

    output <-
      shiny::radioButtons(
        inputId = base::unique(df$label),
        label = base::unique(df$question),
        choices = df$option
      )
  }

  return(list(output))

}


#' Generate the UI Code for demoraphic questions
#'
#' Create the UI code for a Shiny app based on user-supplied questions. Possible
#' question (input) types include numeric, text, multiple choice, or selection.
#'
#' @param df A nested dataframe.
#'
#' @return UI Code for a Shiny App.
#' @export
#'
#' @examples
#'
#' getUICode(teaching_r_questions)
#'
getUICode <- function(df) {

  nested <- nestUniqueQuestions(df)

  purrr::map(nested$data, ~getUICode_individual(.x))

}
