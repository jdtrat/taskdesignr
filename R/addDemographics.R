
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
    dplyr::group_by(.data$question, dependence) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(question_number = dplyr::row_number(), .before = .data$question) %>%
    tidyr::unnest(.data$data) %>%
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
        inputId = base::unique(df$input_id),
        label = base::unique(df$question),
        choices = df$option,
        options = list(
          title = "Placeholder")
      )
  } else if (inputType == "numeric") {

    output <-
      shinyWidgets::numericInputIcon(
        inputId = base::unique(df$input_id),
        label = base::unique(df$question),
        value = df$option,
        icon = list(
          #make the df$input_id sentence case in base R
          base::paste0(base::toupper(base::substring(base::unique(df$input_id), 1,1)),
                       base::tolower(base::substring(base::unique(df$input_id), 2)))
        )
      )

  } else if (inputType == "mc") {

    output <-
      shiny::radioButtons(
        inputId = base::unique(df$input_id),
        label = base::unique(df$question),
        # selected = base::character(0),
        choices = df$option
      )
  } else if (inputType == "text") {

    output <-
      shiny::textInput(inputId = base::unique(df$input_id),
                label = base::unique(df$question),
                value = df$option)

  } else if (inputType == "y/n") {

    output <-
      shiny::radioButtons(
        inputId = base::unique(df$input_id),
        label = base::unique(df$question),
        # selected = base::character(0),
        selected = "No",
        choices = df$option
      )
  }

  if (!base::is.na(df$dependence[1])) {
    output <- shinyjs::hidden(output)
  }

  return(list(output))

}


#' Generate the UI Code for demoraphic questions
#'
#' Create the UI code for a Shiny app based on user-supplied questions. Possible
#' question (input) types include numeric, text, multiple choice, or selection.
#'
#' @param df A user supplied dataframe in the format of teaching_r_questions.
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



#' Show dependence questions
#'
#' @param df The output of \code{\link{nestUniqueQuestions}}.
#' @param input Input from server
#'
#' @return NA; shows a dependence question in the UI.
#'
showDependence <- function(input = input, df) {

  #shiny::req(input[[df$dependence[1]]])

  # if there is a dependence
  if (!base::is.na(df$dependence[1])) {
    # check that the input of that question's dependence
    # is equal to its dependence value. If so,
    # show the question.
    if (input[[df$dependence[1]]] == df$dependence_value[1]) {
      shinyjs::show(df$input_id[1])
    } else {
      shinyjs::hide(df$input_id[1])
    }
  }
}

#' Server code for adding survey questions
#'
#' Create the UI code for a Shiny app based on user-supplied questions. Possible
#' question (input) types include numeric, text, multiple choice, or selection.
#'
#' @param df A user supplied dataframe in the format of teaching_r_questions.
#' @param input Input from server
#'
#' @return NA; server code
#' @export
#'
getServerCode <- function(input, df) {

  nested <- nestUniqueQuestions(df)

  observe({
    purrr::walk(nested$data, ~showDependence(input = input, df = .x))
  })

}

