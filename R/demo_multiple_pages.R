#' Demo Survey over multiple pages
#'
#' This function runs a Shiny app that shows an example of running a demographic survey over multiple pages in Shiny.
#' Code was inspired by Dean Attali's advanced-shiny Github Repo.
#'
#' It requires shiny, shinyjs, and shinyWidgets.
#'
#' @param questions A dataframe of questions of which to create a demographic form.
#'
#' @return A Shiny app
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' demo_multiple_pages(teaching_r_questions)
#' }
#'
demo_multiple_pages <- function(questions) {

  NUM_PAGES <- nrow(nestUniqueQuestions(questions))

  # I created a local directory
  outputDir <- "~/Downloads/"

  # function based on Dean Attali's post about persistent data storage
  # https://shiny.rstudio.com/articles/persistent-data-storage.html
  saveData <- function(data, userID) {

    # Create a unique file name
    fileName <- base::sprintf("%s_%s.csv", userID, base::as.integer(base::Sys.time()))

    # Write the file to the local system
    readr::write_csv(data,
                     path = base::file.path(outputDir, fileName))
  }

  demographicCSS <- ".required { color: red; }"

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    #Create hidden UI elements to show one by one:
    shinyjs::hidden(base::lapply(0:(NUM_PAGES + 1), function(i) {
      if (i < 1) {
        shiny::div(class = "question_page",
            id = paste0("question", i),
            shiny::textInput("userID", "Enter your username."))
      }else if (i >= 1 && i <= NUM_PAGES) {
        shiny::div(
          class = "question_page",
          id = paste0("question", i),
          getUICode_individual(tidyr::unnest(nestUniqueQuestions(questions)[i,], cols = c(.data$data)))
        )
      }else if (i == (NUM_PAGES + 1)) {
        shiny::div(class = "question_page",
            id = paste0("question", i),
            shinyWidgets::actionBttn("submit", "Submit"))
      }
    })
    ),
    shiny::br(),
    #always show action buttons
    shiny::actionButton("prevBtn", "< Previous"),
    shiny::actionButton("nextBtn", "Next >")
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(question_page = 0)



    shiny::observe({
      # code to switch the page after 500 milliseconds
      # shinyjs::delay(500, navPage(1))
      shinyjs::toggleState(id = "prevBtn", condition = rv$question_page >= 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv$question_page <= NUM_PAGES)
      shinyjs::hide(selector = ".question_page")
      shinyjs::show(paste0("question", rv$question_page))
    })

    navPage <- function(direction) {
      rv$question_page <- rv$question_page + direction
    }

    user_id <- shiny::reactive({paste0(input$userID)})

    formData <- shiny::reactive({
      data <- tibble::tribble(~userID, ~question, ~response,
                              input$userID, "age", as.character(input$age),
                              input$userID, "gender", input$gender,
                              input$userID, "education_attained", input$education_attained,
                              input$userID, "first_language", input$first_language,
                              input$userID, "read_language", input$read_language,
                              input$userID, "learned_r", input$learned_r,
                              input$userID, "learned_programming_not_r",input$learned_programming_not_r,
                              input$userID, "completed_data_analysis", input$completed_data_analysis
      )
      data
    })

    shiny::observeEvent(input$submit, {saveData(data = formData(), userID = user_id())})

    shiny::observeEvent(input$prevBtn, navPage(-1))
    shiny::observeEvent(input$nextBtn, navPage(1))

    # this does not work yet
    getServerCode(input = input, df = questions)

  }

  shiny::shinyApp(ui, server)

}
