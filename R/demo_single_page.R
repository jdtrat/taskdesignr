#' Demo Survey over multiple pages
#'
#' This function runs a Shiny app that shows an example of running our demographic survey on one page in Shiny.
#'
#' It requires shiny, shinyjs, and shinyWidgets.
#'
#' @param questions A dataframe of questions of which to create a demographic form.
#'
#' @return A Shiny App
#' @export
#'
#' @examples
#'
#' \dontrun{
#' demo_single_page(teaching_r_questions)
#' }
demo_single_page <- function(questions) {

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



  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(
    getUICode(questions)
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {

    getServerCode(input = input, df = questions, session = session)

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


    shiny::observeEvent(input$submit, {saveData(data = formData(), userID = input$userID)})

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
