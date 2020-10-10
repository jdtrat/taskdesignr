#' Create Survey Questions
#'
#' This function runs a Shiny app that allows the user to create survey
#' questions for use with the taskdesignr package.
#'
#' @export
#'
create_survey_questions <- function() {


  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::sidebarPanel(
      shiny::selectInput("question_type",
                         "What type of Question would you like to add?",
                         choices = c("Select",
                                     "Numeric",
                                     "Multiple Choice",
                                     "Text",
                                     "Yes/No")),
      shiny::textInput("question_title",
                       "What is the question's title?"),
      shiny::checkboxInput("question_required",
                           "This question will be required."),
      shinyjs::hidden(shiny::selectInput("question_dependence",
                                       "Which question does this depend on?",
                                       choices = "FILL THIS IN WITH THE QUESTIONS ADDED")),
      shinyjs::hidden(shiny::selectInput("question_dependence_value",
                                         label = "For what value of tha question should this one be shown?",
                                         choices = "FILL THIS IN WITH THE QUESTIONS ADDED")),
      shiny::actionButton("createQuestion",
                          "Create a Question")
    ),
    shiny::mainPanel(
      tableOutput("table")
    )

  )

  server <- function(input, output, session) {

    observe({
      if (input$question_required == TRUE) {
        shinyjs::show(id = "question_dependence")
        shiny::updateSelectInput(session, "question_dependence", choices = "question")
        shinyjs::show(id = "question_dependence_value")
        shiny::updateSelectInput(session, "question_dependence_value",
                                 label = paste0("For what value of ", input$question_dependence, " should this question appear?"))
      } else {
        shinyjs::hide(id = "question_dependence")
        shinyjs::hide(id = "question_dependence_value")
      }
    })


    output$table <- renderTable({

      data.frame(question = input$question_title,
                 option = "FILL LATER",
                 input_type = input$question_type,
                 input_id = "q1",
                 dependence = input$question_dependence,
                 dependence_value = input$question_dependence_value,
                 required = input$question_required)
    })


  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
