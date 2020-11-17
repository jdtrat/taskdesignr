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
      shiny::checkboxInput("dependency",
                           "This question has a dependency"),
      shinyjs::hidden(shiny::selectInput("question_dependence",
                                       "Which question does this depend on?",
                                       choices = "FILL THIS IN WITH THE QUESTIONS ADDED")),
      shinyjs::hidden(shiny::selectInput("question_dependence_value",
                                         label = "For what value of tha question should this one be shown?",
                                         choices = "FILL THIS IN WITH THE QUESTIONS ADDED")),
      shiny::checkboxInput("question_required",
                           "This question will be required."),
      shiny::actionButton("createQuestion",
                          "Create a Question")
    ),
    shiny::mainPanel(
      shiny::tableOutput("table"),
      #shiny::uiOutput("form_ui"),
      tags$div(id = "form_placeholder")
    )

  )

  server <- function(input, output, session) {


    form <- reactiveValues()


    # IF DEPENDENCE IS NOT NA IT WILL BE HIDDEN SO IT WILL "WORK" BUT NOT
    shiny::observe({

      form$question <- input$question_title
      form$option <- "25"
      form$input_type <- input$question_type
      form$input_id <- "q1"
      form$dependence <- input$question_dependence
      form$dependence_value <- input$question_dependence_value
      form$required <- input$question_required

      form$forms <- data.frame(question = form$question,
                               option = form$option,
                               input_type = base::tolower(form$input_type),
                               input_id = form$input_id,
                               dependence = NA,
                               dependence_value = NA,
                               required = form$required)

      if (input$dependency == TRUE) {
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

    output$table <- shiny::renderTable({


      form$forms

    })

    # output$form_ui <- shiny::renderUI({
    #
    #   print(Sys.time())
    #   taskdesignr::surveyOutput_individual(df = form$forms)
    #
    # })

    observeEvent(input$createQuestion,{

      ui <- taskdesignr::surveyOutput_individual(df = form$forms)
      insertUI(selector = "#form_placeholder",
               ui = ui)

    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
