library(shiny)
library(shinyjs)
library(shinyalert)
library(rdrop2)
library(glue)
library(taskdesignr)

#establish SASS file
sass::sass(
    sass::sass_file("www/survey.scss"),
    output = "www/survey.css"
)


ui <- shiny::fillPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "survey.css")
    ),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    #shinyjs::extendShinyjs(text = js_code,
    #                       functions = 'openExperiment'),
    div(class = "grid",
        div(class = "input-pane",
            shiny::selectInput("question_type",
                               "What type of Question would you like to add?",
                               choices = c(
                                   "Select",
                                   "Numeric",
                                   "Multiple Choice",
                                   "Text",
                                   "Yes/No"
                               )
            ),
            tags$div(id = "option_placeholder"),
            actionButton("add_option", "Add an option"),
            helpText("This is the default value shown for numeric or text questions.",
                     "For Select, Multiple Choice, or Yes/No questions, these are the possible response options."),
            shiny::textInput(
                "question_title",
                "What is the question's title?"
            ),
            shiny::checkboxInput(
                "dependency",
                "This question has a dependency"
            ),
            shinyjs::hidden(shiny::selectInput("question_dependence",
                                               "Which question does this depend on?",
                                               choices = "FILL THIS IN WITH THE QUESTIONS ADDED"
            )),
            shinyjs::hidden(shiny::selectInput("question_dependence_value",
                                               label = "For what value of tha question should this one be shown?",
                                               choices = "FILL THIS IN WITH THE QUESTIONS ADDED"
            )),
            shiny::checkboxInput(
                "question_required",
                "This question will be required."
            ),
            shiny::actionButton(
                "create_question",
                "Create a Question"
            )
            ),
        div(class = "survey",
            shiny::textInput("survey_title",
                             "Survey Title",
                             "Untitled")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    taskdesignr::renderSurvey(input = input,
                              df = taskdesignr::teaching_r_questions,
                              session = session)
}

# Run the application
shinyApp(ui = ui, server = server)
