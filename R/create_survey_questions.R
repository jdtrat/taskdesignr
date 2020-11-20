#' Create Survey Questions
#'
#' This function runs a Shiny app that allows the user to create survey
#' questions for use with the taskdesignr package.
#'
#' @export
#'
create_survey_questions <- function() {
  form_question_ui <- function(form) {
    div(
      class = "binder",
      id = form$input_id,
      div(
        class = "relative",
        taskdesignr::surveyOutput_individual(form)
      ),
      div(
        class = "absolute",
        actionButton(
          inputId = paste0(form$input_id, "remove", sep = "_"),
          label = "Remove",
          icon = icon("trash")
        ),
        shinyWidgets::switchInput(
          inputId = paste0(form$input_id, "required", sep = "_"),
          label = "Required",
          labelWidth = "60px",
          size = "mini",
        )
      )
    )
  }


flex_form_question_ui <- function(question_number) {
  shiny::tagList(shiny::fluidRow(
    column(
      width = 4,
      offset = 0,
      shiny::textInput(paste0("question_", question_number, "_title"),
                       "",
                       "Untitled Question",
                       width = "600px"),
    ),
    column(width = 2,
          offset = 0.5,
          shiny::selectInput(paste0("question_", question_number, "_type"),
                             "",
                             choices = c(
                               "Select",
                               "Numeric",
                               "Multiple Choice",
                               "Text",
                               "Yes/No"
                             )
          )
          ),
    tags$div(id = paste0("option_placeholder_", question_number)),
    ),
  shiny::fluidRow(
    column(
      width = 1,
      offset = 2,
      actionButton(
        inputId = paste0("question_", question_number, "_title_", "remove"),
        label = "Remove",
        icon = icon("trash")
      )
    ),
    column(width = 1,
      offset = 2.9,
      shinyWidgets::switchInput(
        inputId = paste0("question_", question_number, "_title_", "remove"),
        label = "Required",
        labelWidth = "60px"
      )
    )
  ),
  tags$div(id = paste0("question_", c(question_number + 1)))
  )
}

  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(
    tags$head(
      tags$style(HTML("

div.binder_flex {
  display: flex;
}



div.binder {
  position: relative;
}
div.relative {
  position: relative;
  width: 400px;
  height: 200px;
}

div.absolute {
  position: absolute;
  top: 100px;
  left: 150px;
  width: 200px;
  height: 100px;
}

    ")
    ),
    shinyjs::useShinyjs(),
    shiny::sidebarPanel(
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
        # tags$div(id = "option_placeholder"),
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
    shiny::mainPanel(
      shiny::tableOutput("table"),
      #default question
      flex_form_question_ui(question_number = 1)
    )
  )
)
  server <- function(input, output, session) {
    # Have a question already present
    form <- reactiveValues(num_questions = 1)


    # IF DEPENDENCE IS NOT NA IT WILL BE HIDDEN SO IT WILL "WORK" BUT NOT
    shiny::observe({

      form$question <- input[[paste0("question_", form$num_questions, "_title")]]
      form$option <- "25"
      form$input_type <- input$question_type
      form$input_id <- janitor::make_clean_names(input$question_title)
      form$dependence <- input$question_dependence
      form$dependence_value <- input$question_dependence_value
      form$required <- input$question_required

      form[[paste0("question_", form$num_questions)]] <- data.frame(
        question = form$question,
        option = form$option,
        input_type = base::tolower(form$input_type),
        input_id = form$input_id,
        dependence = NA,
        dependence_value = NA,
        required = form$required
      )

      if (input$dependency == TRUE) {
        shinyjs::show(id = "question_dependence")
        shiny::updateSelectInput(session, "question_dependence", choices = "question")
        shinyjs::show(id = "question_dependence_value")
        shiny::updateSelectInput(session, "question_dependence_value",
          label = paste0("For what value of ", input$question_dependence, " should this question appear?")
        )
      } else {
        shinyjs::hide(id = "question_dependence")
        shinyjs::hide(id = "question_dependence_value")
      }
    })

    output$table <- shiny::renderTable({
      form[[paste0("question_", form$num_questions)]]
    })

    # observeEvent(input$create_question, {
    #   form$num_questions <-  form$num_questions + 1
    #   # insertUI(
    #   #   selector = "#form_placeholder",
    #   #   ui = flex_form_question_ui(form$num_questions)
    #   # )
    # })

    observeEvent(input$add_option, {

      insertUI(selector = paste0("#option_placeholder_", input$create_question),
              ui = textInput(inputId = paste0("option", input$add_option),
                             label = paste0("Option", input$add_option),
                             value = "Placeholder"))

      observe({print(input)})
    })

    observe({print(input)})

    observeEvent(input$create_question, {
      form$num_questions <-  form$num_questions + 1
      form$questions <- tagList(form$questions, flex_form_question_ui(form$num_questions))
        insertUI(
          selector = paste0("#question_", form$num_questions),
          ui = flex_form_question_ui(form$num_questions)
        )
    })


  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
