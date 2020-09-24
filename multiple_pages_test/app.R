library(shiny)
library(shinyjs)
library(shinyWidgets)

NUM_PAGES <- nrow(nestUniqueQuestions(teaching_r_questions))

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

ui <- fluidPage(
    useShinyjs(),
    hidden(lapply(0:(NUM_PAGES + 1), function(i) {
                if (i < 1) {
                    div(class = "question_page",
                        id = paste0("question", i),
                        textInput("userID", "Enter your username."))
                }else if (i >= 1 && i <= NUM_PAGES) {
                    div(
                        class = "question_page",
                        id = paste0("question", i),
                        getUICode_individual(tidyr::unnest(nestUniqueQuestions(teaching_r_questions)[i,], cols = c(data)))
                    )
                }else if (i == (NUM_PAGES + 1)) {
                    div(class = "question_page",
                        id = paste0("question", i),
                        actionBttn("submit", "Submit"))
                }
            })
    ),
    br(),
    actionButton("prevBtn", "< Previous"),
    actionButton("nextBtn", "Next >")
)

server <- function(input, output, session) {
    rv <- reactiveValues(question_page = 0)

    observe({
        toggleState(id = "prevBtn", condition = rv$question_page >= 1)
        toggleState(id = "nextBtn", condition = rv$question_page <= NUM_PAGES)
        hide(selector = ".question_page")
        show(paste0("question", rv$question_page))
    })

    navPage <- function(direction) {
        rv$question_page <- rv$question_page + direction
    }

    user_id <- reactive({paste0(input$userID)})

    formData <- reactive({
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

    observeEvent(input$submit, {saveData(data = formData(), userID = user_id())})

    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))

    # this does not work
    # getServerCode(teaching_r_questions)

}

shinyApp(ui, server)
