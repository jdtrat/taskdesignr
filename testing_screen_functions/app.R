text <- expand.grid(c("upper", "middle", "lower"), c("left", "center", "right")) %>%
    tidyr::unite(col = "position", sep = " ") %>%
    dplyr::mutate(text = position) %>%
    purrr::pmap(add_text, style = "color: #4d3a7d;") %>%
    purrr::map(function(x) {div(class = x$position,
                                x$ui)})

image <- expand.grid(c("upper", "middle", "lower"), c("left", "center", "right")) %>%
    tidyr::unite(col = "position", sep = " ") %>%
    dplyr::mutate(file = "test2.jpg") %>%
    purrr::pmap(add_image, height = 140, width = 140) %>%
    purrr::map(function(x) {div(class = x$position,
                                x$ui)})


slider <- expand.grid(c("upper", "middle", "lower"), c("left", "center", "right")) %>%
    tidyr::unite(col = "position", sep = " ") %>%
    purrr::pmap(add_slider,
                inputId = "hi",
                label = "Slider:",
                choices = c("very happy",
                            "meh",
                            "not happy")) %>%
    purrr::map(function(x) {div(class = x$position,
                                x$ui)})

test_slider <- add_slider(inputId = "hi",
                          label = "Slider:",
                          choices = c("very happy",
                                      "meh",
                                      "not happy"),
                          width = "50vw",
                          position = "upper center")

image1 <- add_image("test1.png", position = "left middle")
image2 <- add_image("test2.jpg", position = "right middle")

# ui <- define_screen_working(tagList(div(class = test_slider$position,
#                                 test_slider$ui),
#                             shiny::div(class = "upper left",
#                                shiny::textOutput("slider")))
#                             )

# ui <- define_screen_working(shiny::div(class = "center",
#                                        shiny::textOutput("slider"),
#                             test_slider$ui))




# ui <- define_screen_working(text)
#
# ui <- define_screen_working(
#     tagList(div(class = test_slider$position,
#                test_slider$ui),
#             div(class = image1$position,
#                 image1$ui),
#             div(class = image2$position,
#                 image2$ui))
# )

ui <- shiny::fillPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "testingCSS.css")
    ),
    div(class = "container",
        div(class = "middles",
            add_image("test1.png")$ui)
    )
)

server <- function(input, output, session) {

 #   output$slider <- renderText({input$hi})

}

# Run the application
shiny::shinyApp(ui = ui, server = server)



# Will need an ordering thing for stuff within the same class and then an example where vertical space can be added.
