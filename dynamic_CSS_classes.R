library(shiny)
library(tidyverse)
library(shinyalert)

ui <- fluidPage(
  shinyalert::useShinyalert(),
  wellPanel(actionButton("addClass", "Add a class"),
            downloadButton("downloadCSS", "Download CSS")),
  imageOutput("image",
              width = "85vw",
              height = "85vh",
              brush = brushOpts(id = "image_brush")))

server <- function(input, output, session) {

  # create a reactiveValues object to index into later
  ui_elements <- reactiveValues(numClasses = 0,
                                CSS_output = "")

  # Create a blank canvas for people to draw regions on and define UI elements
  output$image <- renderImage({
    width <- session$clientData$output_image_width
    height <- session$clientData$output_image_height

    outfile <- tempfile(fileext = ".png")
    magick::image_blank(width = width, height = height) %>%
    magick::image_border() %>%
    magick::image_write(path = outfile)

    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height)
  }, deleteFile = TRUE)

  # When the addClass input is hit, pop up a modal that allows you to customize
  # the CSS element and define the type of input.
  observeEvent(input$addClass, {

    ui_elements$numClasses <- ui_elements$numClasses + 1
    shinyalert(title = "Customize your class",
               html = TRUE,
               text = tagList(
                 selectInput("class_type",
                             "What type of UI Element will go here?",
                             choices = c("Text",
                                         "Image",
                                         "Slider"),
                             selected = character(0)),
                 conditionalPanel(
                   condition = "input.class_type == 'Text'",
                   selectInput("text_class", "What type of text UI is this?",
                               choices = c("h1",
                                           "h2",
                                           "h3",
                                           "h4",
                                           "h5",
                                           "h6",
                                           "p"))
                 ),
                 conditionalPanel(
                   condition = "input.class_type == 'Image'",
                   textInput("image_class_height", "Image Height:"),
                   textInput("image_class_width", "Image Width:")
                 ),
                 conditionalPanel(
                   condition = "input.class_type == 'Slider'",
                   textInput("slider_class_label", "Slider Label:"),
                   textInput("slider_class_choices", "Slider Choices:")
                 ),
                 textInput("class_name", "Provide a one word description (for CSS Class)"),
                 textInput("custom_css", "Provide any custom CSS arguments.")),
               inputId = "class_modal"
    )

    # create a dataframe with the xmin/xmax/ymin/ymax coords scaled relative to
    # the user's template image.
    ui_elements$coords <- data.frame(xmin = input$image_brush$xmin,
                                     xmax = input$image_brush$xmax,
                                     ymin = input$image_brush$ymin,
                                     ymax = input$image_brush$ymax) %>%
      dplyr::mutate(across(.cols = contains("xm"), ~ (.x / session$clientData$output_image_width) * 100),
                    across(.cols = contains("ym"), ~ (.x / session$clientData$output_image_height) * 100),
                    across(.cols = everything(), ~ round(.x)))

  })

  # when the modal is pressed, create the css output
  observeEvent(input$class_modal, {

    ui_elements$CSS_output <- c(ui_elements$CSS_output,
                    glue::glue("
           .[input$class_name] {

           grid-column-start: [ui_elements$coords$xmin];
           grid-column-end: [ui_elements$coords$xmax];
           grid-row-start: [ui_elements$coords$ymin];
           grid-row-end: [ui_elements$coords$ymax];
           [input$custom_css]

           }

           ",
           .open = "[",
           .close = "]"))

    output$downloadCSS <- downloadHandler(
      filename = function() {
        paste("custom-ui", ".css", sep="")
      },
      content = function(file) {
        writeLines(ui_elements$CSS_output, con = file)
      }
    )

  })


}

shiny::shinyApp(ui = ui, server = server)




