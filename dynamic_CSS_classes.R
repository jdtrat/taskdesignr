library(shiny)
library(tidyverse)
library(shinyalert)
source('code_wrappers.R')

update_ui_thumnail <- function(base_image) {



  output <- magick::image_composite(base_image,
                                    new_image,
                                    operator = "SrcOver")

  return(output)

}


ui <- fluidPage(
  shinyalert::useShinyalert(),
  wellPanel(actionButton("addClass", "Add a class"),
            downloadButton("downloadCSS", "Download CSS"),
            downloadButton("downloadShiny", "Download Shiny code")),
  imageOutput("image",
              width = "85vw",
              height = "85vh",
              brush = brushOpts(id = "image_brush")))

server <- function(input, output, session) {

  # create a reactive values object to index into later
  ui_elements <- reactiveValues(R_body = "",
                                CSS_body = "")

  base_image <- reactive({
    magick::image_blank(width = session$clientData$output_image_width,
                        height = session$clientData$output_image_height) %>%
      magick::image_border(geometry = "10x10")
  })


  # Create a function that takes in ui elements from shiny modal and returns the
  # appropriate R code using taskdesignr
  get_R_body <- reactive({
    switch(input$ui_type,
           "Text" = glue::glue("add_text(text = \"[input$text_value]\",
                           tag_type = \"[input$text_class]\",
                           class = \"[input$class_name]\")$ui,",
                           .open = "[",
                           .close = "]"),
           "Image" = glue::glue("add_image(file = \"[input$image_path]\",
                            class = \"[input$class_name]\",
                            width = \"[input$image_class_width]\",
                            height = \"[input$image_class_height]\")$ui,",
                            .open = "[",
                            .close = "]"),
           "Slider" = glue::glue("add_slider()",
                                 .open = "[",
                                 .close = "]")
    )
  })

  # Create a blank canvas for people to draw regions on and define UI elements
  output$image <- renderImage({

    outfile <- tempfile(fileext = ".png")
    outImage <- base_image()

    # observeEvent(input$class_modal, {
    #
    if (!is.null(input$image_brush)) {
      new_image <- magick::image_blank(width = (input$image_brush$xmax - input$image_brush$xmin),
                                       height = (input$image_brush$ymax - input$image_brush$ymin),
                                       color = "#d4ebf2") %>%
        magick::image_border(color = "blue",
                             geometry = "2x2")

      outImage <- magick::image_composite(outImage,
                                          new_image,
                                          offset = paste0("+", input$image_brush$xmin, "+", input$image_brush$ymin),
                                          operator = "SrcOver")
    }

    #
    # })

    magick::image_write(image = outImage,
                        path = outfile)

    list(src = outfile,
         contentType = "image/png",
         width = session$clientData$output_image_width,
         height = session$clientData$output_image_height)
  }, deleteFile = TRUE)

  # When the addClass input is hit, pop up a modal that allows you to customize
  # the CSS element and define the type of input.
  observeEvent(input$addClass, {

    shinyalert(title = "Customize your class",
               html = TRUE,
               text = tagList(
                 selectInput("ui_type",
                             "What type of UI Element will go here?",
                             choices = c("Text",
                                         "Image",
                                         "Slider"),
                             selected = character(0)),
                 conditionalPanel(
                   condition = "input.ui_type == 'Text'",
                   textInput("text_value", "What text do you want to display?"),
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
                   condition = "input.ui_type == 'Image'",
                   textInput("image_path", "Path to Image:"),
                   textInput("image_class_height", "Image Height:"),
                   textInput("image_class_width", "Image Width:")
                 ),
                 conditionalPanel(
                   condition = "input.ui_type == 'Slider'",
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

    ui_elements$CSS_body <- c(ui_elements$CSS_body,
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

    # combine the R
    ui_elements$R_body <- c(ui_elements$R_body,
                            get_R_body())

    # combine the CSS header with the user added CSS body
    CSS_file <- c(CSS_header,
                  ui_elements$CSS_body)


    # combine the R header and footer with user added body
    R_file <- c(R_header,
                ui_elements$R_body,
                R_footer)

    # download CSS file
    output$downloadCSS <- downloadHandler(
      filename = function() {
        paste("custom-ui", ".css", sep="")
      },
      content = function(file) {
        writeLines(CSS_file, con = file)
      }
    )


    # download .R file
    output$downloadShiny <- downloadHandler(
      filename = function() {
        paste("custom-ui", ".R", sep="")
      },
      content = function(file) {
        writeLines(R_file, con = file)
      }
    )

  })

}

shiny::shinyApp(ui = ui, server = server)




