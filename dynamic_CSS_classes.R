library(shiny)
library(tidyverse)

ui <- fluidPage(
  wellPanel(actionButton("addClass", "Add a class")),
  imageOutput("image",
              width = "85vw",
              height = "85vh",
              brush = brushOpts(id = "image_brush")))

server <- function(input, output, session) {

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

  observeEvent(input$addClass, {

    download <- data.frame(xmin = input$image_brush$xmin,
               xmax = input$image_brush$xmax,
               ymin = input$image_brush$ymin,
               ymax = input$image_brush$ymax) %>%
      dplyr::mutate(across(.cols = contains("xm"), ~ (.x / session$clientData$output_image_width) * 100),
                    across(.cols = contains("ym"), ~ (.x / session$clientData$output_image_height) * 100),
                    across(.cols = everything(), ~ round(.x)))

    print(download)

    # write.csv(download, file = paste0("CSS_COORDS", Sys.time(), ".csv"))

  })
}

shiny::shinyApp(ui = ui, server = server)




