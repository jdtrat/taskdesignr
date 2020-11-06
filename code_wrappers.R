
# CSS ---------------------------------------------------------------------


CSS_header <- "
.container {
  display: grid;
  width: 100vw;
  height: 100vh;
  grid-template-columns: repeat(100, 1fr);
  grid-template-rows: repeat(100, 1fr);
  border: 1px solid black;
  background-color: rgba(0, 0, 0, 0.05);
}
"


# R -----------------------------------------------------------------------


R_header <- "
library(shiny)
library(taskdesignr)
library(tidyverse)
library(shinyjs)

ui <- fillPage(
shinyjs::useShinyjs(),
div(class = \"container\",

"

R_footer <- "

)
# Remove the comma from the last element.
# Add any additional UI Elements As Needed
)

server <- function(input, output, session) {

# Fill in Shiny Server Logic As Needed

}


shiny::shinyApp(ui = ui, server = server)

"
