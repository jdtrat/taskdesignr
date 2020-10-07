#' Display an informed consent document.
#'
#' @param file The file (path) containing the informed consent document.
#' @param type The file type -- either .docx or PDF.
#'
#' @return A Shiny UI with an informed consent document.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' addInformedConsent(file = 'InformedConsent.docx')
#'}
addInformedConsent <- function(file, type) {

  ui <- shiny::fluidPage(
    shiny::mainPanel(
    purrr::map(textreadr::read_docx(file), ~shiny::tags$p(.x)))
  )



  server <- function(input, output) {

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}



