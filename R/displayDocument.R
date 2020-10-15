#' Convert a Word (docx) to a Markdown (md) file
#'
#' This function converts a word document to a markdown file. Please note that
#' the file name must not include any periods other than the extension, e.g.
#' 'file.name.docx' will not work but 'file_name.docx' will.
#'
#' @param file The path to your docx file.
#'
#' @return The same file as a Markdown document.
#' @export
#'
wordToMarkdown_old <- function(file) {

  # This extracts the file name before the . at the extension.
  fileName <- base::regmatches(file, base::regexpr("([^.]+)", file))

  # Generate terminal command for converting a word document to a markdown document.
  string <- base::paste0("pandoc -s ", file, " -t markdown -o ", fileName, ".md")

  # run the pandoc conversion
  base::system(string)

  # set the output file as the newly converted markdown document
  outputFile <- base::paste0(fileName, ".md")

  return(outputFile)

}

#' Convert a Word (docx) to a Markdown (md) file
#'
#' This function converts a word document to a markdown file.
#' If output path is NULL (as default), the function will save this to a temporary path.
#'
#' @param file The path to your .docx file.
#' @param output The path to save your new markdown file
#'
#' @return The same file as a Markdown document.
#' @export
#'
#' @examples
#'
#' #ADD LATER
#'
wordToMarkdown <- function(file, output = NULL) {


  if (base::is.null(output)) {
    outputFile <- base::tempfile(fileext = ".md")
  } else {
    outputFile <- output
  }

  rmarkdown::pandoc_convert(input = file, to = "markdown", output = outputFile)

  return(outputFile)

}

#' Display a document in Shiny.
#'
#' This function will take a Markdown document (.md) or Word document (.docx) and
#' return the UI for a Shiny app to display it. This should be embedded within a
#' UI code (see examples for details).
#'
#'@param file The file (path) containing a document to display.
#'
#'@return A Shiny UI with an informed consent document.
#'@export
#'
#' @examples
#'
#' \dontrun{
#' ui <- shiny::fluidPage(
#' shiny::mainPanel(
#' displayDocument('path-to-your-file')
#' )
#' )
#'}
displayDocument <- function(file) {

  if (base::grepl(".docx", file)) {
    markdownFile <- wordToMarkdown(file)
  } else if (base::grepl(".md", file)) {
    markdownFile <- file
  } else {
    stop("Invalid file type. Please add a Markdown .md or Word .docx file.")
  }

  shiny::includeMarkdown(markdownFile)

}


#' Demo for displaying a document in a Shiny app.
#'
#' This function provides a minimal example of using the
#' \code{\ref{displayDocument}} function within your Shiny application to
#' display a document.
#' @param file The file (path) containing a document to display. Can be .docx or .md file type.
#'
#' @return A Shiny App that displays a document.
#' @export
#'
#' @examples
#'
#' # FILL THIS OUT LATER
demo_displayDocument <- function(file) {

  ui <- shiny::fluidPage(
    shiny::mainPanel(
      displayDocument(file = file)
    )
  )

  server <- function(input, output, session) {

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}






