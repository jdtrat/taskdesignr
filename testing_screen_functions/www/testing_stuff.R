



tagList(shiny::h1("hi", style = "color: #4d3a7d"), syle = "color: #4d3a7d;")


tagList(
  fluidRow(
  column(4,
         offset = 4,
         add_text("Hello!!!", tag_type = "h2", style = "color: #4d3a7d;")
         )
  )
  )


shiny::column(width = 4,
              offset = 4)

ui <- fluidPage(
  place_horizontal(add_text("abc", position = "upper-left"))
)



LCR <- function() {

ui_leftCenterRight <- define_screen(
  add_text("Hello!!!", tag_type = "h2", position = "upper-left", style = "color: #4d3a7d;"),
  add_text("I've been wondering", position = "upper-center", style = "color: #ad1d28;"),
  add_text("It's Me!!!", tag_type = "h2", position = "upper-right", style = "color: #48ca3b;")
)

shiny::shinyApp(ui = ui_leftCenterRight, server = function(input, output) { })
}

LRC <- function() {
ui_leftRightCenter <- define_screen(
  add_text("Hello!!!", tag_type = "h2", position = "upper-left", style = "color: #4d3a7d;"),
  add_text("It's Me!!!", tag_type = "h2", position = "upper-right", style = "color: #48ca3b;"),
  add_text("I've been wondering", position = "upper-center", style = "color: #ad1d28;")
)

shiny::shinyApp(ui = ui_leftRightCenter, server = function(input, output) { })
}


list(add_text("Hello!!!", tag_type = "h2", position = "upper-left", style = "color: #4d3a7d;"),
     add_text("I've been wondering", position = "upper-center", style = "color: #ad1d28;"),)




ui <- shiny::fluidPage(
shiny::div(class = "row align-items-start", tagList(place_horizontal(add_text("Hello!!!", tag_type = "h2", position = "upper-left", style = "color: #4d3a7d;")),
                                                    place_horizontal(add_text("It's me!!!", tag_type = "h2", position = "upper-right", style = "color: #4d3a7d;")))))
,
shiny::div(class = "row align-items-start", place_horizontal(add_text("It's me!!!", tag_type = "h2", position = "upper-right", style = "color: #4d3a7d;"))))



ui <- fillPage(flex = c(1,1,1),
  fillRow(
          flex = c(1,1,1),
          fillCol(wellPanel(p("upper-left"))),
          fillCol(wellPanel(p("upper-middle"))),
          fillCol(wellPanel(p("upper-rightt")))
  ),
  fillRow(
          wellPanel(p("hi"))
  ),
  fillRow(
          wellPanel(p("hi"))
  )
)


shiny::shinyApp(ui = ui, server = function(input, output) {})




ui <- define_screen(
  tagList(
      column(4,
             offset = 4,
             add_text("Hello!!!", tag_type = "h2", style = "color: #4d3a7d;")
      )
  )
)

ui <- define_screen(
  shiny::(shiny::strong("How do you feel about the last outcome?"),
          style = "font-weight: 500; line-height: 1.1; font-size: 20px;
        color: #4d3a7d;"))
)

ui <- define_screen(
  title = "Hello Shiny!",
  shiny::verticalLayout(
    br(),
    shiny::fluidRow(
      shiny::column(width = 1, offset = 4,
                    "How do you feel about the last outcome?"
      )
    ),
    br(),
    br(),
    shiny::fluidRow(
      shiny::column(width = 4, offset = 4,
                    a(href="http://example.com/link1", "Link One"),
      )
    )
  ),
  a(href="http://example.com/link2", "Link Two"),
  a(href="http://example.com/link3", "Link Three"),
  shiny::fluidRow(
    shiny::column(width = 4,
                  "How do you feel about the last outcome?"
    )
  )
)

ui <- fluidPage(
  fluidRow(
    column(12,
           "Fluid 12",
           fluidRow(
             column(6,
                    "Fluid 6",
                    fluidRow(
                      column(6,
                             "Fluid 6"),
                      column(6,
                             "Fluid 6")
                    )
             ),
             column(width = 6,
                    "Fluid 6")
           )
    )
  )
)



ui <- fluidPage(
  fluidRow(wellPanel("hello")),
  fluidRow(wellPanel(p("it's me", style = " grid-area: 1 / col4-start / last-line / 6;")))
)


ui <- define_screen(
  shiny::fluidRow(class = "row align-items-end",
                  shiny::column(4,
                                wellPanel(
                                  "You can't asdf the motion of the ocean")),
                  shiny::fluidRow(class = "row align-items-center",
                                  shiny::column(4,
                                                wellPanel(
                                                  "You can't stop the motion of the ocean"))
                  )

                  #                   )
                  # shiny::verticalLayout(
                  #   shiny::column(4,
                  #                 wellPanel(
                  #                   "You can't stop the motion of the ocean")),
                  #   shiny::column(4, offset = 4,
                  #                 wellPanel("on a Saturday night")),
                  #   shiny::column(4,
                  #                 wellPanel(
                  #                   "You can't stop the motion of the ocean")),
                  #   shiny::column(4, offset = 4,
                  #                 wellPanel("on a Saturday night")),
                  #   shiny::column(4,
                  #                 wellPanel(
                  #                   "You can't stop the motion of the ocean")),
                  #   shiny::column(4, offset = 4,
                  #                 wellPanel("on a Saturday night")),
                  #   shiny::column(4,
                  #                 wellPanel(
                  #                   "You can't stop the motion of the ocean")),
                  #   shiny::column(4, offset = 4,
                  #                 wellPanel("on a Saturday night")),
                  #   shiny::column(4,
                  #                 wellPanel(
                  #                   "You can't stop the motion of the ocean")),
                  #   shiny::column(4, offset = 8,
                  #                 wellPanel("on a Saturday night")),
                  #   shiny::column(4,
                  #                 wellPanel(
                  #                   "You can't stop the motion of the ocean")),
                  #   shiny::column(4, offset = 8,
                  #                 wellPanel("on a Saturday night"))
  )
)

shiny::shinyApp(ui, server = function(input, output) { })



base::expand.grid(vertical = c("upper", "middle", "lower"),
                  horizontal = c("left", "center", "right")) %>%
  tidyr::unite(col = "position", sep = "-") %>%
  dplyr::pull(position)




base::switch(vertical, horizontal,
)







place_horizontal(add_text("Hello, World.", tag_type = "h3", style = "color: #4d3a7d;"))
