box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[contactDesc]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Contact us"),
    
    # Application sidebar
    mainPanel(
      div(
        class = 'introText',
        htmlOutput(ns('Contacts'))
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$Contacts <- renderUI({
    HTML(contactDesc)
  })
}