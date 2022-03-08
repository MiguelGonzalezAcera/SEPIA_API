box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[imgLabels,displayNames,modelDescriptions]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("SEpIa models"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("project"), "Project:", displayNames),
      
      #submitButton("Update View"),
      
      width = 3
    ),
    
    mainPanel(
      textOutput(ns("introdTitle")),
      br(),
      htmlOutput(ns("introdDesc")),
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$introdTitle <- renderText({
    sprintf('A brief descriotion of the %s mouse model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Description
  output$introdDesc <- renderUI({
    HTML(modelDescriptions[[input$project]])
  })
}