box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[imgLabels,displayNames,modelDescriptions]
)

#' @export
ui <- function(id) {
  # Basic shiny doesn't do modules, so one needs to call this and use it in
  # every single object tag for them to work
  ns <- NS(id)
  div(
    # Application title
    headerPanel("SEpIa models"),
    
    # Application sidebar
    sidebarPanel(
      # Sidebar content
      selectInput(ns("project"), "Project:", displayNames),
      # Sidebar size
      width = 3
    ),
    
    mainPanel(
      div(
        class = 'introTitle',
        # title of the section, with the name of the model
        textOutput(ns("introdTitle"))
      ),
      br(),
      div(
        class = 'introText',
        # Description of the selected model
        htmlOutput(ns("introdDesc"))
      ),
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$introdTitle <- renderText({
    sprintf('%s mouse model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Description, rendered in html format
  output$introdDesc <- renderUI({
    HTML(modelDescriptions[[input$project]])
  })
}