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
    headerPanel("Contact us"),
    
    # Application sidebar
    mainPanel(
      div(
        htmlOutput(ns('Contacts'))
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$Contacts <- renderText({
    "</br><b>Lead developer:</b></br>\tMiguel Gonzalez Acera</br>\tMiguel.GonzalezAcera@uk-erlangen.de</br></br><b>Project chief:</b></br>\tChristoph Becker</br>\tChristoph.becker@uk-erlangen.de"
  })
}