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
    "</br><b>Lead developer:</b></br>\tMiguel Gonzalez Acera</br>\tMiguel.GonzalezAcera@uk-erlangen.de</br></br><b>Project chief:</b></br>\tChristoph Becker</br>\tChristoph.becker@uk-erlangen.de</br></br><b>\tIMPORTANT</b>: As the tool is constantly being developed, some errors and bugs are expected as new features are added. If a bug or a mistake is found when consulting the information hosted, please contact us and communicate us the issue, and we will fix it as soon as possible."
  })
}