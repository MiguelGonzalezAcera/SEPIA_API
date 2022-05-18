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
    "</br><b>Lead developer:</b></br>\tMiguel Gonzalez Acera</br>\tMiguel.GonzalezAcera@uk-erlangen.de</br></br><b>Project chief:</b></br>\tChristoph Becker</br>\tChristoph.becker@uk-erlangen.de</br></br><b>\tIMPORTANT</b>: As the tool is still in development, most of the used datasets are still unpublished. If you want to use the information that you obtain from this tool in an official presentation, poster or publication, permissions must be agreed and granted explicitely by us and/or the workgroup that owns the data. Please, it is imperative that you contact us so we can discuss the conditions and the porper credit for you to use the data. For internal TRR241 businessess, such as progress reports or webinars, you can use the data freely."
  })
}