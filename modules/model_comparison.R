box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[imgLabels,displayNames]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Model comparison"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("projectA"), "Project A:", displayNames),
      selectInput(ns("projectB"), "Project B:", displayNames),
      
      selectizeInput(
        ns("genename"),
        label = "Gene names (optional): ",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),

      submitButton("Update View"),
      
      width = 3
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      textOutput(ns("resultTitleComparison")),
      br(),
      plotOutput(ns("comparisonPlot"))
    )
  )
}

#' @export
server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  # Update gene selector with existing labels
  updateSelectizeInput(
    session,
    "genename",
    choices = geneLabels()$mouse_genes,
    selected = c(),
    server = TRUE
  )
  
  # Preprocess the data
  preprocComparisonsInput <- reactive({
    req(input$projectB)
    preprocComparisons(input$projectA, input$projectB, input$genename)
  })
  
  # Render title of counts plot
  output$resultTitleComparison <- renderText({
    req(input$projectB)
    sprintf('Comparison of significant fold changes between models %s and %s', names(displayNames)[match(input$projectA,displayNames)], names(displayNames)[match(input$projectB,displayNames)])
  })
  
  # Produce plot
  output$comparisonPlot <- renderPlot({
    req(input$projectB)
    preprocComparisonsInput()[['plotData']]
  },
  height = 750, width = 750)
}