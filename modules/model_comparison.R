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

      #submitButton("Update View"),
      
      width = 3
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      div(
        class = 'SmallTitleText',
        textOutput(ns("resultTitleComparison"))
      ),
      br(),
      uiOutput(ns("comparisonPlot_ui")),
      br(),
      conditionalPanel(
        condition = "output.noteDisplay == true",
        div(
          class = 'comparisonPlotNoteFrame',
          div(
            class = 'comparisonPlotNote',
            htmlOutput(ns('genelistNote'))
          )
        ),
        ns = ns
      )
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
  
  # Make dimensions for the plot
  plot_dimensions <- reactive({
    list(
      heigth = 750,
      width = 750
    )
  })
  
  # Produce plot
  output$comparisonPlot <- renderPlot({
    req(input$projectB)
    preprocComparisonsInput()[['plotData']]
  })
  
  # Wrap plot in ui for dynamism
  output$comparisonPlot_ui <- renderUI({
    plotOutput(session$ns("comparisonPlot"), height = plot_dimensions()$heigth, width = plot_dimensions()$width)
  })
  
  # Render informative note about the gene selection
  output$genelistNote <- renderText({
    req(input$genename)
    '<b>NOTE:</b> A selected gene that does not appear in the graphic might not be significant, or not be present in one of the selected experiments. Please, check them with the other available tools'
  })
  
  # Check if it has to display the note
  output$noteDisplay <- reactive({
    length(input$genename) > 0
  })
  outputOptions(session$output, "noteDisplay", suspendWhenHidden = FALSE)
}