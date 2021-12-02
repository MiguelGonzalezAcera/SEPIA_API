box::use(
  shiny[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[geneLabels,imgLabels,displayNames]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Gene query"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("project"), "Project:", displayNames),
      
      selectizeInput(
        ns("genename"),
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      
      submitButton("Update View"),
      
      width = 3
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      textOutput(ns("resultTitleCounts")),
      br(),
      uiOutput(ns("countsPlot_ui")),
      br(),
      textOutput(ns("resultTitle")),
      br(),
      tableOutput(ns("FCtable"))
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
    choices = geneLabels$mouse_genes,
    selected = c("S100a8","Vil1"),
    server = TRUE
  )
  
  # Preprocess the data
  preprocResultInput <- reactive({
    req(input$genename)
    preprocessing(input$project, input$genename)
  })
  
  # Render title of counts plot
  output$resultTitleCounts <- renderText({
    req(input$genename)
    sprintf('Counts of the selected genes in %s model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Make dimensions for the plot
  plot_dimensions <- reactive({
    list(
      heigth = max(300, ifelse(length(input$genename) %% 3 == 0, 300*(trunc(length(input$genename)/3)), 300*(1+trunc(length(input$genename)/3)))),
      width = ifelse(length(input$genename) <= 1, 300, ifelse(length(input$genename) <= 2, 600, 900))
    )
  })
  
  # Create and render barplot for counts
  output$countsPlot <- renderPlot({
    req(input$genename)
    preprocResultInput()[['plotData']]
  })
  
  # Wrap in ui for dynamism
  output$countsPlot_ui <- renderUI({
    plotOutput(session$ns("countsPlot"), height = plot_dimensions()$heigth, width = plot_dimensions()$width)
  })
  
  # Render the title
  output$resultTitle <- renderText({
    req(input$genename)
    sprintf('Fold change of the selected genes in %s model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename)
    preprocResultInput()[['foldChangeData']][c('Comparison','Genes','log2FoldChange','pvalue','padj')]
  })
}