box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[geneLabels,imgLabels]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Model comparison"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("projectA"), "Project A:",
                  list("Acute DSS" = "AcDSS",
                       "Chronic DSS" = "cDSS",
                       "Oxazolone colitis" = "OxC",
                       "T cell transfer colitis" = "TC",
                       "TNF-dARE colitis" = "TdAc",
                       "TNF-dARE ileitis" = "TdAi",
                       "Acute TNBS colitis" = "AcTNBS",
                       "Chronic TNBS colitis" = "cTNBS",
                       "Casp8 KO colitis" = "C8KOc",
                       "Eimeria vermiformis infection" = "EvInf",
                       "Helicobacter colitis infection" = "HhInf"
                  )
      ),
      selectInput(ns("projectB"), "Project B:",
                  list("Acute DSS" = "AcDSS",
                       "Chronic DSS" = "cDSS",
                       "Oxazolone colitis" = "OxC",
                       "T cell transfer colitis" = "TC",
                       "TNF-dARE colitis" = "TdAc",
                       "TNF-dARE ileitis" = "TdAi",
                       "Acute TNBS colitis" = "AcTNBS",
                       "Chronic TNBS colitis" = "cTNBS",
                       "Casp8 KO colitis" = "C8KOc",
                       "Eimeria vermiformis infection" = "EvInf",
                       "Helicobacter colitis infection" = "HhInf"
                  )
      ),
      
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
    choices = geneLabels$mouse_genes,
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
    sprintf('Comparison of significant fold changes between models %s and %s', input$projectA, input$projectB)
  })
  
  # Produce plot
  output$comparisonPlot <- renderPlot({
    req(input$projectB)
    preprocComparisonsInput()[['plotData']]
  },
  height = 1000, width = 1000)
}