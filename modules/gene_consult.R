box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[geneLabels]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    titlePanel(
      title = div(
        img(src = "logo1.png", height = 100),
        br(),
        "SEpIa"
      )
    ),
    
    # Application title
    headerPanel("Gene query"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("project"), "Project:",
                  list("Acute DSS" = "AcDSS",
                       "Chronic DSS" = "cDSS",
                       "Oxazolone colitis" = "OxC",
                       "T cell transfer colitis" = "TC",
                       "TNF-dARE colitis" = "TdAc",
                       "TNF-dARE ileitis" = "TdAi",
                       "Acute TNBS colitis" = "AcTNBS",
                       "Chronic TNBS colitis" = "cTNBS",
                       "Casp8 KO colitis" = "C8KOc",
                       "DSS time course" = "DSSTC", 
                       "Wound Healing" = "WH",
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
      
      checkboxInput(ns("timecourse"), "Is Time Course", FALSE),
      
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
    sprintf('Counts of the selected genes in %s model', input$project)
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
    if (input$timecourse) {
      ggplot(preprocResultInput()[['countsData']]) +
        geom_line(aes(x=Treatment, y=CountsMean, group=1), color="red") +
        geom_point(aes(x=Treatment, y=CountsMean)) +
        facet_wrap(~Genename, scales="free_y", ncol=3) +
        geom_errorbar(aes(x=Treatment, ymin=CountsErrInf, ymax=CountsErrSup), width=0.4, colour="orange")
    }else{
      ggplot(preprocResultInput()[['countsData']], aes(x=Treatment, y=Counts))+
        geom_boxplot()+
        facet_wrap(~Genename, scales="free_y", ncol=3)
    }
  })
  
  # Wrap in ui for dynamism
  output$countsPlot_ui <- renderUI({
    plotOutput(session$ns("countsPlot"), height = plot_dimensions()$heigth, width = plot_dimensions()$width)
  })
  
  # Render the title
  output$resultTitle <- renderText({
    req(input$genename)
    sprintf('Fold change of the selected genes in %s model', input$project)
  })
  
  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename)
    preprocResultInput()[['foldChangeData']][c('Comparison','Genes','log2FoldChange','pvalue','padj')]
  })
}