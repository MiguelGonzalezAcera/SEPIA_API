box::use(
  shiny[...],
  ggplot2[...],
  ggpubr[...],
  openxlsx[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[fullExp,singleExp,imgLabels,displayNames]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Application title
    headerPanel("Model query"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("project"), "Project:", displayNames),
      
      selectizeInput(
        ns("genename"),
        label = "Gene names: ",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      
      #submitButton("Update View"),
      
      width = 3
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      conditionalPanel(
        condition = "output.errorDispl == true",
        tags$div(
          class = 'errorFrame',
          p('The following genes were not found in the selected experiments:'),
          br(),
          tableOutput(ns("errorTable"))
        ),
        ns = ns
      ),
      br(),
      conditionalPanel(
        condition = "output.plotDisplay == true",
        div(
          textOutput(ns("resultTitleCounts")),
          br(),
          uiOutput(ns("countsPlot_ui")),
          br(),
          textOutput(ns("resultTitle")),
          br(),
          tableOutput(ns("FCtable")),
          uiOutput(ns("downloadData_ui"))
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
    selected = c("S100a8","Vil1"),
    server = TRUE
  )
  
  # Preprocess the data
  preprocResultInput <- reactive({
    req(input$genename,input$project)
    preprocessing(c(input$project), input$genename)
  })
  
  # Render title of counts plot
  output$resultTitleCounts <- renderText({
    req(input$genename,input$project)
    sprintf('Counts of the selected genes in %s model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Make dimensions for the plot
  # NOTE: I changed this from the length of the input to the length of the genes that come back, to avoid the wide ass graphs
  plot_dimensions <- reactive({
    list(
      heigth = max(300, ifelse(length(unique(preprocResultInput()[['countsData']][['Genename']])) %% 3 == 0, 300*(trunc(length(unique(preprocResultInput()[['countsData']][['Genename']]))/3)), 300*(1+trunc(length(unique(preprocResultInput()[['countsData']][['Genename']]))/3)))),
      width = ifelse(length(unique(preprocResultInput()[['countsData']][['Genename']])) <= 1, 300, ifelse(length(unique(preprocResultInput()[['countsData']][['Genename']])) <= 2, 600, 900))
    )
  })
  
  # Create and render barplot for counts
  output$countsPlot <- renderPlot({
    req(input$genename,input$project)
    ggplot(preprocResultInput()[['countsData']], aes(x=Treatment, y=Counts))+
      geom_boxplot()+
      facet_wrap(~Genename, scales="free_y", ncol=3)
  })
  
  # Wrap in ui for dynamism
  output$countsPlot_ui <- renderUI({
    plotOutput(session$ns("countsPlot"), height = plot_dimensions()$heigth, width = plot_dimensions()$width)
  })
  
  # Render the title
  output$resultTitle <- renderText({
    req(input$genename,input$project)
    sprintf('Fold change of the selected genes in %s model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename,input$project)
    preprocResultInput()[['foldChangeData']][c('Comparison','Genes','log2FoldChange','pvalue','padj')]
  })
  
  # Check if it has to display the plots
  output$plotDisplay <- reactive({
    preprocResultInput()[['plotDispl']]
  })
  outputOptions(session$output, "plotDisplay", suspendWhenHidden = FALSE)
  
  # Render error table
  output$errorTable <- renderTable({
    preprocResultInput()[['errorData']]
  })
  
  # Check if it has to display the error box
  output$errorDispl <- reactive({
    preprocResultInput()[['errDispl']]
  })
  outputOptions(session$output, "errorDispl", suspendWhenHidden = FALSE)
  
  # render the button for download
  output$downloadData_ui <- renderUI({
    req(input$genename,input$project)
    downloadButton(session$ns("downloadData"), 'Download')
  })
  
  # Make downloadeable table in excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'FoldChange.xlsx'), collapse = "_")
    },
    content = function(file) {
      write.xlsx(preprocResultInput()[['foldChangeData']][c('Comparison','Genes','log2FoldChange','pvalue','padj')], file)
    }
  )
}