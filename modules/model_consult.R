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
      textOutput(ns("resultTitleCounts")),
      br(),
      uiOutput(ns("countsPlot_ui")),
      br(),
      textOutput(ns("resultTitle")),
      br(),
      tableOutput(ns("FCtable")),
      uiOutput(ns("downloadData_ui"))
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
  plot_dimensions <- reactive({
    list(
      heigth = max(300, ifelse(length(input$genename) %% 3 == 0, 300*(trunc(length(input$genename)/3)), 300*(1+trunc(length(input$genename)/3)))),
      width = ifelse(length(input$genename) <= 1, 300, ifelse(length(input$genename) <= 2, 600, 900))
    )
  })
  
  # Create and render barplot for counts
  output$countsPlot <- renderPlot({
    req(input$genename,input$project)
    if (input$project %in% names(fullExp)) {
      ggplot(preprocResultInput()[['countsData']]) +
        geom_line(aes(x=Treatment, y=CountsMean, group=1), color="red") +
        geom_point(aes(x=Treatment, y=CountsMean)) +
        facet_wrap(~Genename, scales="free_y", ncol=3) +
        geom_errorbar(aes(x=Treatment, ymin=CountsErrInf, ymax=CountsErrSup), width=0.4, colour="orange")
    } else if (input$project %in% names(singleExp)) {
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
    req(input$genename,input$project)
    sprintf('Fold change of the selected genes in %s model', names(displayNames)[match(input$project,displayNames)])
  })
  
  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename,input$project)
    preprocResultInput()[['foldChangeData']][c('Comparison','Genes','log2FoldChange','pvalue','padj')]
  })
  
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