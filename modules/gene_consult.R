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
  
  theme = "main.css"
  
  div(
    # Application title
    headerPanel("Gene query"),
    
    # Application sidebar
    sidebarPanel(
      selectizeInput(
        ns("genename"),
        label = "Gene name: ",
        choices = NULL,
        multiple = FALSE,
        width = "100%"
      ),
      
      selectizeInput(
        ns("project"),
        label = "Project:",
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
          br(),
          div(
            class = 'errorText',
            p('The following genes were not found in the selected experiments:'),
            br(),
            tableOutput(ns("errorTable"))
          )
        ),
        ns = ns
      ),
      br(),
      conditionalPanel(
        condition = "output.plotDisplay == true",
        div(
          div(
            class = 'SmallTitleText',
            textOutput(ns("resultTitleCounts"))
          ),
          br(),
          uiOutput(ns("countsPlot_ui")),
          br(),
          br(),
          div(
            class = 'SmallTitleText',
            textOutput(ns("resultTitle"))
          ),
          br(),
          tableOutput(ns("FCtable")),
          downloadButton(ns("downloadData"), 'Download Table', class = 'DLButton'),
          downloadButton(ns("downloadPlot"), 'Download Graphic', class = 'DLButton')
        ),
        ns = ns
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Update gene selector with existing labels
  updateSelectizeInput(
    session,
    "genename",
    choices = geneLabels()$mouse_genes,
    selected = c('S100a8'),
    server = TRUE
  )
  
  # Update the selection for the projects
  updateSelectizeInput(
    session,
    "project",
    choices = displayNames,
    selected = c("AcDSS", "cDSS"),
    server = TRUE
  )
  
  # Preprocess the data
  preprocResultInput <- reactive({
    req(input$genename,input$project)
    preprocessing(input$project, input$genename)
  })
  
  # Render title of counts plot
  output$resultTitleCounts <- renderText({
    req(input$genename,input$project)
    sprintf('Counts of %s in the selected models', input$genename)
  })
  
  # Make dimensions for the plot
  # NOTE: I changed this from the length of the input to the length of the genes that come back, to avoid the wide ass graphs
  plot_dimensions <- reactive({
    list(
      height = max(300, ifelse(
        length(unique(preprocResultInput()[['foldChangeData']][['ModelName']])) %% 3 == 0,
        300*(trunc(length(unique(preprocResultInput()[['foldChangeData']][['ModelName']]))/3)),
        300*(1+trunc(length(unique(preprocResultInput()[['foldChangeData']][['ModelName']]))/3)))
      ),
      width = ifelse(length(unique(preprocResultInput()[['foldChangeData']][['ModelName']])) <= 1, 300, ifelse(length(unique(preprocResultInput()[['foldChangeData']][['ModelName']])) <= 2, 600, 900))
    )
  })
  
  # Create and render barplot for counts
  output$countsPlot <- renderPlot({
    req(input$genename,input$project)
    # 'You could do it with facet_wrap' If you can tell me how to make the control of multiple experiments, named differently sometimes, appear always on the left while using facet_wrap, I'll invite you for dinner, you god damned smartass
    ggarrange(
      plotlist = preprocResultInput()[['countsData']], 
      ncol = ifelse(
        length(unique(preprocResultInput()[['foldChangeData']][['ModelName']])) > 2,
        3,
        length(unique(preprocResultInput()[['foldChangeData']][['ModelName']]))
      )
    )
  })
  
  # Wrap in ui for dynamism
  output$countsPlot_ui <- renderUI({
    plotOutput(session$ns("countsPlot"), height = plot_dimensions()$height, width = plot_dimensions()$width)
  })
  
  # Render the title
  output$resultTitle <- renderText({
    req(input$genename,input$project)
    sprintf('Fold change of %s in the selected models', input$genename)
  })
  
  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename,input$project)
    preprocResultInput()[['foldChangeData']][c('ModelName','Genes','log2FoldChange','pvalue','padj')]
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
  
  # Make downloadeable table in excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'FoldChange.xlsx'), collapse = "_")
    },
    content = function(file) {
      write.xlsx(preprocResultInput()[['foldChangeData']][c('ModelName','Genes','log2FoldChange','pvalue','padj')], file)
    }
  )
  
  # Make dowload button for the plot as jpeg in proper resolution
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'boxplot.jpeg'), collapse = "_")
    },
    content = function(file) {
      # plot the thing
      pGC <- ggarrange(
        plotlist = preprocResultInput()[['countsData']], 
        ncol = ifelse(
          length(unique(preprocResultInput()[['foldChangeData']][['ModelName']])) > 2,
          3,
          length(unique(preprocResultInput()[['foldChangeData']][['ModelName']]))
        )
      )
      ggsave(file, plot = pGC, device = 'jpeg', height = plot_dimensions()$height*10, width = plot_dimensions()$width*10, dpi = 650, units = "px")
    }
  )
}