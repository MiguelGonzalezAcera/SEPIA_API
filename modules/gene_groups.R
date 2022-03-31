box::use(
  shiny[...],
  ggplot2[...],
  ggpubr[...],
  ComplexHeatmap[...],
  dendextend[...],
  cluster[...],
  gplots[...],
  circlize[...],
  openxlsx[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[fullExp,singleExp,imgLabels,displayNames,markerNames]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  theme = "main.css"
  
  div(
    # Application title
    headerPanel("Gene set behaviour"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("project"), "Project:", displayNames),
      selectInput(ns("genelist_markers"), "Available markers:", markerNames),
      
      # Upload data
      fileInput(ns('genelist_upload'), 'Upload gene set:', accept = c('.txt','.xlsx')),
      
      # Add short description of the tool
      br(),
      div(
        div(
          class = 'SmallTitleText',
          'Tool description:'
        ),
        div(
          class = 'ToolDesc',
          'Explore a group of genes in one of the models.'
        )
      ),
      
      width = 3
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      div(
        class = 'SmallTitleText',
        textOutput(ns("heatmapTitle"))
      ),
      br(),
      conditionalPanel(
        condition = "output.heatmapDisplay == false",
        tags$div(
          class = 'errorFrame',
          br(),
          div(
            class = 'errorText',
            textOutput(ns("errorMessage"))
          )
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "output.heatmapDisplay == true",
        div(
          div(style='margin-left:200px',
            uiOutput(ns("heatmapResult_ui"))
          ),
          br(),
          div(style='margin-left:275px',
            tableOutput(ns("heatmapTable"))
          ),
          br(),
          div(style='display: flex;',
            div(
              style='margin-left: 60px;',
              div(
                class = 'SmallTitleText',
                'Volcano plot'
              ),
              br(),
              uiOutput(ns("volcanoResult_ui"))
            ),
            div(
              style='margin-left: 60px;',
              div(
                class = 'SmallTitleText',
                'Gene Set Enrichment Analysis'
              ),
              br(),
              uiOutput(ns("GSEAResult_ui")),
              br(),
              tableOutput(ns("GSEATable"))
            )
          )
        ),
        ns = ns
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Create reactive values for the genelist
  genelist <- reactiveValues(genes=c(), title='')
  
  # create two observeEvent to select the usable genelist
  observeEvent(input$genelist_markers,{
    genelist$genes <- getMarkerlist(input$genelist_markers)
    genelist$title <- sprintf('Behaviour of the gene markers for %s', input$genelist_markers)
    genelist$errormess <- sprintf('The genes from the %s gene list are unavailable in the selected project', input$genelist_markers)
    genelist$handle <- input$genelist_markers
  })
  
  observeEvent(input$genelist_upload,{
    genelist$genes <- readGenelist(input$genelist_upload$datapath)
    genelist$title <- 'Behaviour of the uploaded gene list'
    genelist$errormess <- 'The genes from the uploaded gene list are unavailable in the selected project'
    genelist$handle < 'Uploaded gene list'
  })
  
  # Render title of counts plot
  output$heatmapTitle <- renderText({
    req(genelist$title)
    genelist$title
  })
  
  # Preprocess the data
  output$heatmapResult <- renderPlot({
    req(genelist$genes)
    heatmap(input$project, genelist$genes)
  })
  
  # Make dimensions for the plot
  plot_dimensions <- reactive({
    list(
      height = 750,
      width = 750
    )
  })
  
  # Wrap in ui for dynamism
  output$heatmapResult_ui <- renderUI({
    shinycssloaders::withSpinner(plotOutput(session$ns("heatmapResult"), height = plot_dimensions()$height, width = plot_dimensions()$width), type = 2, color="#f88e06", color.background = "white")
  })
  
  # Render fold change table
  output$heatmapTable <- renderTable({
    req(genelist$genes)
    queryExperiment(singleExp[[input$project]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')]
  })
  
  # Render informative note about the gene selection
  output$fctableNote <- renderText({
    req(output$heatmapTable)
    '<b>NOTE:</b> Because there are different models being displayed, the statistical indicator to consider is the <b>P adjusted value</b>, not the P value.'
  })
  
  # Check if it has to display the plots
  output$heatmapDisplay <- reactive({
    # check if there is data to display
    if (dim(queryExperiment(singleExp[[input$project]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')])[1] > 1) {
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(session$output, "heatmapDisplay", suspendWhenHidden = FALSE)
  
  # Generate the volcano plot
  output$volcanoResult <- renderPlot({
    req(genelist$genes)
    volcanoPlot(input$project, genelist$genes)
  })
  
  # Wrap
  output$volcanoResult_ui <- renderUI({
    shinycssloaders::withSpinner(plotOutput(session$ns("volcanoResult"), height = plot_dimensions()$height/1.6, width = plot_dimensions()$width/1.6), type = 2, color="#f88e06", color.background = "white")
  })
  
  # Generate the volcano plot
  output$GSEAResult <- renderPlot({
    req(genelist$genes)
    GSEAresultList <- GSEAgraph(input$project, genelist$genes, genelist$handle)
    genelist$GSEAtableResult <- GSEAresultList[['table_gsea']]
    GSEAresultList[['plot_GSEA']]
  })
  
  # Wrap
  output$GSEAResult_ui <- renderUI({
    shinycssloaders::withSpinner(plotOutput(session$ns("GSEAResult"), height = plot_dimensions()$height/2, width = plot_dimensions()$width/2), type = 2, color="#f88e06", color.background = "white")
  })
  
  # Render GSEA table
  output$GSEATable <- renderTable({
    req(genelist$genes, genelist$GSEAtableResult)
    genelist$GSEAtableResult[c('Description','enrichmentScore', 'pvalue', 'p.adjust')]
  })
  
  # Render error message
  # Render informative note about the gene selection
  output$errorMessage <- renderText({
    req(genelist$genes)
    genelist$errormess
  })
  
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
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'boxplot.png'), collapse = "_")
    },
    content = function(file) {
      # plot the thing
      pGC <- ggarrange(
        plotlist = preprocResultInput()[['countsData']], 
        ncol = ifelse(
          length(unique(preprocResultInput()[['foldChangeData']][['ModelName']])) > 2,
          3,
          length(unique(preprocResultInput()[['foldChangeData']][['ModelName']]))
        ),
        nrow = ceiling(length(unique(preprocResultInput()[['foldChangeData']][['ModelName']]))/3)
      )
      ggsave(file, plot = pGC, device = 'jpeg', height = plot_dimensions()$height*10, width = plot_dimensions()$width*10, dpi = 650, units = "px")
    }
  )
}