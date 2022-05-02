box::use(
  shiny[...],
  shinyFeedback[...],
  ggplot2[...],
  ggpubr[...],
  ComplexHeatmap[...],
  dendextend[...],
  cluster[...],
  gplots[...],
  circlize[...],
  openxlsx[...],
  grid[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[fullExp,singleExp,imgLabels,displayNames,markerNames]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  theme = "main.css"
  
  div(
    # Explicitely state we want to use the feedback for this
    useShinyFeedback(),
    
    # Application title
    headerPanel("Gene set behaviour"),
    
    # Application sidebar
    sidebarPanel(
      selectInput(ns("project"), "Project:", displayNames),
      selectInput(ns("genelist_markers"), "Available markers:", markerNames),
      
      # Upload data
      fileInput(ns('genelist_upload'), 'Upload gene set:', accept = c('.txt','.xlsx')),
      
      # Add short description of the tool
      div(
        div(
          class = 'SmallTitleText',
          'Tool description:'
        ),
        br(),
        div(
          class = 'ToolDesc',
          'Explore a group of genes in one of the models.',
          br(),
          'You can upload your chosen mouse genes in a txt file, with each gene name in a line.',
          br(),
          'Example:',
          br(),
          br(),
          'Brca1',
          br(),
          'Brca2',
          br(),
          'Pkd1',
          br(),
          br(),
          'You can also upload an excel file with only one sheet and your genes in a single column.',
          br(),
          br(),
          HTML('<b>NOTE</b>: Be sure that the genes are written according to official mouse gene notation (lowercase with capital first letter)')
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
            shinycssloaders::withSpinner(plotOutput(ns("heatmapResult"), height = 750, width = 750), type = 2, color="#f88e06", color.background = "white")
          ),
          br(),
          div(style='margin-left:275px; height:400px; width: 650px; overflow-y: scroll;',
            tableOutput(ns("heatmapTable"))
          ),
          br(),
          div(style='margin-left:275px',
            class = 'fcTableNoteFrame',
            div(
              class = 'NoteFrame',
              div(
                class = 'NoteContent',
                htmlOutput(ns('fctableNote'))
              )
            )
          ),
          br(),
          downloadButton(ns("downloadHmap"), 'Download Heatmap', class = 'DLButton'),
          downloadButton(ns("downloadTable"), 'Download Table', class = 'DLButton'),
          br(),
          br(),
          br(),
          div(style='display: flex;',
            div(
              style='margin-left: 60px;',
              div(
                class = 'SmallTitleText',
                'Volcano plot'
              ),
              br(),
              shinycssloaders::withSpinner(plotOutput(ns("volcanoResult"), height = 750/1.65, width = 750/1.65), type = 2, color="#f88e06", color.background = "white"),
              br(),
              downloadButton(ns("downloadVolc"), 'Download Volcano plot', class = 'DLButton')
            ),
            div(
              style='margin-left: 60px;',
              div(
                class = 'SmallTitleText',
                'Gene Set Enrichment Analysis'
              ),
              br(),
              shinycssloaders::withSpinner(plotOutput(ns("GSEAResult"), height = 750/2, width = 750/2), type = 2, color="#f88e06", color.background = "white"),
              br(),
              tableOutput(ns("GSEATable")),
              downloadButton(ns("downloadGSEA"), 'Download GSEA Plot', class = 'DLButton'),
              downloadButton(ns("downloadGSEATable"), 'Download GSEA Table', class = 'DLButton')
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
    genelist$title <- sprintf('Behaviour of the gene markers for %s', names(markerNames)[match(input$genelist_markers,markerNames)])
    genelist$errormess <- sprintf('The genes from the %s gene list are unavailable in the selected project', names(markerNames)[match(input$genelist_markers,markerNames)])
    genelist$handle <- input$genelist_markers
  })
  
  observeEvent(input$genelist_upload,{
    # give feedback on the upload
    if (endsWith(input$genelist_upload$datapath, '.txt') | endsWith(input$genelist_upload$datapath, '.xlsx')) {
      showFeedbackSuccess(
        inputId = "genelist_upload",
        text = "Genelist uploaded successfully."
      )
    } else {
      showFeedbackDanger(
        inputId = "genelist_upload",
        text = "Wrong file format!"
      )
    }
  })
  
  observeEvent(input$genelist_upload,{
    # Check if the extension is the one
    extUpl <- endsWith(input$genelist_upload$datapath, '.txt') | endsWith(input$genelist_upload$datapath, '.xlsx')
    req(extUpl, cancelOutput = TRUE)
    
    # check if the uploaded thing has any genes in the mouse gene record
    extGenes <- length(readGenelist(input$genelist_upload$datapath)) > 0
    if (!extGenes) {
      showFeedbackDanger(
        inputId = "genelist_upload",
        text = "File contents are not gene names."
      )
    }
    req(extGenes, cancelOutput = TRUE)
    
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
  
  # Render fold change table
  output$heatmapTable <- renderTable({
    req(genelist$genes)
    queryExperiment(singleExp[[input$project]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')]
  })
  
  # Render informative note about the gene selection
  output$fctableNote <- renderText({
    req(genelist$genes)
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
  
  # Generate the volcano plot
  output$GSEAResult <- renderPlot({
    req(genelist$genes)
    GSEAresultList <- GSEAgraph(input$project, genelist$genes, genelist$handle)
    genelist$GSEAtableResult <- GSEAresultList[['table_gsea']]
    genelist$GSEAplotResult <- GSEAresultList[['plot_GSEA']]
    GSEAresultList[['plot_GSEA']]
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
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'FoldChange.xlsx'), collapse = "_")
    },
    content = function(file) {
      write.xlsx(queryExperiment(singleExp[[input$project]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')], file)
    }
  )
  
  # Make dowload button for the plot as png in proper resolution
  output$downloadHmap <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'heatmap.png'), collapse = "_")
    },
    content = function(file) {
      # Save the image
      ggsave(file, plot = grid.grabExpr(draw(heatmap(input$project, genelist$genes))), height = 7500, width = 7500, dpi = 650, units = "px")
    }
  )
  
  output$downloadVolc <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'Volcano.png'), collapse = "_")
    },
    content = function(file) {
      # Save the image
      ggsave(file, plot = volcanoPlot(input$project, genelist$genes), height = (750/1.6)*10, width = (750/1.6)*10, dpi = 650, units = "px")
    }
  )
  
  output$downloadGSEA <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'GSEA.png'), collapse = "_")
    },
    content = function(file) {
      # Save the image
      ggsave(file, plot = genelist$GSEAplotResult, height = (750/2)*10, width = (750/2)*10, dpi = 650, units = "px")
    }
  )
  
  output$downloadGSEATable <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'GSEA.xlsx'), collapse = "_")
    },
    content = function(file) {
      write.xlsx(genelist$GSEAtableResult[c('Description','enrichmentScore', 'pvalue', 'p.adjust')], file)
    }
  )
}