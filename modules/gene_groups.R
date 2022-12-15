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
  .. / shinyapp / entities[fullExp,singleExp,imgLabels,displayNames,markerNames,imgFormat]
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
      selectizeInput(
        ns("project"),
        label = "Project:",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      
      selectInput(ns("genelist_markers"), "Available markers:", markerNames),
      
      # Upload data
      fileInput(ns('genelist_upload'), 'Upload gene set:', accept = c('.txt','.xlsx')),
      
      # Select image format
      selectInput(ns("fformat"), "Image download format:", imgFormat),
      
      # Add short description of the tool
      div(
        div(
          class = 'SmallTitleText',
          'Tool description:'
        ),
        br(),
        div(
          class = 'ToolDesc',
          'Explore a group of genes in one of the models. You may select more than one model to explore, with a different type of plot.',
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
          HTML('<b>NOTE</b>: Be sure that the genes are written according to official mouse gene notation (lowercase with capital first letter). Max number of genes: 400')
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
          conditionalPanel(
            condition = 'output.plotsDisplay == true',
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
            ),
            ns = ns
          )
        ),
        ns = ns
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Update the selection for the projects
  updateSelectizeInput(
    session,
    "project",
    choices = displayNames,
    selected = c("AcDSS"),
    server = TRUE
  )
  
  # Create reactive values for the genelist
  genelist <- reactiveValues(genes=c(), title='')
  
  # create two observeEvent to select the usable genelist
  observeEvent(input$genelist_markers,{
    genelist$genes <- getMarkerlist(input$genelist_markers)
    genelist$title <- sprintf('Behaviour of the gene markers for %s', names(markerNames)[match(input$genelist_markers,markerNames)])
    genelist$errormess <- sprintf('The genes from the %s gene list are unavailable in the selected project', names(markerNames)[match(input$genelist_markers,markerNames)])
    genelist$handle <- names(markerNames)[match(input$genelist_markers,markerNames)]
  })
  
  observeEvent(input$genelist_upload,{
    # Hide existing feedback
    hideFeedback("genelist_upload")
    
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
      # Hide existing feedback
      hideFeedback("genelist_upload")
      
      showFeedbackDanger(
        inputId = "genelist_upload",
        text = "File contents are not gene names."
      )
    }
    req(extGenes, cancelOutput = TRUE)
    
    # check if the uploaded thing is too long
    extGenesLen <- length(readGenelist(input$genelist_upload$datapath)) < 400
    if (!extGenes) {
      # Hide existing feedback
      hideFeedback("genelist_upload")
      
      showFeedbackDanger(
        inputId = "genelist_upload",
        text = "Gene list is too long."
      )
    }
    req(extGenes, cancelOutput = TRUE)
    
    genelist$genes <- readGenelist(input$genelist_upload$datapath)
    genelist$title <- 'Behaviour of the uploaded gene list'
    genelist$errormess <- 'The genes from the uploaded gene list are unavailable in the selected project'
    genelist$handle <- 'Uploaded gene list'
  })
  
  # Render title of counts plot
  output$heatmapTitle <- renderText({
    req(genelist$title)
    genelist$title
  })
  
  # Preprocess the data
  output$heatmapResult <- renderPlot({
    req(genelist$genes)
    
    # If Project is empty, use all of 'em
    if (length(input$project) == 0) {
      project <- unlist(displayNames, use.names = FALSE)
    } else {
      project <- input$project
    }

    heatmap(project, genelist$genes)
  })
  
  # Render fold change table
  output$heatmapTable <- renderTable({
    req(genelist$genes)
    if (length(input$project) == 1) {
      tabresult <- queryExperiment(singleExp[[input$project]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')]
    } else {
      # If Project is empty, use all of 'em
      if (length(input$project) == 0) {
        project <- unlist(displayNames, use.names = FALSE)
      } else {
        project <- input$project
      }
      tabresult <- NULL
      for (tabname in project) {
        wdf <- queryExperiment(singleExp[[tabname]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')]
        wdf['Model'] <- names(displayNames)[displayNames == tabname]
        if (is.null(tabresult) == T){
          tabresult <- wdf
        } else {
          tabresult <- rbind(tabresult, wdf)
        }
      }
    }

    tabresult
  }, digits = 5)
  
  # Render informative note about the gene selection
  output$fctableNote <- renderText({
    req(genelist$genes)
    '<b>NOTE:</b> Because there are different models being displayed, the statistical indicator to consider is the <b>P adjusted value</b>, not the P value.'
  })
  
  # Check if it has to display the heatmap
  output$heatmapDisplay <- reactive({
    # check if there is data to display
    # Adapt for cases
    if (length(input$project) == 0) {
      tab <- 'AcDSS'
    } else if (length(input$project) == 1) {
      tab <- input$project
    } else {
      tab <- input$project[1]
    }
    # do the checking
    if (dim(queryExperiment(singleExp[[tab]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')])[1] > 1) {
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(session$output, "heatmapDisplay", suspendWhenHidden = FALSE)
  
  # Check if it has to display the plots
  output$plotsDisplay <- reactive({
    # check if there is data to display
    if (length(input$project) == 1) {
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(session$output, "plotsDisplay", suspendWhenHidden = FALSE)
  
  # Generate the volcano plot
  output$volcanoResult <- renderPlot({
    req(genelist$genes)
    if (length(input$project) == 1) {
      volcanoPlot(input$project, genelist$genes)
    }
  })
  
  # Generate the volcano plot
  output$GSEAResult <- renderPlot({
    req(genelist$genes)
    if (length(input$project) == 1) {
      GSEAresultList <- GSEAgraph(input$project, genelist$genes, genelist$handle)
      genelist$GSEAtableResult <- GSEAresultList[['table_gsea']]
      genelist$GSEAplotResult <- GSEAresultList[['plot_GSEA']]
      GSEAresultList[['plot_GSEA']]
    }
  })
  
  # Render GSEA table
  output$GSEATable <- renderTable({
    req(genelist$genes, genelist$GSEAtableResult)
    genelist$GSEAtableResult[c('Description','enrichmentScore', 'pvalue', 'p.adjust')]
  }, digits = 5)
  
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
      if (length(input$project) == 1) {
        tabresult <- queryExperiment(singleExp[[input$project]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')]
      } else {
        # If Project is empty, use all of 'em
        if (length(input$project) == 0) {
          project <- unlist(displayNames, use.names = FALSE)
        } else {
          project <- input$project
        }
        tabresult <- NULL
        for (tabname in project) {
          wdf <- queryExperiment(singleExp[[tabname]][['tabid']], genelist$genes)[c('EnsGenes','Genes','log2FoldChange','pvalue','padj')]
          wdf['Model'] <- names(displayNames)[displayNames == tabname]
          if (is.null(tabresult) == T){
            tabresult <- wdf
          } else {
            tabresult <- rbind(tabresult, wdf)
          }
        }
      }
      write.xlsx(tabresult, file)
    }
  )
  
  # Make dowload button for the plot as png in proper resolution
  output$downloadHmap <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'heatmap',input$fformat), collapse = "_")
    },
    content = function(file) {
      grDevices::pdf(NULL)
      
      # If Project is empty, use all of 'em
      if (length(input$project) == 0) {
        project <- unlist(displayNames, use.names = FALSE)
        
        # Save the image
        ggsave(file, plot = heatmap(project, genelist$genes), height = 7500, width = 7500, dpi = 650, units = "px")
      } else {
        project <- input$project
        
        # Save the image
        ggsave(file, plot = heatmap(project, genelist$genes), height = 7500, width = 7500, dpi = 650, units = "px")
      }
    }
  )
  
  output$downloadVolc <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'Volcano',input$fformat), collapse = "_")
    },
    content = function(file) {
      # Save the image
      ggsave(file, plot = volcanoPlot(input$project, genelist$genes), height = (750/1.6)*10, width = (750/1.6)*10, dpi = 650, units = "px")
    }
  )
  
  output$downloadGSEA <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'GSEA',input$fformat), collapse = "_")
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