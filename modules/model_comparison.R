box::use(
  shiny[...],
  ggplot2[...],
  ggpubr[...],
  openxlsx[...],
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
      selectInput(ns("projectA"), "Project A:", displayNames, selected = 'AcDSS'),
      selectInput(ns("projectB"), "Project B:", displayNames, selected = 'cDSS'),
      
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
      conditionalPanel(
        condition = "output.sameprojErrorDisplay == false",
        div(
          plotOutput(ns("comparisonPlot"), height = 650, width = 650, click = ns("gene_name")),
          div(style='width: 650px;',
            verbatimTextOutput(ns("gene_info"))
          ),
          br(),
          conditionalPanel(
            condition = "output.noteDisplay == true",
            div(
              class = 'comparisonPlotNoteFrame',
              div(
                class = 'NoteFrame',
                div(
                  class = 'NoteContent',
                  htmlOutput(ns('genelistNote'))
                )
              )
            ),
            ns = ns
          ),
          br(),
          downloadButton(ns("downloadData"), 'Download Genes', class = 'DLButton'),
          downloadButton(ns("downloadSCPlot"), 'Download Scatterplot', class = 'DLButton'),
          br(),
          br(),
          div(
            class = 'SmallTitleText',
            textOutput(ns("vennTitleComparison"))
          ),
          br(),
          uiOutput(ns("vennPlot_ui")),
          br(),
          downloadButton(ns("downloadVennPlot"), 'Download Venn diagram', class = 'DLButton'),
          br()
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "output.sameprojErrorDisplay == true",
        div(
          class = 'errorFrame',
          br(),
          div(
            class = 'errorText',
            p('Please, select two different experiments.'),
            br(),
            tableOutput(ns("errorTable"))
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
  
  # Check if the two projects are the same
  output$sameprojErrorDisplay <- reactive({
    input$projectA == input$projectB
  })
  outputOptions(session$output, "sameprojErrorDisplay", suspendWhenHidden = FALSE)
  
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
  
  # Render title of Venn diagram set
  output$vennTitleComparison <- renderText({
    req(input$projectB)
    sprintf('Venn diagrams of significant genes between models %s and %s', names(displayNames)[match(input$projectA,displayNames)], names(displayNames)[match(input$projectB,displayNames)])
  })
    
  # Produce plot
  output$comparisonPlot <- renderPlot({
    req(input$projectB)
    preprocComparisonsInput()[['plotData']]
  })
  
  # Render the text for the clicking on the dots
  output$gene_info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    clicked <- nearPoints(preprocComparisonsInput()[['fullData']], input$gene_name, threshold = 10, xvar = "log2FoldChange.x", yvar = "log2FoldChange.y")[["Genes"]]
    if (length(clicked) == 0) {
      "Please, click one gene."
    } else {
      sprintf("Gene: %s", clicked)
    }
  })
  
  # Create and render venn diagram plot set
  output$vennPlot <- renderPlot({
    req(input$projectB)
    preprocComparisonsInput()[['vennData']]
  })

  # Wrap plot in ui for dynamism
  output$vennPlot_ui <- renderUI({
    shinycssloaders::withSpinner(plotOutput(session$ns("vennPlot"), height = 650/2, width = 650*1.2), type = 2, color="#f88e06", color.background = "white")
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
  
  # Make downloadeable table in excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'GenesByQuadrant.xlsx'), collapse = "_")
    },
    content = function(file) {
      # Create workbook
      wb <- createWorkbook()
      
      # Iter through the tables
      for (name in names(preprocComparisonsInput()[['commonData']])) {
        # Add the sheet
        sheetDB <- addWorksheet(wb, sheetName = name)
        
        # add the data
        writeData(wb, sheetDB, preprocComparisonsInput()[['commonData']][[name]])
      }
      
      # Save the notebook
      saveWorkbook(wb, file = file)
    }
  )
  
  # Make dowload button for the plot as jpeg in proper resolution
  output$downloadSCPlot <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'scatter.png'), collapse = "_")
    },
    content = function(file) {
      # plot the thing
      pSc <- preprocComparisonsInput()[['plotData']]
      
      ggsave(file, plot = pSc, height = 6500, width = 6500, dpi = 650, units = "px")
    }
  )
  
  # Make dowload button for the plot as jpeg in proper resolution
  output$downloadVennPlot <- downloadHandler(
    filename = function() {
      paste(c("Sepia",gsub("-","",as.character(Sys.Date())),'venn.png'), collapse = "_")
    },
    content = function(file) {
      # plot the thing
      pVd <- preprocComparisonsInput()[['vennData']]
      
      ggsave(file, plot = pVd, height = (650/3)*10, width = 6500, dpi = 650, units = "px")
    }
  )
}