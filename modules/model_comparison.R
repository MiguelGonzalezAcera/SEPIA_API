box::use(
  shiny[...],
  ggplot2[...],
  ggpubr[...],
  openxlsx[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[imgLabels,displayNames,imgFormat]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Model comparison"),
    
    # Application sidebar
    sidebarPanel(
      # Select the models to compare
      selectInput(ns("projectA"), "Project A:", displayNames, selected = 'AcDSS'),
      selectInput(ns("projectB"), "Project B:", displayNames, selected = 'cDSS'),
      
      # Select genes to highlight
      #<TODO>: Add an option to upload the gene list. Copy dynamic from Gene groups
      selectizeInput(
        ns("genename"),
        label = "Gene names (optional): ",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      
      # Select image format
      selectInput(ns("fformat"), "Image download format:", imgFormat),

      width = 3
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      div(
        class = 'SmallTitleText',
        textOutput(ns("resultTitleComparison"))
      ),
      br(),
      # Selecting the same two models should display an error.
      conditionalPanel(
        condition = "output.sameprojErrorDisplay == false",
        div(
          # Fancy little loader bc this could take a bit attached to the plot generation
          shinycssloaders::withSpinner(plotOutput(ns("comparisonPlot"), height = 650, width = 650, click = ns("gene_name")), type = 2, color="#f88e06", color.background = "white"),
          br(),
          # Section for the gene information
          #<TODO>: Increase the information when only 1 gene is clicked
          div(style='width: 650px;background-color:rgba(248, 142, 6, 0.2);padding-bottom: 25px;border-style: solid;border-color:rgba(248, 142, 6, 1);',
            br(),
            div(style='margin-left:25px;',
              htmlOutput(ns("gene_info"))
            )
          ),
          br(),
          br(),
          # Note for when a selected gene does not appear in the plot for whichever reason
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
          # Download buttons
          downloadButton(ns("downloadData"), 'Download Genes', class = 'DLButton'),
          downloadButton(ns("downloadSCPlot"), 'Download Scatterplot', class = 'DLButton'),
          br(),
          br(),
          # Venn diagram section
          div(
            class = 'SmallTitleText',
            textOutput(ns("vennTitleComparison"))
          ),
          br(),
          uiOutput(ns("vennPlot_ui")),
          br(),
          # download them venns
          downloadButton(ns("downloadVennPlot"), 'Download Venn diagram', class = 'DLButton'),
          br()
        ),
        ns = ns
      ),
      # The errror in question when selecting the same exps
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
  # Hide the error when false
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
  output$gene_info <- renderText({
    # With base graphics, need to tell it what the x and y variables are.
    clickedDF <- nearPoints(preprocComparisonsInput()[['fullData']], input$gene_name, threshold = 10, xvar = "log2FoldChange.x", yvar = "log2FoldChange.y")
    
    # Get the quadrant of the click
    clickedA <- all(clickedDF[['log2FoldChange.x']] >= 0)
    clickedB <- all(clickedDF[['log2FoldChange.y']] >= 0)
    
    if (clickedA) {
      if (clickedB) {
        quadrant = 'Q2'
      } else {
        quadrant = 'Q4'
      }
    } else {
      if (clickedB) {
        quadrant = 'Q1'
      } else {
        quadrant = 'Q3'
      }
    }
    
    # Display the text structure
    clicked <- clickedDF[['Genes']]
    if (length(clicked) == 0) {
      "Please, click one gene."
    } else {
      sprintf("<b>%s</b>. Gene: <u>%s</u>. ", quadrant, clicked)
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
      paste(c(paste(c("Sepia", gsub("-","",as.character(Sys.Date())), 'scatter'), collapse = "_"), input$fformat), collapse = "")
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
      paste(c(paste(c("Sepia", gsub("-","",as.character(Sys.Date())), 'venn'), collapse = "_"), input$fformat), collapse = "")
    },
    content = function(file) {
      # plot the thing
      pVd <- preprocComparisonsInput()[['vennData']]
      
      ggsave(file, plot = pVd, height = (650/3)*10, width = 6500, dpi = 650, units = "px")
    }
  )
}