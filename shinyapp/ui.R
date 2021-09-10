suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))

# Define UI for miles per gallon application
shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  
  # Application title
  headerPanel("Gene query"),
  
  # Application sidebar
  sidebarPanel(
    textInput("genename", "Genename:",
              value = 'Brca1'),
    
    selectInput("project", "Project:",
                list("Mouse Inflammation models" = "MouseModelsInflammation", 
                     "DSS time course" = "DSS_TimeCourse", 
                     "Wound Healing" = "WoundHealing",
                     "Washington University Cohort (Ileum) - E MTAB 5783" = "E_MTAB_5783_WashU",
                     "Sclerosing cholangitis Cohort (Colon) - E MTAB 7915" = "EMTAB7915_PSC_Cohort",
                     "PROTECT Cohort (Rectum) - GSE109142" = "PROTECT_GSE109142",
                     "RISK Cohort (Rectum) - GSE117993" = "RISK_GSE117993",
                     "Risk Cohort (Ileum) - GSE57945" = "RISK_GSE57945")),
    
    checkboxInput("timecourse", "Is Time Course", FALSE),
    
    submitButton("Update View")
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("resultTitle")),
    br(),
    plotOutput("FCplot"),
    br(),
    tableOutput("FCtable"),
    br(),
    br(),
    h3(textOutput("resultTitleCounts")),
    br(),
    plotOutput("countsPlot")
  )
))
