box::use(
  shiny[...],
  shinythemes[...]
)

# Paste the code for the select input snippet. Sauce: https://mastering-shiny.org/action-dynamic.html
make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

# Define UI for miles per gallon application
shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  
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
    selectInput("project", "Project:",
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
      "genename",
      label = NULL,
      choices = NULL,
      multiple = TRUE,
      width = "100%"
    ),

    checkboxInput("timecourse", "Is Time Course", FALSE),
    
    submitButton("Update View"),
    
    width = 3
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("resultTitleCounts")),
    br(),
    uiOutput("countsPlot_ui"),
    br(),
    h3(textOutput("resultTitle")),
    br(),
    tableOutput("FCtable")
  )
))
