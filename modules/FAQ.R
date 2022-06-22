box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[imgLabels,displayNames,modelDescriptions]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Frequently Asked Questions"),
    
    # Application sidebar
    mainPanel(
      div(
        htmlOutput(ns('FAQS'))
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$FAQS <- renderText({
    "</br>
<b>A selected gene does not appear in one of the results.</b></br>
There are two things that might have happened. One, the selected gene is not expressed at all in any of the samples, including controls, so it is filtered out in the sample processing. Two, circunstances of the sample and the sequencing might make some genes very difficult to sequence, even though there is empirical evidence of its expression. An RNAseq analysis might not be the ideal method for analyzing these genes.</br>
</br>
<b>Why is the p adjusted value recommended for the analysis instead of the p value?</b></br>
When running multiple statistical tests, such as differential expressions over a complete transcriptome gene panel, the p value observation is prone to produce false positives, especially when there is no standard threshold of significance. In order to adress this issue, multiple methods are used to correct and adjust the p value and avoid as many false positives as possible (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6099145/)</br>
</br>
<b>A gene cannot be found in the model comparison dot map.</b></br>
The genes that appear in the dot map have been filterd by adjusted p value significance. If a gene does not appear in the dot plot it is because it is not significan in, at least, one of the selected models. Please, check in the gene or model consult if your chosen genes have a significant fold change in both the selected models.</br>
</br>
<b></b></br>"
  })
}