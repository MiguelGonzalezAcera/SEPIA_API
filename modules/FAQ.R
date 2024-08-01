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
        class = 'introText',
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
There are two things that might have happened. One, the selected gene is not expressed at all in any of the samples, including controls, so it is filtered out in the sample processing. Two, some genes, even though expressed and detectable by other techniques, are not detected by RNAseq with similar ease for a plethora of reasons (length, transcript stability, multiple copies in the genome, etcetera). An RNAseq approach might not be the best course of action for the study of those genes.</br>
</br>
<b>Why is the p-adjusted value recommended for the analysis instead of the p-value?</b></br>
When running multiple statistical tests such as differential expressions over a complete transcriptome gene panel, the p-value is prone to produce false positives, especially when there is no standard threshold of significance. In order to adress this issue, multiple methods are used to correct and adjust the p-value and avoid as many false positives as possible (<a href=\"https://doi.org/10.22074%2Fcellj.2019.5992\">Jafari and Ansari-Pour, 2018</a>)</br>
</br>
<b>A gene cannot be found in the model comparison dot map.</b></br>
The genes that appear in the dot map have been filterd by adjusted p-value significance (p < 0.05 in both models). If a gene does not appear in the dot plot it is because it is not significant in, at least, one of the selected models. Please, check in the gene or model consult tools if your chosen genes have a significant fold change in both the selected models.</br>
</br>
<b></b></br>"
  })
}