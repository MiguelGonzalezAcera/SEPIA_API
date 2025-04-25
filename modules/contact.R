box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[contactDesc]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    # Application title
    headerPanel("Citation and contact"),
    
    # Application sidebar
    mainPanel(
      div(
        class = 'introText',
        htmlOutput(ns('Citation')),
        br(),
        htmlOutput(ns('Contacts'))
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$Citation <- renderText({
    "<h2>Cite us</h2></br>\
If you use SEPIA in your scientific publication, please cite the following paper:</br>\
</br>\
Gonzalez-Acera, M. et al. Integrated multi-model analysis of intestinal inflammation exposes key molecular features of preclinical and clinical IBD. bioRxiv, 2024.08.14 [<a href=\"https://www.biorxiv.org/content/10.1101/2024.08.14.607902v2\">Biorxiv</a>, <a href=\"https://scholar.google.com/scholar?as_q=Integrated+multi-model+analysis+of+intestinal+inflammation+exposes+key+molecular+features+of+preclinical+and+clinical+IBD&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=Gonzalez-Acera&as_publication=biorxiv&as_ylo=&as_yhi=&hl=es&as_sdt=0%2C5\">Google Scholar</a>]</br>\
</br>\
The raw data for each individual dataset is deposited in publicly available databases with the following IDs:</br>\
</br>\
<ul>
<li>Acute DSS colitis, Chronic DSS colitis and T cell transfer colitis: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14306\">E-MTAB-14306</a></li>
<li><i>Eimeria vermiformis</i> infection model: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14297\">E-MTAB-14297</a></li>
<li><i>Helicobacter hepaticus</i> infection model: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14316\">E-MTAB-14316</a></li>
<li>Oxazolone colitis and <i>Citrobacter rodentium</i> infection: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14312\">E-MTAB-14312</a></li>
<li>Intestinal epithelial cell Caspase 8 ablation: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14318\">E-MTAB-14318</a></li>
<li>Acute TNBS colitis and chronic TNBS colitis: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14329\">E-MTAB-14329</a></li>
<li>TnfdARE inflammation model: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14325\">E-MTAB-14325</a></li>
<li>Opa1dIEC inflammation model: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14487\">E-MTAB-14487</a></li>
<li>Anti-CD3 inflammation model: <a href=\"https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-14790\">E-MTAB-14790</a></li>
</ul>"
  })

  output$Contacts <- renderText({
    "<h2>Contact us</h2></br>\
<b>Lead developer:</b></br>\
Miguel Gonzalez Acera</br>\
E-mail: Miguel.GonzalezAcera@uk-erlangen.de</br>\
</br>\
<b>Project chief:</b></br>\
Christoph Becker</br>\
E-mail: Christoph.becker@uk-erlangen.de</br>\
</br>\
<b>IMPORTANT</b>: As the tool is constantly being developed, some errors and bugs are expected as new features are added. If a bug or a mistake is found when consulting the information hosted, please contact us and communicate us the issue, and we will fix it as soon as possible."
  })
}