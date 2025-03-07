box::use(
  shiny[...],
  ggplot2[...],
  .. / shinyapp / tools[...],
  .. / shinyapp / entities[imgLabels,displayNames,modelDescriptions]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # Css theme
  theme = "main.css"

  div(
    # Main body
    mainPanel(
      div(
        # Establish class for centered elements
        class = 'landing',

        # Add the circle 
        img(src = "SepiaCircle.png", height = 525),
        br(),

        # Title and short description
        div(
            class = 'introTextLand',
            htmlOutput(ns('SepiaDesc'))
        ),
        br(),

        # Features in columns. Get a 'table' with 1 row and 3 cols. all named for formatting.
        div(
            class = "row",
            div(
                class = 'column',
                htmlOutput(ns('SepiaDescC1'))
            ),
            div(
                class = 'column',
                htmlOutput(ns('SepiaDescC2'))
            ),
            div(
                class = 'column',
                htmlOutput(ns('SepiaDescC3'))
            ),
        ),
        br(),
        br(),
        br(),

        # Get a 'footer' in the bottom of the section with logos.
        div(
            class = 'landing_footer',

            # logos in columns so it looks a little better
            div(
                class = 'rowF',
                div(
                    class = 'columnF',
                    img(src = 'logo1_trnsp.png', height = 100)
                ),
                div(
                    class = 'columnF',
                    img(src = 'FAU_logo2.png', height = 70)
                ),
                div(
                    class = 'columnF',
                    img(src = 'Charite_logo.png', height = 70)
                ),
                div(
                    class = 'columnF',
                    img(src = 'drfz.png', height = 70)
                ),
                div(
                    class = 'columnF',
                    img(src = 'Logo_i-med_trnsp.png', height = 100)
                ),
                div(
                    class = 'columnF',
                    img(src = 'MDC_logo.png', height = 110)
                ),
                div(
                    class = 'columnF',
                    img(src = 'uksh_logo.png', height = 100)
                )
            )
        )
      ),
      width = 12
    )
  )
}

#' @export
server <- function(input, output, session) {
  # Title string
  output$SepiaDesc <- renderText({
    "</br>
<h1>SEPIA: A databank to explore transcriptomic changes in mouse models of intestinal inflammation.</h1>
</br>
<b>Inflammatory bowel disease</b> (IBD) is a chronic inflammatory condition of the intestine with a complex and multifaceted pathogenesis. Multiple animal models exist to study specific disease mechanisms relevant to human IBD. This platform recapitulates the transcriptomic changes observed in models of intestinal inflammation generated in mouse and allows researchers to explore, analyze and compare the datasets.
"
  })

  output$SepiaDescC1 <- renderText({
    "</br>
<h3>3R principles</h3>
</br>
Using this platform to explore mouse intestinal inflammation models fullfills the <b><i>3R</i></b> principles of animal ethics in reseqarch. It <i>Replaces</i> the repeated generation of the mouse model with an in-silico approach. It <i>Reduces</i> the number of mice employed for comparisons of inflamed vs control. It helps <i>Refine</i> research methods by granting access to the transcriptomic information with an user-friendly platform that grants the desired results in a dilligent time.
"
  })

  output$SepiaDescC2 <- renderText({
    "</br>
<h3>Comprehensive model selection</h3>
</br>
At the stage of design for future experiments, the selection of a model in which to test novel hypotheses can be challenging, especially if the information about a topic is limited. This can lead to the spending of time, money and other resources looking for an appropriate model for a new project. With this platform, it is possible to consult the transcriptomic alterations in a particular model, and establish simple conclusions and preliminary information that supports the selection of a relevant model for future studies.
"
  })

  output$SepiaDescC3 <- renderText({
    "</br>
<h3>Hypothesis testing</h3>
</br>
Working in a particular model of intestinal inflammation might limit the relevance of scientific results. It is often difficult to determine if the alterations present in a model are a result of the intestinal inflammation or the particular characteristics of the chosen modelling attempt. This platform allows researchers to contrast their results when working with mouse models with other methods to generate intestinal inflammation, expanding the context and the scope of their analysis.
"
  })
}