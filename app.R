box::use(
  shiny[...],
  shiny.router[...],
  shinythemes[...],
  . / modules / gene_consult,
  . / modules / model_comparison
)

addResourcePath('static', '/DATA/RNAseq_test/API_shiny/static/')

router <- make_router(
  route("gene_consult", gene_consult$ui("gene_consult")),
  route("model_comparison", model_comparison$ui("model_comparison"))
)

ui <- fluidPage(
  theme = "/static/main.css",
  tags$ul(
    tags$li(a(href = route_link("gene_consult"), "Gene Consult")),
    tags$li(a(href = route_link("model_comparison"), "Model Comparison"))
  ),
  titlePanel(
    title = div(
      img(src = "/static/logo1.png", height = 100),
      br(),
      "SEpIa"
    )
  ),
  router$ui
)

server <- function(input, output, session) {
  router$server(input, output, session)
  callModule(gene_consult$server, "gene_consult")
  callModule(model_comparison$server, "model_comparison")
}

app <- shinyApp(ui, server)

runApp(app, port = 5003)
