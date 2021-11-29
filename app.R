box::use(
  shiny[...],
  shiny.router[...],
  shinythemes[...],
  . / modules / gene_consult,
  . / modules / model_comparison
)

router <- make_router(
  route("gene_consult", gene_consult$ui("gene_consult")),
  route("model_comparison", model_comparison$ui("model_comparison"))
)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  title = "SEpIa",
  router$ui
)

server <- function(input, output, session) {
  router$server(input, output, session)
  callModule(gene_consult$server, "gene_consult")
  callModule(model_comparison$server, "model_comparison")
}

app <- shinyApp(ui, server)

runApp(app, port = 5003)
