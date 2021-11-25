box::use(
  shiny[...],
  shiny.router[...],
  shinythemes[...],
  . / modules / gene_consult,
)

router <- make_router(
  route("gene_consult", gene_consult$ui("gene_consult"))
)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  title = "SEpIa",
  router$ui
)

server <- function(input, output, session) {
  router$server(input, output, session)
  callModule(gene_consult$server, "gene_consult")
}

app <- shinyApp(ui, server)

runApp(app, port = 5003)
