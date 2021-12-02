box::use(
  shiny[...],
  shiny.router[...],
  shinythemes[...],
  . / modules / model_consult,
  . / modules / model_comparison,
  . / modules / introduction
)

addResourcePath('static', '/DATA/RNAseq_test/API_shiny/static/')

router <- make_router(
  route("/", introduction$ui("introduction")),
  route("model_consult", model_consult$ui("model_consult")),
  route("model_comparison", model_comparison$ui("model_comparison"))
)

ui <- fluidPage(
  theme = "/static/main.css",
  tags$ul(
    tags$li(a(href = route_link("/"), "Home")),
    tags$li(a(href = route_link("model_consult"), "Model Consult")),
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
  callModule(introduction$server, "introduction")
  callModule(model_consult$server, "model_consult")
  callModule(model_comparison$server, "model_comparison")
}

app <- shinyApp(ui, server)

ip = '141.67.104.37'

runApp(app, host = ip, port = 5003)
