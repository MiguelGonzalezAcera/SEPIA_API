box::use(
  shiny[...],
  shiny.router[...],
  shinythemes[...],
  shinyauthr[...],
  . / modules / gene_consult,
  . / modules / model_consult,
  . / modules / model_comparison,
  . / modules / introduction,
  . / shinyapp / entities[userBase]
)

addResourcePath('static', '/DATA/RNAseq_test/API_shiny/static/')

router <- make_router(
  route("/", introduction$ui("introduction")),
  route("gene_consult", gene_consult$ui("gene_consult")),
  route("model_consult", model_consult$ui("model_consult")),
  route("model_comparison", model_comparison$ui("model_comparison"))
)

ui <- fluidPage(
  theme = "/static/main.css",
  
  tags$ul(
    tags$li(a(href = route_link("/"), "Home")),
    tags$li(a(href = route_link("gene_consult"), "Gene Consult")),
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

  # add logout button UI
  div(class = "pull-right", logoutUI(id = "logout")),
  # add login panel UI function
  loginUI(id = "login"),
  
  # Establish content
  router$ui
)

server <- function(input, output, session) {
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = userBase,
    user_col = user,
    pwd_col = password,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) { 
      router$server(input, output, session)
      
      callModule(introduction$server, "introduction")
      callModule(gene_consult$server, "gene_consult")
      callModule(model_consult$server, "model_consult")
      callModule(model_comparison$server, "model_comparison")
    }
  })
}

app <- shinyApp(ui, server)

runApp(app, port = 5003)
