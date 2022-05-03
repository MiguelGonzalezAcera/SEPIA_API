box::use(
  shiny[...],
  shiny.router[...],
  shinythemes[...],
  shinyauthr[...],
  . / modules / gene_consult,
  . / modules / model_consult,
  . / modules / model_comparison,
  . / modules / introduction,
  . / modules / gene_groups,
  . / modules / contact,
  . / shinyapp / entities[userBase]
)

#addResourcePath('static', 'static/')

router <- make_router(
  route("/", introduction$ui("introduction")),
  route("gene_consult", gene_consult$ui("gene_consult")),
  route("model_consult", model_consult$ui("model_consult")),
  route("model_comparison", model_comparison$ui("model_comparison")),
  route("gene_groups", gene_groups$ui("gene_groups")),
  route("contact", contact$ui("contact"))
)

ui <- fluidPage(
  theme = "main.css",
  
  tags$ul(
    tags$li(a(href = route_link("/"), "Home")),
    tags$li(a(href = route_link("gene_consult"), "Gene Consult")),
    tags$li(a(href = route_link("model_consult"), "Model Consult")),
    tags$li(a(href = route_link("model_comparison"), "Model Comparison")),
    tags$li(a(href = route_link("gene_groups"), "Gene groups")),
    tags$li(a(href = route_link("contact"), "Contact"))
  ),
  titlePanel(
    title = div(
      img(src = "logo1.png", height = 100),
      br(),
      "SEpIa"
    )
  ),

  # add logout button UI
  div(class = "pull-right", logoutUI(id = "logout")),
  # add login panel UI function
  loginUI(id = "login"),
  
  # Establish content
  router$ui,
  
  # Last break and bottom banner
  div(
    class = 'footer',
    textOutput("gitVers"),
  )
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
  
  output$gitVers <- renderText({
    sortedTagList <- git2r::tags(repo = '.')[order(names(git2r::tags(repo = ".")))]
    tagVers <- names(sortedTagList)[length(sortedTagList)]
    sprintf('Version: %s', tagVers)
  })
  
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) { 
      router$server(input, output, session)
      
      callModule(introduction$server, "introduction")
      callModule(gene_consult$server, "gene_consult")
      callModule(model_consult$server, "model_consult")
      callModule(model_comparison$server, "model_comparison")
      callModule(gene_groups$server, "gene_groups")
      callModule(contact$server, "contact")
    }
  })
}

#app <- shinyApp(ui, server)
shinyApp(ui, server)

#runApp(app)
