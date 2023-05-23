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
  . / modules / FAQ,
  . / modules / contact,
  . / shinyapp / entities[userBase]
)

#addResourcePath('static', 'static/')

# Router for the pages. The stuff behind menu thingy atop the page to navigate.
#<NOTE>: If I'm gonna keep adding tools to this thing I should make it a dropdown menu
router <- make_router(
  route("/", introduction$ui("introduction")),
  route("gene_consult", gene_consult$ui("gene_consult")),
  route("model_consult", model_consult$ui("model_consult")),
  route("model_comparison", model_comparison$ui("model_comparison")),
  route("gene_groups", gene_groups$ui("gene_groups")),
  route("FAQ", FAQ$ui("FAQ")),
  route("contact", contact$ui("contact"))
)

# User interface for the website
ui <- fluidPage(
  # Get the css config
  theme = "main.css",
  
  # Place the links to the other pages. Elements are tagged to be recognized by the css
  tags$ul(
    tags$li(a(href = route_link("/"), "Home")),
    tags$li(a(href = route_link("gene_consult"), "Gene Consult")),
    tags$li(a(href = route_link("model_consult"), "Model Consult")),
    tags$li(a(href = route_link("model_comparison"), "Model Comparison")),
    tags$li(a(href = route_link("gene_groups"), "Gene groups")),
    tags$li(a(href = route_link("FAQ"), "FAQ")),
    tags$li(a(href = route_link("contact"), "Contact"))
  ),
  # Title.
  #<NOTE>: We should make a logo.
  titlePanel(
    title = div(
      img(src = "logo1.png", height = 100),
      br(),
      "SEpIa"
    )
  ),

  # Establish the log in and logout buttons
  # add logout button UI
  div(class = "pull-right", logoutUI(id = "logout")),
  # add login panel UI function
  loginUI(id = "login"),

  # Establish content through whatever the router is running
  router$ui,

  # Last break and bottom banner
  div(
    class = "footer",
    textOutput("gitVers"),
  )
)

# Server side commands
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
  
  # Add the current version of the git repo
  output$gitVers <- renderText({
    sortedTagList <- git2r::tags(repo = '.')[order(names(git2r::tags(repo = ".")))]
    tagVers <- names(sortedTagList)[length(sortedTagList)]
    sprintf('Version: %s', tagVers)
  })
  
  # Run the modules if lig is successful
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) { 
      router$server(input, output, session)
      
      callModule(introduction$server, "introduction")
      callModule(gene_consult$server, "gene_consult")
      callModule(model_consult$server, "model_consult")
      callModule(model_comparison$server, "model_comparison")
      callModule(gene_groups$server, "gene_groups")
      callModule(FAQ$server, "FAQ")
      callModule(contact$server, "contact")
    }
  })
}

#app <- shinyApp(ui, server)
shinyApp(ui, server)

#runApp(app)
