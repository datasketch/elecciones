library(shiny)
library(visNetwork)

ui <- fluidPage(
  fluidRow(
    uiOutput("search"),
    verbatimTextOutput("debug")
  ),
  fluidRow(
   uiOutput("select") 
  ),
  fluidRow(
    column(6, visNetworkOutput("network")),
    column(6,
           uiOutput("profile_box"),
           uiOutput("profile_secop"),
           uiOutput("profile_more")
           )
  )
)

server <- function(input, output, session) {
  candidatos <- read_csv("data/clean/candidatos.csv")
  candidatos <- candidatos %>% slice(1:5000)
  candidates <- candidatos %>% pull(id) %>% set_names(candidatos$name)
  
  nodes <- read_csv("data/clean/nodes.csv")
  edges <- read_csv("data/clean/edges.csv")
  
  output$search <- renderUI({
    list(
      column(6, selectizeInput("selected_candidate", "Buscar candidato", candidates))
    )
  }) 
  
  selected_candidate_info <- reactive({
    candidatos %>% filter(id == input$selected_candidate) %>% select(id, name, cargo, campaign)
  })
  available_campaign <- reactive({
    selected_candidate_info()$campaign
    #candidatos %>% filter(id == selected_candidate_info()$id) %>% pull(campaign)
  })
  selected_campaign <- reactive({
    input$selected_campaign
  })
  
  output$select <- renderUI({
    campaigns <- available_campaign()
    if(is.null(campaigns)) return()
    tagList(
      column(4, h2(selected_candidate_info()$name)),
      column(4, h2(selected_candidate_info()$cargo)),
      column(4, selectInput("selected_campaign","Campaña", campaigns))
    )
  })
  
  output$debug <- renderPrint({
    #selected_candidate_info()
    selected_aportante()
  })
  
  output$network <- renderVisNetwork({
    eds <- edges %>% 
      filter(campaign == selected_campaign()) %>% 
      filter(from %in% selected_candidate_info()$id | to %in% selected_candidate_info()$id)
    nds <- nodes %>% filter(id %in% c(eds$from, eds$to))
    
    nds <- nds %>% mutate(label = name, group = type, value = amount) %>% distinct(id, .keep_all = TRUE)
    
    visNetwork(nds, eds) %>%
      #visLegend() %>%
      #visGroups(groupname = "Persona Natural", color = "red") %>%
      #visGroups(groupname = "Persona Jurídica", color = "yellow") %>% 
      visIgraphLayout(layout="layout_nicely") %>% 
      #visNodes(scaling = list(label = list(enabled = T))) # texto proporcional al tamaño del circulo
      visEvents(
        click = "function(nodes) {
        console.info('click')
        console.info(nodes)
        Shiny.onInputChange('clickNode', {nodes : nodes.nodes[0]});
        ;}"
      )
  })
 
  selected_aportante <- reactive({
    if(is.null(input$clickNode)) return()
    nodes %>% filter(id == input$clickNode$nodes)
    
  })
  
  output$profile_box <- renderUI({
    if(is.null(selected_aportante())) return()
    aportante <- selected_aportante()
    list(
      p("Nombre"), h3(aportante$name),
      p("Identificación"), h3(aportante$id),
      p("Municipio"), h3(aportante$municipio),
      p("Departamento"), h3(aportante$departamento),
      p("Monto aporte (millones)"), h3(aportante$amount)
      
    )
  }) 
   
}

shinyApp(ui, server)


