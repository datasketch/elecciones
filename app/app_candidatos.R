library(shiny)
library(shinyWidgets)
library(tidyverse)
library(visNetwork)


# Data
candidatos <- read_csv('data/candidatos.csv')
aportes <- read_csv('data/aportes.csv')
nodes <- read_csv('data/nodes.csv', col_types = cols(.default = "c"))
nodes$value <- as.numeric(nodes$value)
edges <- read_csv('data/edges.csv')

ui <- 
  fluidPage(
  suppressDependencies("bootstrap"),
  uiOutput('buscador_candidato'),
  uiOutput('candidato_general'),
  visNetworkOutput('vizRed'),
  uiOutput('ficha_financiador'),
  verbatimTextOutput('test')
  )

server <-
  function(input, output, session) {
    
    # buscador
    output$buscador_candidato <- renderUI({
      searchInput('id_candidato', label = 'BÚSQUEDA', value = "", placeholder = 'Nombre o cédula del candidato',
                  btnSearch = icon("search"), btnReset = icon("remove"), resetValue = "")
    })
    
    # Input buscador
    candidato_buscado <- reactive({
      if (is.null(input$id_candidato)) return()
      persona <- tolower(iconv(input$id_candidato, "UTF-8", "ASCII//TRANSLIT"))
      if (persona == "") return()
      candidatos %>% filter(name_id == persona | id == persona)
    })
    
    # Información Candidato
    output$candidato_general <- renderUI({
      dt <- candidato_buscado()
      div(
        div(
        HTML('CANDIDATO'),
        unique(dt$name)
        ),
        div(
          selectizeInput('id_cargo', 'CARGO', unique(dt$cargo))
        ),
        div(
          selectizeInput('id_campana', 'CAMPAÑA', NULL)
        )
      )
    })
    
    
    observe({
      if (is.null(input$id_cargo) | is.null(candidato_buscado())) return()
      campana <- candidato_buscado() %>% filter(cargo == input$id_cargo)
      updateSelectizeInput(session, 'id_campana', choices = unique(campana$campaign))
    })
    
    
    # candidato filtrado
    candidato_filter <- reactive({
      if (is.null(candidato_buscado())) return()
      candidato <- unique(candidato_buscado()$id)
      campana <- input$id_campana
      edges_filter <- edges %>% 
                       filter(from == candidato, campaign %in% campana) %>% 
                        distinct(to, .keep_all = T)
      nodes_filter <- nodes %>% 
                       filter(id %in%  c(unique(edges_filter$from), unique(edges_filter$to))) %>% 
                        group_by(id) %>%
                         dplyr::summarise(label = paste(unique(name), collapse = '-'),
                                          group = paste(unique(group), collapse = '-'),
                                          value = sum(value))
      list(edges = edges_filter, nodes = nodes_filter)
    })
    

    # Red de aportantes con contratos
    
    output$vizRed <- renderVisNetwork({
      if (is.null(candidato_filter())) return()
      visNetwork(candidato_filter()$nodes,  candidato_filter()$edges) %>% 
        visEvents( click = "function(nodes) {
        Shiny.onInputChange('clickNode', {nodes : nodes.nodes[0]});
        ;}"
        )
    })
    
    # Información aportante
     aportante_filter <- reactive({
       id_click <- input$clickNode
       if (is.null(id_click)) return()
       aportante <- aportes %>% filter(Identificación.Normalizada == id_click)
       if (nrow(aportante) == 0) return()
       aportante
     })

     
     # Información aporte al candidato seleccionado
     aporte_candidato <- reactive({
       dt <- candidato_buscado()
       if (is.null(dt) | is.null(aportante_filter())) return()
       id_candidato <- unique(dt$id)
       aportante_filter() %>% filter(Identificacion.Candidato == id_candidato)
     })
     
     
     # Ficha 
     output$ficha_financiador <- renderUI({
       if (is.null(aportante_filter())) return(HTML('bla bla bla'))
       options(scipen = 9990)
       dt <- aporte_candidato()
       total_aporte <- format(sum(dt$value), big.mark = ',', small.mark = '.')
       div(class = 'ficha_aportante',
         dt$APORTANTE.NORMALIZADO,
         dt$Tipo.de.Identificación,
         dt$Identificación.Normalizada,
         dt$Ciudad.Ingreso,
         total_aporte,
         tags$button(id = dt$Identificación.Normalizada, class = "click_ficha",  "Ver más")
       )
     })
     
     output$test <- renderPrint({
      aporte_candidato() 
     })
     
    # 
    

  }

shinyApp(ui, server)