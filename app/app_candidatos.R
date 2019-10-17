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
contratos <- read_csv('data/contratos.csv')

ui <- 
  fluidPage(
    suppressDependencies("bootstrap"),
    tags$head(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      tags$link(rel="stylesheet", type="text/css", href="styles.css"),
      includeScript("js/iframeSizer.contentWindow.min.js"),
      includeScript("js/elecciones.js")
    ),
  div(class = 'bg-blue seccion_busqueda',
      tags$img(class = 'line-decoration', src='divider_blue_large.png'),
      div(class = 'content',
        div(class = 'flex justify-between items-center',
          h1(class = 'title text-aqua', 'búsqueda'),
  uiOutput('buscador_candidato'),
  div(class = 'texto_busqueda',
      h1(class = 'title text-aqua', 'en esta sección'),
   p(class = 'general-text text-white',
'podrás conocer de dónde provino la financiación de las
campañas electorales de distintas figuras políticas en los
útimos períodos de elección popular.'
     )
    )
   )
  )
),
  uiOutput('candidato_general'),
  visNetworkOutput('vizRed'),
  uiOutput('ficha_financiador'),
  uiOutput('ficha_contrata'),
  uiOutput('otros_candidatos')
  )

server <-
  function(input, output, session) {
    
    # buscador
   
    output$buscador_candidato <- renderUI({
      
      div(class = '',
      searchInput('id_candidato', label = ' ', value = "", placeholder = 'Nombre o cédula del candidato',
                  btnSearch = icon("search"), btnReset = icon("remove"), resetValue = "")
         )
      #selectizeInput('id_candidato', 'BÚSQUEDA',  choices = NULL, selected = NULL, options = list(placeholder = 'Nombre o cédula del candidato'))
     #textInput('id_candidato',  label = 'BÚSQUEDA', value = "", placeholder = 'Nombre o cédula del candidato')
      #  opts = c(unique(candidatos$name), unique(candidatos$id))
      # autocomplete_input('id_candidato', 'id_candidato', opts, max_options = 10000, placeholder = 'Nombre o cédula del candidato')
    })
    
    
    # observe({
    #   if (is.null(input$id_candidato)) return()
    #   id_c <-  isolate(tolower(iconv(input$id_candidato, "UTF-8", "ASCII//TRANSLIT")))
    #   cand_filt <- candidatos$name[grep(id_c, unique(candidatos$name_id))]
    #   updateSelectizeInput(session, 'id_candidato', choices = cand_filt)
    # })
    
    # Input buscador
    candidato_buscado <- reactive({
      # if (is.null(input$id_candidato)) return()
      # persona <- tolower(iconv(input$id_candidato, "UTF-8", "ASCII//TRANSLIT"))
      # if (persona == "") return()
      persona <- 'alvaro uribe velez'
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
     
     
     
     # Red financiadores
     output$red_financiadores <- renderPrint({
       aportante_filter()
     })
     
     
     observeEvent(input$last_case, {
       showModal(modalDialog(
         title = "ACÁ VA LA RED",
         verbatimTextOutput('red_financiadores'),
         easyClose = TRUE,
         footer = NULL
       ))
     })
     
     
     # Ficha contratos
     contratos_info <- reactive({
      id_aportante <-  input$clickNode
      if (is.null(id_aportante)) return(HTML('acá va un texto'))
      dc <- contratos %>% filter(contratista_id == id_aportante | rep_legal_id == id_aportante)
      if (nrow(dc) == 0) return()
      dc
     })
     
     output$ficha_contrata <- renderUI({
       if (is.null(contratos_info()) | sum(class(contratos_info()) == 'html') == 1) return('aca va otro texto')
      options(scipen = 9999)
      resumen <- contratos_info() %>%
                  dplyr::summarise(Valor = sum(as.numeric(cont_valor_tot), na.rm = T), Total = n())
      div(
        format(resumen$Valor, big.mark = ',', small.mark = '.'),
        resumen$Total
      )
     })
     
     # imprimir contratos_info en dos tablas
     #output$data_secop1
     #output$data_secop2
     
     # otros candidatos financiados
     output$otros_candidatos <- renderUI({
       dt <- candidato_buscado()
       if (is.null(dt) | is.null(aportante_filter())) return()
       id_candidato <- unique(dt$id)
       info <- aportante_filter() %>% filter(Identificacion.Candidato != id_candidato)
       
       if (nrow(info) == 0) {
        res <- HTML('No ha financiado otros candidatos')
       } else {
        res <- map(1:nrow(info), function(i) {
            tags$button(
            id = info$Nombre.Candidato[i],
            class = 'otros_candidatos',
            info$Nombre.Candidato[i]
          )
        })
       }
       
       res
     })
     
     observeEvent(input$last_cand, {
       updateSearchInput(session, 'id_candidato', value = input$last_cand)
       #update_autocomplete_input(session, 'id_candidato', value = input$last_cand)
       #updateTextInput(session, 'id_candidato', value = input$last_cand)
     })
    

  }

shinyApp(ui, server)