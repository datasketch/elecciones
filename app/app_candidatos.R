library(shiny)
library(shinyWidgets)
library(tidyverse)
library(visNetwork)


# Data
candidatos <- read_csv('data/candidatos.csv')
aportes <- read_csv('data/aportes.csv')
nodes <- read_csv('data/nodes.csv', col_types = cols(.default = "c"))
nodes$Valor <- as.numeric(nodes$Valor)
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
        #tags$img(class = 'line-decoration', src='divider_blue_large.png'),
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
    div(class = 'filters',
        div(class = 'content',
            div(class = 'flex justify-between items-start flex-wrap',
                div( class = 'filter-output',
                     uiOutput('candidato_nombre')
                ),
                div( class = 'filter-output',
                     uiOutput('candidato_cargo')
                ),
                div( class = 'filter-output',
                     uiOutput('candidato_campana')
                )
            ))),
    div(class = 'summary',
        div(class = 'content',
            div(class = 'results',
                div(class = 'red',
                      h4('red de financiadores'),
                    div(class = 'panel',
                    visNetworkOutput('vizRed', width = '100%', height = 550))),
                div(class = 'contributor' ,
                    h4('datos del financiador'),
                    div(class = 'panel',
                    uiOutput('ficha_financiador'))),
                div(class = 'contracting',
                    h4('información del financiador en secop'),
                    div(class = 'panel',
                    uiOutput('ficha_contrata'))),
                div(class = 'others',
                    h4('otros candidatos financiados'),
                    div(class = 'panel',
                    uiOutput('otros_candidatos'))),
                div(class = 'table',
                    h4('tabla de contratos del financiador'),
                    div(class = 'panel',
                    verbatimTextOutput('data_secop1')
                ))
                )
        )
    )
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
      if (is.null(input$id_candidato)) return()
      persona <- tolower(iconv(input$id_candidato, "UTF-8", "ASCII//TRANSLIT"))
      if (persona == "") persona <- 'alvaro uribe velez'
      candidatos %>% filter(name_id == persona | id == persona)
    })
    
    # Información Candidato
    output$candidato_nombre <- renderUI({
      dt <- candidato_buscado()
      div(
        h3(class = 'titles-filters text-blue', 'CANDIDATO'),
        div(class = 'title text-blue',
            unique(dt$name)
        )
      )
    })
    
    output$candidato_cargo <- renderUI({
      dt <- candidato_buscado()
      selectizeInput('id_cargo',  h3(class = 'titles-filters text-blue','CARGO'), unique(dt$cargo)
      )
    })
    
    output$candidato_campana <- renderUI({
      dt <- candidato_buscado()
      selectizeInput('id_campana',  h3(class = 'titles-filters text-blue', 'CAMPAÑA'), NULL)
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
        group_by(id, node_type) %>%
        dplyr::summarise(label = paste(unique(label), collapse = '-'),
                         group = paste(unique(group), collapse = '-'),
                         Valor = sum(Valor))
    
      nodes_filter$borderWidth <- 1
      nodes_filter$font.size <- 30
      nodes_filter$size <- scales::rescale(nodes_filter$Valor, to = c(25, 75))
      nodes_filter$size <- ifelse(nodes_filter$node_type == 'candidato', 85, nodes_filter$size)
      list(edges = edges_filter, nodes = nodes_filter)
    })
    
    
    # Red de aportantes con contratos
    
    output$vizRed <- renderVisNetwork({
      if (is.null(candidato_filter())) return()
      visNetwork(candidato_filter()$nodes,  candidato_filter()$edges,  style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;") %>% 
        visGroups(groupname = "Persona Natural", color = "#C250C2") %>%
        visGroups(groupname = "Persona Jurídica", color = "#137FC0") %>% 
        visGroups(groupname = "Candidato", color = "#0A446B") %>% 
        visPhysics(barnesHut = list(
          gravitationalConstant = -10000,
          springConstant = 0.002,
          springLength = 100
        )) %>% 
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
      if (is.null(aportante_filter())) {
        HTML(
          '<div class = "info-ficha"><img src="click.svg" style="width: 50px; display:block;margin-left: 40%;"/>
      <br/>
      <p class = "info-ficha">Haz click en algún financiador para ver su información detallada</p>
      </div>')
      } else {
      options(scipen = 9990)
      dt <- aporte_candidato()
      print(dt)
      total_aporte <- format(sum(dt$value), big.mark = ',', small.mark = '.')
      tx <- div(class = 'ficha_aportante',
                HTML(paste0('<div><div>Razón social <br/>',      
                dt$APORTANTE.NORMALIZADO,'</div>',
                '<div>Parentesco <br/>',
                dt$Parentesco, '</div>',
                '<div>Monto de financiación <br/> $',
                total_aporte,'</div></div>
                <div><div>',
                dt$Tipo.de.Identificación, ' <br/>',
                dt$Identificación.Normalizada,'</div>',
                '<div>Ciudad <br/>',
                dt$Ciudad.Ingreso,'</div>',
                tags$button(id = dt$Identificación.Normalizada, class = "click_ficha",  "Ver más"), '</div>'
      )))
      }
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
      if (is.null(contratos_info()) | sum(class(contratos_info()) == 'html') == 1) return()
      options(scipen = 9999)
      resumen <- contratos_info() %>%
        dplyr::summarise(Valor = sum(as.numeric(cont_valor_tot), na.rm = T), Total = n())
      div(
        format(resumen$Valor, big.mark = ',', small.mark = '.'),
        resumen$Total
      )
    })
    
    # imprimir contratos_info en dos tablas
    output$data_secop1 <- renderPrint({
      if (is.null(contratos_info()) | sum(class(contratos_info()) == 'html') == 1) return('aca va la tabla')
      contratos_info()  
    })
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
            class = 'others-info',
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