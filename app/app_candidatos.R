library(shiny)
library(shinyWidgets)
library(tidyverse)
library(visNetwork)
library(DT)
library(dsCustom)
library(formattable)

# Data
candidatos <- read_csv('data/candidatos.csv')
aportes <- read_csv('data/aportes.csv')
nodes <- read_csv('data/nodes.csv', col_types = cols(.default = "c"))
nodes$Valor <- as.numeric(nodes$Valor)
edges <- read_csv('data/edges.csv')
contratos <- read_csv('data/contratos.csv')
dic_contratos <- read_csv('data/dic.csv', col_types = cols(.default = "c"))

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
                div(class = 'search-candidate',
                    h1(class = 'title text-aqua', 'búsqueda'),
                    uiOutput('buscador_candidato')),
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
    verbatimTextOutput('salida'),
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
                        visNetworkOutput('vizRed', width = '100%', height = 750))),
                div(class = 'candidato',
                    h4('datos del candidato'),
                    div(class = 'panel',
                        uiOutput('candidato_info'))),
                div(class = 'contributor' ,
                    h4('datos del financiador'),
                    div(class = 'panel',
                        uiOutput('ficha_financiador'))),
                div(class = 'contracting',
                    h4('información del financiador en secop'),
                    div(class = 'panel',
                        formattableOutput('ficha_contrata'),
                        uiOutput('fecha_contr'))),
                div(class = 'others',
                    h4('otros candidatos financiados'),
                    div(class = 'panel',
                        uiOutput('otros_candidatos'))),
                div(class = 'table',
                    h4('tabla de contratos del financiador'),
                    div(class = 'panel',
                        dataTableOutput('data_secop1'),
                        br(),
                        br(),
                        dataTableOutput('data_secop2')
                    ))
            )
        )
    )
  )

server <-
  function(input, output, session) {
    
    # buscador
    
    output$buscador_candidato <- renderUI({
      temp_cand <- c(sort(unique(candidatos$name)), unique(candidatos$id))
      searchInput('id_candidato', temp_cand)
    })
    
    candidato <- reactiveValues(buscado = NULL)
    
    observe({
      busqueda <- input$id_candidato
      if (is.null(busqueda)) return()
      temp <- c('SERGIO FAJARDO VALDERRAMA', 'IVAN DUQUE MARQUEZ','AURELIJUS RUTENIS ANTANAS MOCKUS SIVICKAS', 'ARTURO CHAR CHALJUB', 'GABRIEL JAIME VALLEJO CHUJFI' ,'ALVARO URIBE VELEZ', 'CESAR AUGUSTO ORTIZ ZORRO', 'GUSTAVO FRANCISCO PETRO URREGO', 'ENRIQUE PEÑALOSA LONDOÑO', 'EMMANUEL ENRIQUE ARANGO GOMEZ')
      temp <- sample(temp, 1)
      if (busqueda == "") {
        candidato$buscado <- temp
      } else {
        candidato$buscado <- busqueda
      }
    })
    
    # Input buscador
    candidato_buscado <- reactive({
      if (is.null(candidato$buscado)) return()
      persona <- tolower(iconv(candidato$buscado, "UTF-8", "ASCII//TRANSLIT"))
      #if (persona == "") persona <- 'alvaro uribe velez'
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
      cargo <- unique(dt$cargo)
      if (length(cargo) == 1) {
        r <- HTML(paste0("<h3 class = 'titles-filters text-blue'>CARGO</h3><div class = 'title text-blue'>", cargo, "</div>"))
      } else {
        r <- selectizeInput('id_cargo',  h3(class = 'titles-filters text-blue','CARGO'), unique(dt$cargo))
      }
      r
    })
    
    
    data_candidato <- reactive({
      dt <- candidato_buscado()
      if (is.null(dt)) return()
      cargo <- unique(dt$cargo)
      if (length(cargo) == 1) {
        dt <- dt
      } else {
        if (is.null(input$id_cargo)) return()
        dt <- candidato_buscado() %>% filter(cargo == input$id_cargo)
      }
      dt
    })
    
    output$candidato_campana <- renderUI({
      dt <- data_candidato()
      HTML(paste0("<h3 class = 'titles-filters text-blue'>CAMPAÑA</h3><div class = 'title text-blue'>", unique(dt$campaign), "</div>"))
    })
    
    
    
    # ficha info candidato
    output$candidato_info <- renderUI({
      d_i <- data_candidato()
      if (is.null(d_i)) return()
      
      HTML(paste0('<div class = "info-candidato">
                 <div class = "candidato-general">
                  <div class = "inp-i max-sev"><span class = "ficha-titulos">Partido político</span></br><span class = "ficha-results">', d_i$party, '</span></div>
                  <div class = "inp-i"><span class = "ficha-titulos">Número Aportes</span></br><span class = "ficha-results">', d_i$total, '</span></div>
                  </div>
                  <div class = "candidato-elegido">
                  <div class = "inp-i max-sev"><span class = "ficha-titulos">Elegido</span></br><span class = "ficha-results"">', d_i$elegido ,'</span></div>
                  <div class = "inp-i"><span class = "ficha-titulos">Valor Aportes</span></br><span class = "ficha-results">$', format(d_i$valor, scientific = F, big.mark = ',', small.mark = '.'), '</span></div>
                  </div>
                  </div>'))
    })
    
    
    # candidato filtrado
    candidato_filter <- reactive({
      if (is.null(candidato_buscado())) return()
      candidato <- unique(candidato_buscado()$id)
      cargo <- input$id_cargo
      
      if (cargo == "") {
        edges_filter <- edges %>%
          filter(from == candidato) %>%
          distinct(to, .keep_all = T)
      } else {
        edges_filter <- edges %>%
          filter(from == candidato, cargo %in% cargo) %>%
          distinct(to, .keep_all = T)
      }
      if (nrow(edges_filter) == 0) return()
      nodes_filter <- nodes %>%
        filter(id %in%  c(unique(edges_filter$from), unique(edges_filter$to))) %>%
        group_by(id, node_type) %>%
        dplyr::summarise(label = paste(unique(label), collapse = '-'),
                         group = paste(unique(group), collapse = '-'),
                         Valor = sum(Valor)) %>% distinct(id, .keep_all = T)
      
      nodes_filter$borderWidth <- 1
      nodes_filter$font.size <- 30
      nodes_filter$size <- scales::rescale(nodes_filter$Valor, to = c(25, 75))
      nodes_filter$size <- ifelse(nodes_filter$node_type == 'candidato', 85, nodes_filter$size)
      list(edges = edges_filter, nodes = nodes_filter)
    })
    
    
    # Red de aportantes con contratos
    
    output$vizRed <- renderVisNetwork({
      if (is.null(candidato_filter())) return()
      visNetwork(candidato_filter()$nodes,  candidato_filter()$edges) %>%
        visGroups(groupname = "Persona Natural", color = "#C250C2") %>%
        visGroups(groupname = "Persona Jurídica", color = "#137FC0") %>%
        visGroups(groupname = "Candidato", color = "#0A446B") %>%
        visPhysics(
          stabilization = FALSE,
          barnesHut = list(
            gravitationalConstant = -10000,
            springConstant = 0.002,
            springLength = 100
          )) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visEvents(
          click = "function(nodes) {
        Shiny.onInputChange('clickNode', {nodes : nodes.nodes[0]});
        ;}"
        )
    })
    
    
    
    # Información aportante
    aportante_filter <- reactive({
      id_click <- input$clickNode$nodes
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
      click_ref <-  HTML(
        '<div class = "info-ficha"><img src="click.svg" style="width: 50px; display:block;margin-left: 40%;"/>
      <br/>
      <p class = "info-ficha">Haz click en algún financiador para ver su información detallada</p>
      </div>')
      if (is.null(aportante_filter())) {
        tx <- click_ref
      } else {
        options(scipen = 9990)
        dt <- aporte_candidato()
        total_aporte <- format(sum(dt$value), big.mark = ',', small.mark = '.')
        tx <- div(class = 'ficha_aportante',
                  HTML(paste0('
                  <div class = "financiador-general">
                  <div class = "inp-i" style = "width:60%"><span class = "ficha-titulos">Razón social</span> <br/>
                  <span class = "ficha-results">', dt$APORTANTE.NORMALIZADO,'</span></div>
                  <div class = "inp-i"><span class = "ficha-titulos">',dt$Tipo.de.Identificación, ' </span><br/>
                  <span class = "ficha-results">', dt$Identificación.Normalizada,'</span></div>
                  </div>
                  <div class = "financiador-elegido">
                  <div class = "inp-i" style = "width:60%"><span class = "ficha-titulos">Ciudad aporte</span><br/>
                  <span class = "ficha-results">', dt$Ciudad.Ingreso,'</span></div> 
                  <div class = "inp-i"><span class = "ficha-titulos">Monto de financiación </span><br/> 
                  <span class = "ficha-results">$', total_aporte,'</span></div>
                  </div>')),
                  tags$button(id = unique(dt$Identificación.Normalizada), class = 'click_ficha', 'Ver más')
        )
      }
      tx
    })
    
    
    
    # Red financiadores
    output$red_financiadores <- renderPrint({
      aportante_filter()
    })
    
    # observeEvent(input$last_case, {
    #   
    #   js_test <- tags$script(JS('alert("hola zorro")'))
    #   
    #   insertUI("body", "beforeEnd", ui = js_test, immediate = T)
    #   
    #   # showModal(modalDialog(
    #   #   title = "ACÁ VA LA RED",
    #   #   verbatimTextOutput('red_financiadores'),
    #   #   easyClose = TRUE,
    #   #   footer = NULL
    #   # ))
    # })
    
    
    
    # Ficha contratos
    contratos_info <- reactive({
      id_aportante <-  input$clickNode$nodes
      if (is.null(id_aportante)) return()
      dc <- contratos %>% filter(contratista_id == id_aportante | rep_legal_id == id_aportante)
      if (nrow(dc) == 0) return()
      dc
    })
    
    output$fecha_contr <- renderUI({
      d_c <- contratos_info()
      if (is.null(d_c)) return()
      options(scipen = 9999)
      fechas <- as.numeric(unique(d_c$cont_firma_ano))
      if (length(unique(fechas)) == 1) {
        inf_fecha <- HTML(paste0('<div class = "ficha-results"><span class = "ficha-titulos">Periodo de contratación: </span>', fechas, '</div>'))
      } else {
        inf_fecha <- HTML(paste0('<div class = "ficha-results"><span class = "ficha-titulos">Periodo de contratación: </span>', min(fechas), ' - ', max(fechas), '</div>'))
      }
      inf_fecha  
    })
    
    output$ficha_contrata <- renderFormattable({
      d_c <- contratos_info()
      if (is.null(d_c)) return()
      options(scipen = 9999)
      resumen <- contratos_info() %>% 
        group_by(Secop = secop, Moneda = moneda) %>% 
        dplyr::summarise(Valor = sum(as.numeric(cont_valor_tot), na.rm = T), Total = n()) %>% 
        arrange(-Valor)
      resumen$Valor <- format(resumen$Valor, big.mark = ',', small.mark = '.')
      formattable::formattable(resumen)
    })
    
    # imprimir contratos_info en dos tablas)
    output$data_secop1 <- renderDataTable({
      dt <- contratos_info()
      if (is.null(dt)) return()
  
      if (length(unique(dt$secop)) == 1) {
        if (unique(dt$secop == 'Uno')) {
          dt <- dt
        } else {
          return()
        }
      } else {
        dt <- dt %>% filter(secop == 'Uno')
      }
      
      dt <- dt %>% select(-secop, -cont_objeto_det)
      dt <- Filter(function(x) !all(is.na(x)), dt)
      dic <- dic_contratos %>% filter(secop ==  'uno')
      dic_filt <- data.frame(id = as.character(names(dt)))
      dic_filt <- inner_join(dic_filt, dic)
      names(dt) <- dic_filt$label
      
      pg <- nrow(dt)
      if (nrow(dt) > 3) pg <- 4
      
      datatable(dt,
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'Table 1: ', htmltools::em('Información registrada en Secop 1')
                ),
                options = list(
                  pageLength = pg, 
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  lengthChange = F,
                  rownames = F,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#0A446B', 'color': '#fff'});",
                    "}"),
                  searching = FALSE
                )) %>% 
        formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='11px', lineHeight='15px')
      
    })
  
    
    output$data_secop2 <- renderDataTable({
      dt <- contratos_info()
      if (is.null(dt)) return()
      
      if (length(unique(dt$secop)) == 1) {
        if (unique(dt$secop == 'Dos')) {
          dt <- dt
        } else {
          return()
        }
      } else {
        dt <- dt %>% filter(secop == 'Dos')
      }
      
      dt <- dt %>% select(-secop, -cont_objeto_det)
      dt <- Filter(function(x) !all(is.na(x)), dt)
      dic <- dic_contratos %>% filter(secop ==  'dos')
      dic_filt <- data.frame(id = as.character(names(dt)))
      dic_filt <- inner_join(dic_filt, dic)
      names(dt) <- dic_filt$label
      pg <- nrow(dt)
      if (nrow(dt) > 3) pg <- 4
      datatable(dt,
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'Table 2: ', htmltools::em('Información registrada en Secop 2')
                ),
                options = list(
                  pageLength = pg, 
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  lengthChange = F,
                  rownames = F,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#0A446B', 'color': '#fff'});",
                    "}"),
                  searching = FALSE
                )) %>% 
        formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='11px', lineHeight='15px')
      
    })
    
    
    # otros candidatos financiados
    output$otros_candidatos <- renderUI({
      dt <- candidato_buscado()
      if (is.null(dt) | is.null(aportante_filter())) return()
      id_candidato <- unique(dt$id)
      info <- aportante_filter() %>% filter(Identificacion.Candidato != id_candidato)
      
      if (nrow(info) == 0) {
        res <- HTML('<div class = "ficha-results">No ha financiado otros candidatos</div>')
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
    
    # observeEvent(input$last_cand, {
    #   opts <- input$last_cand
    #   candidato$buscado <- opts
    # })
    
    
  }

shinyApp(ui, server)