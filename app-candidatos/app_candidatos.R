library(shiny)
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
contratos <- read_csv('data/contratos.csv', col_types = cols(.default = "c"))
contratos$cont_valor_tot <- as.numeric(contratos$cont_valor_tot)
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
        tags$img(class = 'line-decoration', src='divider_large.png'),
        div(class = 'content',
            div(class = 'flex justify-between items-center',
                div(class = 'search-candidate',
                    h1(class = 'title text-aqua', 'búsqueda'),
                    uiOutput('buscador_candidato')),
                div(class = 'texto_busqueda',
                    h1(class = 'title text-aqua', 'en esta sección'),
                    p(class = 'general-text text-white',
                      'Podrás conocer los financiadores de campañas electorales (Territoriales 2015, Congreso y
                      Presidencia 2018) que tuvieron contratos con el Estado entre los años 2015 y 2019.'
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
                        visNetworkOutput('vizRed', width = '100%', height = 715),
                        uiOutput('legenda'))),
                div(class = 'candidato',
                    h4('otros datos del candidato'),
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
                        uiOutput('desc_secop1_out'),
                        br(),
                        br(),
                        dataTableOutput('data_secop2'),
                        uiOutput('desc_secop2_out')
                    ))
            )
        )
    ),
    div(id="info-modal", class="modal",
        div(class="modal-wrapper",
            div(class="modal-content"
            )))
  )

server <-
  function(input, output, session) {
    
    # buscador
    
    output$buscador_candidato <- renderUI({
      temp_cand <- c(sort(unique(candidatos$name)), unique(candidatos$id))
      searchInput('id_candidato', temp_cand, placeholder = 'Busca por nombre o cédula del candidato')
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
                  <div class = "inp-i"><span class = "ficha-titulos">Elegido</span></br><span class = "ficha-results"">', d_i$elegido ,'</span></div>
                  </div>
                  <div class = "candidato-elegido">
                  <div class = "inp-i max-sev"><span class = "ficha-titulos">Número Aportes</span></br><span class = "ficha-results">', d_i$total, '</span></div>
                  
                  <div class = "inp-i"><span class = "ficha-titulos">Total de aportes recibidos</span></br><span class = "ficha-results">$', format(d_i$Valor, scientific = F, big.mark = ',', small.mark = '.'), '</span></div>
                  </div>
                  </div>'))
    })
    
    
    # candidato filtrado
    candidato_filter <- reactive({
      if (is.null(candidato_buscado())) return()
      candidato <- unique(candidato_buscado()$id)
      uni_carg <- unique(candidato_buscado()$cargo)
      cargo <- input$id_cargo
      
      if (length(uni_carg) == 1)  cargo <-  uni_carg
      
      edges_filter <- edges %>%
        filter(from == candidato, cargo %in% cargo) %>%
        distinct(to, .keep_all = T)
      nodes <- nodes %>% filter(cargo %in% cargo) %>% distinct()
      
      if (nrow(edges_filter) == 0) return()
      nodes_filter <- nodes %>%
        filter(id %in%  c(unique(edges_filter$from), unique(edges_filter$to))) %>%
        group_by(id, node_type) %>%
        dplyr::summarise(label = paste(unique(label), collapse = '-'),
                         group = paste(unique(group)[1], collapse = '-'),
                         Valor = sum(Valor)) %>%
        distinct(id, .keep_all = T)
      
      nodes_filter$borderWidth <- 1
      nodes_filter$font.size <- 30
      nodes_filter$size <- scales::rescale(nodes_filter$Valor, to = c(25, 75))
      nodes_filter$size <- ifelse(nodes_filter$node_type == 'candidato', 85, nodes_filter$size)
      list(edges = edges_filter, nodes = nodes_filter)
    })
    
    output$legenda <- renderUI({
      summ <- candidato_filter()$nodes
      if (is.null(summ)) return()
      
      summ <- summ %>%
        group_by(group) %>% 
        summarise(Total = n()) %>% 
        filter(group != 'Candidato')
      
      total_jud <- ifelse('Persona Jurídica' %in% summ$group, summ$Total[summ$group == 'Persona Jurídica'], 0)
      total_nat <- ifelse('Persona Natural' %in% summ$group, summ$Total[summ$group == 'Persona Natural'], 0)
      
      HTML(paste0(
        '
        <div class = "border-legend">
        <div class = "content-legend">
        <div id = "circle-juridica"></div><div> Persona Jurídica (', total_jud,') </div>
        </div>
         <div class = "content-legend">
        <div id = "circle-natural"></div><div> Persona Natural (', total_nat,') </div>
        </div>
        </div>
        '
      ))
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
          startStabilizing = "function() {
            this.moveTo({scale:0.3})}",
          click = "function(nodes) {
        Shiny.onInputChange('clickNode', {nodes : nodes.nodes[0]});
        ;}"
        )
    })
    
    observe({
      if (!is.null(input$clickNode)) print(input$clickNode)
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
        total_aporte <- format(sum(dt$Valor), big.mark = ',', small.mark = '.')
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
                  actionButton('blabla', 'Ver más', class = 'click_ficha')
        )
      }
      tx
    })
    
    observeEvent(input$blabla, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Thank you for clicking')
    })
    
    
    
    # Red financiadores
    output$red_financiadores <- renderVisNetwork({
      info <- aportante_filter()
      if (is.null(info)) return()
      cand <- info %>%
        group_by(id = Identificacion.Candidato, name = Nombre.Candidato, label = name_cand_line) %>%
        summarise( Valor = sum(Valor)) %>% 
        mutate(group = "Candidato") 
      
      aport <- info %>%
        select(id = Identificación.Normalizada, name = APORTANTE.NORMALIZADO, group, label = name_aport_line) 
      
      nodes <- bind_rows(list(Candidato = cand, Aportante = aport), .id = "node_type")
      
      nodes$Valor <- ifelse(nodes$node_type == 'Aportante', 10, nodes$Valor)
      nodes <- nodes %>% distinct(id, .keep_all = T)
      edges <-  info[c("Identificacion.Candidato", "Identificación.Normalizada")] 
      edges <- edges %>%
        select(from = Identificación.Normalizada, 
               to = Identificacion.Candidato) %>% 
        distinct(to, .keep_all = T)
      
      nodes$borderWidth <- 1
      nodes$font.size <- 25
      nodes$size <- scales::rescale(nodes$Valor, to = c(25, 75))
      nodes$size <- ifelse(nodes$node_type == 'Aportante', 85, nodes$size)
      
      visNetwork(nodes, edges) %>%
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
        visInteraction(navigationButtons = TRUE)
    })
    
    # tabla de aportantes
    output$tabla_aportantes <- renderDataTable({
      dt <- aportante_filter()
      if (is.null(dt)) return()
      dt$Valor <- format(dt$Valor, big.mark = ',', small.mark = '.')
      dt  <- dt %>% select('Candidato financiado' = Nombre.Candidato,  'Partido político' = Organizacion.Politica, 'Ciudad aporte' = Ciudad.Ingreso, 'Valor aporte' = Valor, 'Campaña' = campaign, 'Cargo' = cargo, Elegido)
      pg <- nrow(dt)
      if (nrow(dt) > 5) pg <- 6
      datatable(dt,
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'Tabla: ', htmltools::em('Información candidatos financiados')
                ),
                rownames = F,
                options = list(
                  pageLength = pg, 
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  lengthChange = F,
                  scrollX = T,
                  scrollY = T,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#4D4D4D', 'color': '#fff', 'font-size':'17px'});",
                    "}"),
                  searching = FALSE
                )) %>% 
        formatStyle( 0 , target= 'row',color = '#0A446B')
      
    })
    
    
    
    # Ficha contratos
    contratos_info <- reactive({
      id_aportante <-  input$clickNode$nodes
      if (is.null(id_aportante)) return()
      dc <- contratos %>% filter(contratista_id == id_aportante | rep_legal_id == id_aportante)
      if (nrow(dc) == 0) return()
      dc
    })
    
    
    output$info_ver_mas <- renderUI({
      dt <- aportante_filter()
      div(
        div(class = 'nombre-fin',
            h3(class = 'titles-filters text-blue', 'FINANCIADOR'),
            div(class = 'title text-blue',
                unique(dt$APORTANTE.NORMALIZADO)
            )),
        div(class = 'results-fin',
            div(class = 'red-aportantes panel-fin',
                visNetworkOutput('red_financiadores', height = '100%', width = '100%')),
            div(class = 'tabla-aportantes panel-fin', 
                dataTableOutput('tabla_aportantes',  width = '100%'),
                uiOutput('desc_aptr_out')
            )))
    })
    
    
    output$desc_aptr_out <- renderUI({
      if (is.null(aportante_filter())) return()
      if (nrow(aportante_filter()) == 0) return()
      downloadButton('descarga_info_apt', 'Descarga de vista')
    })
    
    output$descarga_info_apt <- downloadHandler(
      filename = function() {
        "data-apts.csv"
      },
      content = function(file) {
        data <- aportante_filter()
        write_csv(data, file, na = '')
      }
    ) 
    
    observeEvent(input$clickNode$nodes, {
      insertUI('.modal-content', ui = uiOutput('info_ver_mas'))
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
        group_by(SECOP = secop, Moneda = moneda) %>% 
        dplyr::summarise(Valor = sum(as.numeric(cont_valor_tot), na.rm = T), `Total de contratos` = n()) %>% 
        arrange(-Valor)
      resumen$Valor <- format(resumen$Valor, big.mark = ',', small.mark = '.')
      formattable::formattable(resumen)
    })
    
    # imprimir contratos_info en dos tablas
    filter_secopI <- reactive({
      dt <- contratos_info()
      if (is.null(dt)) return()
      
      dt$cont_valor_tot <- format(dt$cont_valor_tot, big.mark = ',', small.mark = '.')
      
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
      dt
    })
    
    output$data_secop1 <- renderDataTable({
      
      dt <- filter_secopI()
      if (is.null(dt)) return()
      pg <- nrow(dt)
      if (nrow(dt) > 3) pg <- 4
      
      datatable(dt,
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'Tabla 1: ', htmltools::em('Información registrada en SECOP 1')
                ),
                rownames = F,
                options = list(
                  pageLength = pg, 
                  scrollX = T,
                  scrollY = T,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  lengthChange = F,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#0A446B', 'color': '#fff'});",
                    "}"),
                  searching = FALSE
                )) %>% 
        formatStyle( 0, target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
      
    })
    
    
    output$desc_secop1_out <- renderUI({
      if (is.null(filter_secopI())) return()
      if (nrow(filter_secopI()) == 0) return()
      downloadButton('descarga_secop1', 'Descarga vista SECOP 1')
    })
    
    output$descarga_secop1 <- downloadHandler(
      filename = function() {
        "data-secop1.csv"
      },
      content = function(file) {
        data <- filter_secopI()
        write_csv(data, file, na = '')
      }
    )
    
    
    filter_secopII <- reactive({
      dt <- contratos_info()
      if (is.null(dt)) return()
      dt$cont_valor_tot <- format(dt$cont_valor_tot, big.mark = ',', small.mark = '.')
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
      dic_filt <- left_join(dic_filt, dic)
      dic_filt$label <- coalesce(dic_filt$label, dic_filt$id)
      names(dt) <- dic_filt$label
      dt
    })
    
    output$data_secop2 <- renderDataTable({
      dt <- filter_secopII()
      if (is.null(dt)) return()
      pg <- nrow(dt)
      if (nrow(dt) > 3) pg <- 4
      datatable(dt,
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'Tabla 2: ', htmltools::em('Información registrada en SECOP 2')
                ),
                rownames = F,
                options = list(
                  pageLength = pg, 
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  lengthChange = F,
                  scrollX = T,
                  scrollY = T,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#0A446B', 'color': '#fff'});",
                    "}"),
                  searching = FALSE
                )) %>% 
        formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
      
    })
    
    output$desc_secop2_out <- renderUI({
      if (is.null(filter_secopII())) return()
      if (nrow(filter_secopII()) == 0) return()
      downloadButton('descarga_secop2', 'Descarga vista SECOP 2')
    })
    
    output$descarga_secop2 <- downloadHandler(
      filename = function() {
        "data-secop2.csv"
      },
      content = function(file) {
        data <- filter_secopII()
        write_csv(data, file, na = '')
      }
    )
    
    
    
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
    
    observeEvent(input$last_cand, {
      session$sendCustomMessage(type = 'otros_candidatos',
                                message = input$last_cand)
    })
    
    
  }

shinyApp(ui, server)