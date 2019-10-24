library(tidyverse)
library(DT)

data <- read_csv('data/contratos.csv')
data$cont_valor_tot <- format(data$cont_valor_tot, big.mark = ',', small.mark = '.', scientific = F)
data <- head(data)





datatable(data,
          options = list(
            #pageLength = 3, 
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

