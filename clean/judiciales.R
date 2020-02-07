library(tidyverse)
library(openxlsx)


elegidos <- read.xlsx('data/candidatos/BASE-GENERAL-CUENTAS-CLARAS.xlsx')
elegidos <- elegidos %>% filter(Elegido == 'Si')
judiciales <- elegidos %>% filter(Tipo.Persona == 'Persona Jurídica')

#correr de clean.R hasta secop_temp
secop_temp$contrato <- 'Si'

judiciales$Identificación.Normalizada <- as.character(judiciales$Identificación.Normalizada)
id_contrata <- judiciales %>%
  left_join(secop_temp,
            by = c('Identificación.Normalizada' = 'contratista_id'))

id_contrata$contrato[is.na(id_contrata$contrato)] <- 'No'

library(hgchmagic)
id_contrata <- id_contrata %>% distinct(Identificación.Normalizada, .keep_all = TRUE)
v <- hgch_bar_Cat(id_contrata %>% select(contrato), opts = c(list(percentage = T, horLabel = 'Porcentaje de financiadores con contratos con el Estado.', verLabel = ' '), opts_viz))
saveWidget(v, 'judiciales.html', background = 'transparent')
