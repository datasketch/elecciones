library(tidyverse)

# contratos funcionamiento
contratos_func <- read_csv('data_clean/secop_filter_funcionamiento.csv', col_types = cols(.default = "c"))

cont_vars <- c("cont_firma_ano", "contratista_id", "contratista_nombre", "rep_legal_id",
               "rep_legal_nombre","ent_nombre", "ent_nit", "cont_valor_tot", "moneda", "proc_tipo",
               "proc_status", "cont_tipo", "cont_objeto","cont_objeto_det", "grupo", "ruta_secop1", "secop")

contratos2 <- contratos_func %>% 
                select(one_of(cont_vars))   

write_csv(contratos2, 'data/clean/contratos_funcionamiento.csv')

# información de partidos y sus aportantes

info_aport <- read_csv('data_clean/funcionamiento_filter.csv')
