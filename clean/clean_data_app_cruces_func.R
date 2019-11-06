library(tidyverse)
source('~/Repos/elecciones/clean/tools.R')

contratos_func <- read_csv('data_clean/secop_filter_funcionamiento.csv', col_types = cols(.default = "c"))
cont_vars <- c("cont_firma_ano", "contratista_id", "contratista_nombre", "rep_legal_id",
               "rep_legal_nombre","ent_nombre", "ent_nit", "ent_nivel", "cont_valor_tot", "moneda", "proc_tipo",
               "proc_status", "cont_tipo", "cont_objeto","cont_objeto_det", "grupo", "ruta_secop1", "secop")

contratos2 <- contratos_func %>% 
  select(one_of(cont_vars))


write_csv(contratos2, 'data/clean/contratos_funcionamiento.csv')


source('~/Repos/elecciones/clean/tools.R')
aportes <- read_csv("data_clean/funcionamiento_filter.csv", col_types = cols(.default = "c"))
aportes <- aportes %>% group_by(NOMBRE_ORGANIZACION) %>% mutate(id_part = sample(100:300, 1))


aportes_cand <- aportes  %>% 
  group_by(NOMBRE_ORGANIZACION, id_part, informe = ANO_INFORME, APORTANTE, IDENTIFICACION_NORM, CONCEPTO, DEPARTAMENTO_INGRESO) %>% 
  summarise(Valor = sum(as_number(VALOR)),
            Ciudad.Ingreso = paste(unique(CIUDAD_INGRESO), collapse = ' y '),
            Tipo.Persona = paste(unique(TIPO_PERSONA)[1], collapse = ' - '),
            Tipo.de.Identificación = paste(unique(TIPO_IDENTIFICACION)[1], collapse = ' - '))


write_csv(aportes_cand, 'data/clean/aportes.csv', na = 'Sin información')


dic_par <- data.frame(id = names(aportes_cand), label = tolower(names(aportes_cand)))
write_csv(dic_par, 'data/clean/dic_part.csv')
