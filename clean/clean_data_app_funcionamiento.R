library(tidyverse)
source('~/Repos/elecciones/clean/tools.R')

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
info_aport <- info_aport %>% group_by(NOMBRE_ORGANIZACION) %>% mutate(id_part = sample(100:300, 1))

aportantes <- info_aport %>% 
               group_by(NOMBRE_ORGANIZACION, id_part, ANO_INFORME, APORTANTE, IDENTIFICACION_NORM, TIPO_IDENTIFICACION) %>% 
                 summarise(valor = sum(VALOR, na.rm = T),
                           total = n(),
                           ciudad.ingreso = paste0(unique(CIUDAD_INGRESO), collapse = ' - '),
                           group = paste0(unique(TIPO_PERSONA)[1], collapse = ))
aportantes$name_cand_line <- add_break(aportantes$NOMBRE_ORGANIZACION)
aportantes$name_aport_line <- add_break(aportantes$APORTANTE)

write_csv(aportantes, 'data/clean/aportantes.csv', na = 'Sin información')


partidos <- info_aport %>% 
             group_by(NOMBRE_ORGANIZACION, id_part, ANO_INFORME) %>% 
              summarise(total = n(),
                        valor = sum(VALOR, na.rm = T))

write_csv(partidos, 'data/clean/partidos.csv', na = 'Sin información')



partidos <- info_aport %>% 
             group_by(NOMBRE_ORGANIZACION, ANO_INFORME) %>% 
              summarise(total = n(),
                valor = sum(VALOR, na.rm = T))


partidos <- info_aport %>% 
             select(id = id_part, name = NOMBRE_ORGANIZACION, informe = ANO_INFORME) %>% 
                distinct( .keep_all = T) %>% 
                    mutate(group = "Partido")


aportantes <-  aportantes[c("IDENTIFICACION_NORM", "APORTANTE", "group",
                             "ANO_INFORME", "ciudad.ingreso",  "valor")]

aportantes <- aportantes %>% 
               select(id = IDENTIFICACION_NORM, name = APORTANTE, group,
                      informe = ANO_INFORME, ciudad.ingreso, valor) 

nodes <- bind_rows(list(partido = partidos, aportante = aportantes), .id = "node_type")
nodes$valor <- ifelse(nodes$node_type == 'Partido', 10, nodes$valor)
#nodes$group <- ifelse(nodes$node_type == 'candidato', 'Candidato', nodes$group)
nodes$label <- as.character(add_break(nodes$name)) 

write_csv(nodes, "data/clean/nodes.csv")


edges <- info_aport %>% group_by(id_part, IDENTIFICACION_NORM, ANO_INFORME) %>% summarise(total = n())

edges <- edges %>%
  select(from = id_part, 
         to = IDENTIFICACION_NORM,
         informe = ANO_INFORME)
edges$color <- '#cccccc'
write_csv(edges, "data/clean/edges.csv")

