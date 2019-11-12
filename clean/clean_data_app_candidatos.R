library(tidyverse)
#CERRADO O ADJUDICADO


#contratos <- read_csv("data_clean/contratos_aportantes.csv", col_types = cols(.default = "c"))
#contratos <- contratos %>% filter(proc_status == 'Cerrado')
#"cont_objeto"
contratos <- read_csv('data/secop/clean/contratos_clean.csv', col_types = cols(.default = "c"))
cont_vars <- c("cont_firma_ano", "contratista_id", "contratista_nombre", "rep_legal_id",
               "rep_legal_nombre","ent_nombre", "ent_nit", "cont_valor_tot", "moneda", "proc_tipo",
               "proc_status", "cont_tipo","cont_objeto_det", "ruta_secop1", "secop")

contratos2 <- contratos %>% 
               select(one_of(cont_vars))

write_csv(contratos2, "data/clean/contratos.csv")

source('~/Repos/elecciones/clean/tools.R')
aportes <- read_csv("data_clean/candidatos_aportantes.csv", col_types = cols(.default = "c"))

aportes <- aportes %>%
             mutate(campaign = ifelse(Corporación.o.Cargo %in% c("Cámara de Representantes", "Senado de la República"), "Congreso 2018",
                                ifelse(Corporación.o.Cargo == "Presidencia de la República", "Presidente 2018", "Regionales 2015"))
                  )


aportes_cand <- aportes  %>% 
                 group_by(campaign,  cargo = Corporación.o.Cargo, Organizacion.Politica, Elegido, Nombre.Candidato, Identificacion.Candidato, APORTANTE.NORMALIZADO, Identificación.Normalizada) %>% 
                  summarise(Valor = sum(as_number(Valor)),
                            Ciudad.Ingreso = paste(unique(Ciudad.Ingreso), collapse = ' y '),
                            group = paste(unique(Tipo.Persona)[1], collapse = ' - '),
                            Tipo.de.Identificación = paste(unique(Tipo.de.Identificación)[1], collapse = ' - '))
unique(aportes_cand$group)
aportes_cand$name_cand_line <- add_break(aportes_cand$Nombre.Candidato)
aportes_cand$name_aport_line <- add_break(aportes_cand$APORTANTE.NORMALIZADO)

write_csv(aportes_cand, 'data/clean/aportes.csv', na = 'Sin información')

candidatos <- aportes_cand %>% 
               group_by(id = Identificacion.Candidato, name = Nombre.Candidato,
                        party = Organizacion.Politica, elegido = Elegido, campaign, cargo) %>% 
                summarise(total = n(), 
                          Valor = sum(Valor, na.rm = T))
candidatos$name_id <- tolower(iconv(candidatos$name, "UTF-8", "ASCII//TRANSLIT"))

write_csv(candidatos, "data/clean/candidatos.csv", na = 'Sin información')


candidatos <- aportes_cand[c("Identificacion.Candidato", "Nombre.Candidato",
                             "Organizacion.Politica", "Elegido", "campaign", "cargo")]

candidatos <- candidatos %>% 
                select(id = Identificacion.Candidato, name = Nombre.Candidato,
                       party = Organizacion.Politica, elegido = Elegido, campaign, cargo) %>% 
                  distinct( .keep_all = T) %>% 
                    mutate(group = "Candidato")


aportantes <- aportes_cand[c("Identificación.Normalizada", "APORTANTE.NORMALIZADO", "group",
                             "campaign", "Ciudad.Ingreso", "Tipo.de.Identificación",  "Valor")]

aportantes <- aportantes %>% 
                select(id = Identificación.Normalizada, name = APORTANTE.NORMALIZADO, group,
                       campaign, Ciudad.Ingreso, Tipo.de.Identificación, Valor) 

nodes <- bind_rows(list(candidato = candidatos, aportante = aportantes), .id = "node_type")
nodes$Valor <- ifelse(nodes$node_type == 'candidato', 10, nodes$Valor)
#nodes$group <- ifelse(nodes$node_type == 'candidato', 'Candidato', nodes$group)
nodes$label <- as.character(add_break(nodes$name)) 

write_csv(nodes, "data/clean/nodes.csv")

edges <- aportes_cand[c("Identificacion.Candidato", "Identificación.Normalizada", "campaign")] 
edges <- edges %>%
  select(from = Identificacion.Candidato, 
         to = Identificación.Normalizada,
         campaign = campaign)
edges$color <- '#cccccc'
write_csv(edges, "data/clean/edges.csv")



