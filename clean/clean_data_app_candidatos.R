library(tidyverse)


contratos <- read_csv("data_clean/contratos_aportantes.csv", col_types = cols(.default = "c"))
cont_vars <- c("cont_firma_ano", "contratista_id", "contratista_nombre", "rep_legal_id", "rep_legal_nombre",
               "ent_nombre", "ent_nit", "cont_valor_tot", "proc_tipo","proc_status","cont_objeto","cont_objeto_det")

contratos2 <- contratos %>% select(one_of(cont_vars))
write_csv(contratos2, "data/clean/contratos.csv")

aportes <- read_csv("data_clean/candidatos_aportantes.csv", col_types = cols(.default = "c"))

as_number <- function(x, decimal = "."){
  regex <- paste0("[^0-9",decimal,"]+")
  as.numeric(gsub(regex,"",x))
}

aportes <- aportes %>%
  mutate(campaign = ifelse(Corporación.o.Cargo %in% c("Cámara de Representantes", "Senado de la República"),
                           "Congreso 2018",
                           ifelse(Corporación.o.Cargo == "Presidencia de la República", "Presidente 2018", "Regionales 2015"))
  ) #%>%
#   mutate(amount = as_number(Valor),
#          cargo = Corporación.o.Cargo,
#          aportante_name = APORTANTE.NORMALIZADO)
aportes$Nombre.Candidato <- gsub("\\s+", " ", aportes$Nombre.Candidato)
aportes$APORTANTE.NORMALIZADO <- gsub("\\s+", " ", aportes$APORTANTE.NORMALIZADO)

aportes_cand <- aportes  %>% 
                  group_by(campaign,  cargo = Corporación.o.Cargo, Organizacion.Politica, Elegido, Nombre.Candidato, Identificacion.Candidato, APORTANTE.NORMALIZADO, Identificación.Normalizada,  Ciudad.Ingreso, Tipo.de.Identificación, group = Tipo.Persona, Tipo.Donacion, Parentesco) %>% 
                   summarise(value = sum(as_number(Valor)))


candidatos <- aportes_cand %>% 
               group_by(id = Identificacion.Candidato, name = Nombre.Candidato,
                        party = Organizacion.Politica, elegido = Elegido, campaign, cargo) %>% 
                summarise(total = n())
candidatos$name_id <- tolower(iconv(candidatos$name, "UTF-8", "ASCII//TRANSLIT"))
write_csv(candidatos, "data/clean/candidatos.csv")


candidatos <- aportes_cand[c("Identificacion.Candidato", "Nombre.Candidato",
                             "Organizacion.Politica", "Elegido", "campaign", "cargo")]

candidatos <- candidatos %>% 
                select(id = Identificacion.Candidato, name = Nombre.Candidato,
                       party = Organizacion.Politica, elegido = Elegido, campaign, cargo) %>% 
  distinct() %>% mutate(group = "Persona Natural", value = 10)


aportantes <- aportes_cand[c("Identificación.Normalizada", "APORTANTE.NORMALIZADO", "group",
                             "campaign", "Ciudad.Ingreso", "Tipo.de.Identificación", "Tipo.Donacion",
                             "Parentesco", "value")]

aportantes <- aportantes %>% 
               select(id = Identificación.Normalizada, name = APORTANTE.NORMALIZADO, group,
                      campaign, Ciudad.Ingreso, Tipo.de.Identificación, Tipo.Donacion, Parentesco, value) 

nodes <- bind_rows(list(candidato = candidatos, aportante = aportantes), .id = "node_type")

write_csv(nodes, "data/clean/nodes.csv")

edges <- aportes_cand[c("Identificacion.Candidato", "Identificación.Normalizada", "campaign")] 
edges <- edges %>%
          select(from = Identificacion.Candidato, to = Identificación.Normalizada, campaign = campaign)
write_csv(edges, "data/clean/edges.csv")



