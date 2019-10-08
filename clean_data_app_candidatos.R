library(tidyverse)


contratos <- read_csv("data_clean/candidatos_aportantes.csv")
cont_vars <- c("cont_firma_ano","ent_nombre","proc_tipo","proc_status","cont_objeto","cont_objeto_det",
               "cont_valor_tot","contratista_id","contratista_nombre","rep_legal_nombre","rep_legal_id",
               "ruta_secop1")
contratos2 <- contratos %>% select(one_of(cont_vars))
write_csv(contratos2, "data/clean/contratos.csv")

aportes <- read_csv("data/consolidado.csv")
aportes <- aportes %>% filter(`Identificación Normalizada` %in% unique(contratos2$contratista_id) |
                              `Identificación Normalizada` %in% unique(contratos2$rep_legal_id))


unique(aportes$`Corporación o Cargo`)

as_number <- function(x, decimal = "."){
  regex <- paste0("[^0-9",decimal,"]+")
  as.numeric(gsub(regex,"",x))
}

aportes <- aportes %>%
  mutate(campaign = ifelse(`Corporación o Cargo` %in% c("Cámara de Representantes", "Senado de la República"),
                           "Congreso 2018", 
                           ifelse(`Corporación o Cargo` == "Presidencia de la República", "Presidente 2018", "Regionales 2015"))
  ) %>% 
  mutate(amount = as_number(Valor),
         cargo = `Corporación o Cargo`,
         aportante_name = `APORTANTE NORMALIZADO`)

candidatos <- aportes %>% 
  select(id=`Identificacion Candidato`, name=`Nombre Candidato`,
         party = `Organizacion Politica`, elegido = Elegido, campaign, cargo) %>% 
  distinct() %>% mutate(type = "Persona Natural", amount = 10)

write_csv(candidatos, "data/clean/candidatos.csv")

aportantes <- aportes %>% 
  select(id=`Identificación Normalizada`, name = `APORTANTE NORMALIZADO`, 
         type = `Tipo Persona`, amount, campaign) %>% 
  mutate(amount = amount/1000000)

nodes <- bind_rows(list(candidato = candidatos, aportante = aportantes), .id = "node_type")

write_csv(nodes, "data/clean/nodes.csv")

edges <- aportes %>% select(from = `Identificación Normalizada`, to = `Identificacion Candidato`, campaign = campaign)
write_csv(edges, "data/clean/edges.csv")



