library(tidyverse)

#contratos <- read_csv("data_clean/contratos_aportantes.csv", col_types = cols(.default = "c"))
contratos <- read_csv('data/secop/clean/contratos_clean.csv', col_types = cols(.default = "c"))
cont_vars <- c("cont_firma_ano", "contratista_id", "contratista_nombre", "rep_legal_id",
               "rep_legal_nombre","ent_nombre", "ent_nit", "ent_nivel", "cont_valor_tot", "moneda", "proc_tipo",
               "proc_status", "cont_tipo", "cont_objeto_det", "dept_ejec", "grupo", "ruta_secop1", "secop")

contratos2 <- contratos %>% 
  select(one_of(cont_vars))

write_csv(contratos2,  'data/clean/contratos_cruces.csv', na = 'Sin información')

source('~/Repos/elecciones/clean/tools.R')
aportes <- read_csv("data_clean/candidatos_aportantes.csv", col_types = cols(.default = "c"))

aportes <- aportes %>%
  mutate(campaign = ifelse(Corporación.o.Cargo %in% c("Cámara de Representantes", "Senado de la República"), "Congreso 2018",
                           ifelse(Corporación.o.Cargo == "Presidencia de la República", "Presidente 2018", "Regionales 2015"))
  )


aportes_cand <- aportes  %>% 
  group_by(campaign,  cargo = Corporación.o.Cargo, Organizacion.Politica, Elegido, Nombre.Candidato, Identificacion.Candidato, APORTANTE.NORMALIZADO, Identificación.Normalizada, Parentesco, Elegido, Tipo.Donacion, Genero) %>% 
  summarise(Valor = sum(as_number(Valor)),
            Ciudad.Ingreso = paste(unique(Ciudad.Ingreso), collapse = ' y '),
            Tipo.Persona = paste(unique(Tipo.Persona)[1], collapse = ' - '),
            Tipo.de.Identificación = paste(unique(Tipo.de.Identificación)[1], collapse = ' - '))
unique(aportes_cand$Tipo.Persona)


write_csv(aportes_cand, 'data/clean/aportes.csv', na = 'Sin información')
