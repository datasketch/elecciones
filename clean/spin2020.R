library(tidyverse)
library(openxlsx)

# cambio de nombres secop 2

secop_ii <- read_csv('data/secop/original/SECOP_II_Contratos.csv', col_types = cols(.default = "c"))
secop_ii <- secop_ii %>% filter(`Estado Contrato` != "Cancelado")
secop_ii <- secop_ii %>%
  drop_na(`Nit Entidad`)

secop_ii <- secop_ii %>% filter(`Valor del Contrato` > 100)
dic_secop_ii <- read_csv('data/secop/original/names_secop.csv')
names(secop_ii) <- dic_secop_ii$names_secop
secop_ii$secop <- 'Dos'
secop_ii <- secop_ii %>% 
  separate(cont_firma_ano, c('mes', 'dia', 'cont_firma_ano'), sep = '/') %>% 
  select(-mes, -dia) %>% 
  drop_na(cont_firma_ano)

secop_ii <- secop_ii %>% 
  filter(cont_firma_ano %in% c("2015", "2016", "2017", "2018", "2019")) 



secop_i <- read_csv('data/secop/original/secop_aportantes.csv', col_types = cols(.default = "c"))
secop_i$secop <- 'Uno'

secop_i <- secop_i %>% drop_na(cont_firma_ano)

secop_i <- secop_i %>% 
  filter(cont_firma_ano %in% c("2015", "2016", "2017", "2018", "2019")) 

unique(secop_i$cont_firma_ano)

# union secop
secop_all <- bind_rows(secop_i, secop_ii)
secop_all$moneda[is.na(secop_all$moneda)] <- "No Definida" 
#secop_all <- secop_all %>% filter(cont_valor_tot > 50)
# secop_all$ind_cuantia <-  ifelse(secop_all$cont_valor_tot <= 100 & secop_all$moneda != "Dólares (USD)", 'remover', 'dejar' )
# secop_all <- secop_all %>% 
#               filter(ind_cuantia == 'dejar') %>% 
#                select(-ind_cuantia)
min(secop_all$cont_cuantia)
min(secop_all$cont_valor_tot)
# solo contratos de aportantes a candidatos

sec_cont <- secop_all %>%
  distinct(contratista_id) %>%
  drop_na(contratista_id)

sec_rep <- secop_all %>% 
  distinct(contratista_id = rep_legal_id) %>%
  drop_na(contratista_id)


secop_temp <- bind_rows(sec_cont, sec_rep) %>%
  distinct(contratista_id, .keep_all = T)


candidatos <- read.xlsx('data/candidatos/BASE-GENERAL-CUENTAS-CLARAS.xlsx')
candidatos$Elegido[candidatos$Identificacion.Candidato == 79940745] <- 'Sí'
candidatos$Tipo.Persona <- ifelse(is.na(candidatos$Tipo.Persona), 'Persona Jurídica', candidatos$Tipo.Persona)
candidatos$Identificación.Normalizada <- as.character(candidatos$Identificación.Normalizada)
candidatos$Nombre.Candidato <- gsub("\\s+", " ", candidatos$Nombre.Candidato)
candidatos$APORTANTE.NORMALIZADO <- gsub("\\s+", " ", candidatos$APORTANTE.NORMALIZADO)
candidatos <- candidatos %>% 
  filter(!APORTANTE.NORMALIZADO %in% c('IDENTIFICACIÓN INVÁLIDA', 'ANULADO',  'APORTANTE INEXISTENTE'),
         Identificación.Normalizada > 1) 
cand_ident <- candidatos %>% 
  distinct(Identificación.Normalizada)

cand_ident$financiador <- 'Si'

id_contrata <- secop_temp %>%
  left_join(cand_ident,
            by = c('contratista_id' = 'Identificación.Normalizada'))