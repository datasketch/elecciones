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

id_contrata <- id_contrata %>% 
                filter(!is.na(financiador))

secop_fil <- secop_all %>% 
              filter(rep_legal_id %in% unique(id_contrata$contratista_id) | contratista_id %in% unique(id_contrata$contratista_id))
length(unique(id_contrata$contratista_id))


write_csv(secop_fil,  'data_clean/contratos_aportantes.csv', na = '')

candidatos_fil <- candidatos %>% filter(Identificación.Normalizada %in% unique(secop_temp$contratista_id))
length(unique(candidatos_fil$Identificación.Normalizada))
# blabla <- candidatos %>% filter(Identificación.Normalizada %in% unique(secop_all$contratista_id) | Identificación.Normalizada %in% unique(secop_all$rep_legal_id))
# length(unique(blabla$Identificación.Normalizada))
write_csv(candidatos_fil,  'data_clean/candidatos_aportantes.csv', na = '')

#rm(list = ls())


# Funcionamiento


func <- read.xlsx('data/candidatos/BASE-GENERAL-CUENTAS-CLARAS.xlsx', 2)
func <- func %>% drop_na(IDENTIFICACION_NORM)
func_sec <- func %>% filter(IDENTIFICACION_NORM %in% unique(secop_temp$contratista_id))
write_csv(func_sec, 'data_clean/funcionamiento_filter.csv')

secop_func <- secop_all %>% filter(rep_legal_id %in% unique(func$IDENTIFICACION_NORM) | contratista_id %in% unique(func$IDENTIFICACION_NORM))
length(unique(secop_func$contratista_id))

write_csv(secop_func, 'data_clean/secop_filter_funcionamiento.csv', na = '')

length(unique(func_sec$IDENTIFICACION_NORM))



rm(list = ls())

secop <- read_csv('data/secop/clean/CONTRATOS-ACTUALIZADO.csv', col_types = cols(.default = "c"))
secop <- secop[-nrow(secop),-92]
secop <- secop %>% filter(`Estado del Proceso` != "Cancelado")
secop$`DUPLICIDAD CONTRATO`[is.na(secop$`DUPLICIDAD CONTRATO`)] <- 'Sin información'
unique(secop$`DUPLICIDAD CONTRATO`)
secop <- secop %>% filter(`DUPLICIDAD CONTRATO` != 'REPETIDO-1')
secop <- secop %>% select(-`Tipo de Contrato`)
secop <- secop %>% plyr::rename(c('TIPO-CONTRATO-REF' = 'Tipo de Contrato'))

dic_secop <- read_csv('data/secop/clean/dic.csv')
dic_secop <- dic_secop %>% filter(secop == 'uno') %>% distinct(id, .keep_all = T)
varInf <- data.frame(label = names(secop))
dic_aag <- varInf %>% left_join(dic_secop)
dic_aag$id <- coalesce(dic_aag$id, dic_aag$label)
names(secop) <- dic_aag$id
secop$moneda[is.na(secop$moneda)] <- "No Definida"

write_csv(secop, 'data/secop/clean/contratos_clean.csv')
