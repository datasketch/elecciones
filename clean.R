library(tidyverse)
library(openxlsx)

# cambio de nombres secop 2

secop_ii <- read_csv('data/secop/original/SECOP_II_Contratos.csv', col_types = cols(.default = "c"))

secop_ii <- secop_ii %>%
                   drop_na(`Nit Entidad`)

dic_secop_ii <- read_csv('data/secop/original/names_secop.csv')
names(secop_ii) <- dic_secop_ii$names_secop
secop_ii$secop <- 'Dos'
secop_ii <- secop_ii %>% 
                  separate(cont_firma_ano, c('mes', 'dia', 'cont_firma_ano'), sep = '/') %>% 
                   select(-mes, -dia) %>% 
                    drop_na(cont_firma_ano)

secop_ii <- secop_ii %>% 
              filter(cont_firma_ano %in% c("2016", "2017", "2018", "2019")) 



secop_i <- read_csv('data/secop/original/secop_aportantes.csv', col_types = cols(.default = "c"))
secop_i$secop <- 'Uno'
secop_i <- secop_i %>% drop_na(cont_firma_ano)

secop_i <- secop_i %>% 
            filter(cont_firma_ano %in% c("2016", "2017", "2018", "2019")) 

unique(secop_i$cont_firma_ano)

# union secop
secop_all <- bind_rows(secop_i, secop_ii)
unique(secop_all$cont_firma_ano)



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
candidatos$Identificación.Normalizada <- as.character(candidatos$Identificación.Normalizada)

cand_ident <- candidatos %>% 
               distinct(Identificación.Normalizada)

cand_ident$financiador <- ' Si'

id_contrata <- secop_temp %>%
                  left_join(cand_ident,
                   by = c('contratista_id' = 'Identificación.Normalizada'))

id_contrata <- id_contrata %>% 
                filter(!is.na(financiador))
id_contrata$Identificación.Normalizada <- id_contrata$contratista_id

secop_fil <- secop_all %>% left_join(id_contrata)
id_contrata$fin_temp <- id_contrata$financiador
id_contrata <- id_contrata %>% select(rep_legal_id = contratista_id, fin_temp, Identificación.Normalizada)
secop_fil <- secop_fil %>% left_join(id_contrata)
secop_fil$financiador <- coalesce(secop_fil$fin_temp, secop_fil$financiador)
secop_fil <- secop_fil %>% select(-fin_temp) %>% filter(!is.na(financiador))
length(unique(secop_fil$Identificación.Normalizada))
write_csv(secop_all,  'data_clean/contratos_aportantes.csv')


id_secop <- secop_fil %>% distinct(Identificación.Normalizada)
id_secop$contratos <- 'Si'
candidatos_fil <- candidatos %>% left_join(id_secop)
candidatos_fil <- candidatos_fil %>% filter(!is.na(contratos))
write_csv(secop_all,  'data_clean/candidatos_aportantes.csv')

rm(list = ls())


# lectura secop completa



