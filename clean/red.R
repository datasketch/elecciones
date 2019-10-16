library(visNetwork)


nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
                    group = sample(c("A", "B"), 10, replace = TRUE), 
                    value = c(20, 20, 1, 40, 5,  70, 10, 8, 20, 70))
edges <- data.frame(from = c(2,2,2), to = c(1,3,10))


visNetwork(nodes, edges) %>%
  visLegend() %>%
   visGroups(groupname = "A", color = "red") %>%
   visGroups(groupname = "B", color = "yellow") %>% 
  visIgraphLayout(layout="layout_in_circle") %>% 
  visNodes(scaling = list(label = list(enabled = T))) # texto proporcional al tamaño del circulo





# 

# 

# 
# 
# # candidatos con representantes que tienen contratos con el estado
# # congreso
# # Corporación o cargo
# 
# corporacion_sample <- data_contrata %>% 
#                   filter(Corporación.o.Cargo %in% c("Cámara de Representantes", "Senado de la República"))
# 
# corporacion_sample <- corporacion_sample %>% 
#                         filter(id_cont == 'Si')
# 
# congresista_con <- corporacion_sample %>% 
#                     distinct(Identificacion.Candidato, .keep_all = T)
# 
# corporacion <- congresista_con %>% 
#                 group_by(Corporación.o.Cargo) %>% 
#                   summarise(total = n())
# 
# # 942 candidatos a camara de representantes tienen aportantes que tienen o han tenido contratos con el estado
# # 432 candidatos a senado tienen aportantes que tienen o han tenido contratos con el estado
# 
# hgch_bar_CatNum(corporacion)
# 
# 
# 
# # Departamento
# 
# depto <- congresista_con %>% 
#   group_by(Departamento) %>% 
#   summarise(total = n())
# hgch_bar_CatNum(depto)
# 
# # Organizacion Politica
# org_plt <- congresista_con %>% 
#              group_by(Organizacion.Politica) %>% 
#               summarise(total = n())
# hgch_bar_CatNum(depto, opts = list(drop_na = T))
# 
# # Elegido
# elegido <- congresista_con %>% 
#              group_by(Elegido) %>% 
#                summarise(total = n())
# hgch_bar_CatNum(elegido)
# 
# 
# # Genero
# genero <- congresista_con %>% 
#            group_by(Genero) %>% 
#             summarise(total = n())
# hgch_bar_CatNum(genero)
# # Tipo Persona
# tipo_persona <- congresista_con %>% 
#                  group_by(Tipo.Persona) %>% 
#                   summarise(total = n())
# hgch_bar_CatNum(tipo_persona)
# 
# # Valor
# # Departamento Ingreso
# # Ciudad Ingreso
# # Parentesco
# # Tipo Donacion
