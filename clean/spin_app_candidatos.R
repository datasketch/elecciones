library(tidyverse)
library(visNetwork)


candidatos <- read_csv("data/clean/candidatos.csv")
candidatos <- candidatos %>% slice(1:1000)
candidates <- candidatos %>% pull(id) %>% set_names(candidatos$name)

nodes <- read_csv("data/clean/nodes.csv")
edges <- read_csv("data/clean/edges.csv")


selected_candidate <- function(){
  cand <- sample(candidates,1)
  message(cand)
  cand
}
available_campaign <- function(){
  candidatos %>% filter(id == selected_candidate()) %>% pull(campaign)
}
selected_campaign <- function(){
  sample(available_campaign(),1)
}

eds <- edges %>% 
  filter(campaign == selected_campaign()) %>% 
  filter(from %in% selected_candidate() | to %in% selected_candidate())
nds <- nodes %>% filter(id %in% c(eds$from, eds$to))

nds <- nds %>% mutate(label = name, group = type, value = amount) %>% distinct(id, .keep_all = TRUE)

visNetwork(nds, eds) %>%
  #visLegend() %>%
  #visGroups(groupname = "Persona Natural", color = "red") %>%
  #visGroups(groupname = "Persona Jurídica", color = "yellow") %>% 
  visIgraphLayout(layout="layout_nicely") %>% 
  #visNodes(scaling = list(label = list(enabled = T))) # texto proporcional al tamaño del circulo

