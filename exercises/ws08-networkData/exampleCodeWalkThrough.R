library(tidyverse)
letters <- read_csv("data/correspondence-data-1585.csv")

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")

nodes <- rowid_to_column(nodes, "id")

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(weight = n(), .groups = "drop")
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

library(network)

routes_network <- network(edges,
                          vertex.attr = nodes,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)

plot(routes_network, vertex.cex = 3)


