library(ggraph)
library(igraph)
library(tidyverse)
script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)
#load the data
nodes<-read.csv("data/RESQ-nodes.csv")
edges<-read.csv("data/RESQ-edges.csv")

# create the net object with edges and nodes
net<-graph_from_data_frame(d=edges, vertices=nodes, directed=T)

#example plot
plot(net, edge.arrow.mode=2, layout=do.call("layout_as_star", list(net)) , main="layout_in_circle", edge.arrow.size = 0.2)

#other options
#  "layout_as_star"
#  "layout_components"
#  "layout_in_circle"
#  "layout_nicely"
#  "layout_on_grid"
#  "layout_on_sphere"
#  "layout_randomly"
#  "layout_with_dh"
#  "layout_with_drl"
#  "layout_with_fr"
#  "layout_with_gem"
#  "layout_with_graphopt"
#  "layout_with_kk"
#  "layout_with_lgl"
#  "layout_with_mds"