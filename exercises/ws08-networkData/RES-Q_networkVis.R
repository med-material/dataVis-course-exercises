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


ggraph(net, layout = 'linear') + 
  geom_edge_arc(color = "orange", width=0.7) +
  geom_node_point(aes(size=Count), color="gray50") +
  theme_void()+ geom_node_text(aes(label = id), vjust = 1.5)


# adjacency matrix --------------------------------------------------------
# Step 1: Example dataframe from 'from' and 'to' columns
edges_df <- edges %>% select(from, to)

# Step 2: Convert edges dataframe to adjacency matrix
library(igraph)
adj_matrix <- as_adjacency_matrix(graph_from_data_frame(edges_df, directed = TRUE))

# Step 3: Transform the adjacency matrix to a dataframe
adj_df <- as.data.frame(as.matrix(adj_matrix))

# Step 4: Reshape the dataframe for plotting
library(reshape2)
adj_df_melted <- melt(adj_df)
adj_df_melted$value <- as.character(adj_df_melted$value)
# Step 5: Plotting
library(ggplot2)
ggplot() +
  geom_rect(data = expand.grid(x = 1:nrow(adj_matrix), y = 1:ncol(adj_matrix)),
            aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5),
            fill = "white", color = "black") +
  geom_rect(data = expand.grid(x = 1:nrow(adj_matrix), y = 1:ncol(adj_matrix))[as.logical(adj_matrix), ],
            aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5),
            fill = "black") +
  scale_x_continuous(expand = c(0, 0), breaks = 1:nrow(adj_matrix), labels = colnames(adj_matrix)) +
  scale_y_continuous(expand = c(0, 0), breaks = 1:ncol(adj_matrix), labels = rownames(adj_matrix)) +
  theme_minimal() +
  labs(x = "Node", y = "Node") +
  ggtitle("Adjacency Matrix Visualization")

