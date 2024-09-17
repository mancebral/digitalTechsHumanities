# install.packages(c("tidyverse", "datasets", "plot3D", "gsbm", "igraph", "tidygraph",
#                  "ggraph"))

library(tidyverse)
library(datasets)

setwd("~/RPROJECTS/Tecnologias_Digitales")

iris
summary(iris)
View(iris)

#Visualizaciones de 1 variable
iris %>% 
  ggplot(aes(Sepal.Length, fill= Species))+
  geom_histogram()+
  facet_wrap(~Species)

#Visualizaciones de 2 variables
iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, color=Species))+
  geom_point(size=4)+
  facet_wrap(~Species)

#Comparativa de dispersión en un dataset (pairs)
pairs(iris[, 1:4])

#Visualizaciones tridimensionales
library(plot3D)

scatter3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, phi = 0, bty ="g", pch=18,
          col.var = as.integer(iris$Species), 
          col = c("#F8766D", "#00BA38", "#619CFF"),
          colkey = list(at = c(2, 4, 6), side = 4, 
          labels = c("setosa", "versicolor", "virginica")),
          xlab = "Sepal.Length",
          ylab ="Petal.Length", zlab = "Sepal.Width")

#Dimensiones a través de características
iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, color=Petal.Length, size= Petal.Length))+
  geom_point()+
  facet_wrap(~Species)

#Mapas de calor
data <- as.matrix(mtcars)
heatmap(data, Rowv = NA, Colv = NA, scale = "column")

#facet grid
iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, color=Species))+
  geom_point(size=4)+
  facet_grid(vars(Species), vars(Petal.Length))

#grafo
library(gsbm)
library(igraph)
library(tidygraph)
library(ggraph)
data(les_miserables)

miserablesGraph <- graph_from_adjacency_matrix(les_miserables$A) %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  mutate(names=les_miserables$names)

miserablesGraph %>%
  activate(nodes) %>% 
  mutate(centrality_degree=centrality_degree()) %>% 
  ggraph(layout = "stress")+
  geom_edge_link()+
  geom_node_point(aes(size=centrality_degree), color="orange") +
  geom_node_text(aes(label=names), size=2, fontface=2) +
  theme_void()+
  scale_size(range = c(6, 20)) 

#maps
world_countries <- spData::world %>% 
  mutate(Country=toupper(name_long)) %>% 
  mutate(Country=gsub("UNITED STATES", "USA", Country)) %>% 
  select(Country, geom)

dhData <- read.csv("DH_Country_Production.csv") %>% #datos obtenidos de Scopus 
  mutate(Country=region)

world_countries %>%
  full_join(dhData) %>% 
  ggplot(aes(fill=Freq))+
  geom_sf()+
  theme_minimal()


