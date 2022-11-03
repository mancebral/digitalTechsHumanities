#Previamente, instalar estos packages
library(tidyverse)
library(sf)
library(sp)
library(spData)
library(tmap)

#observamos la estructura de un aarchivo con datos geolocalizados
#es un archivo guardado como ejemplo en spData
View(world)

#Trabajamos con ese archivo, lo podemos graficar con ggplot
world %>% 
  ggplot(aes(fill=subregion))+
  geom_sf()

#vamos a cargar nuestros datos descargados de google trends
setwd("~/R/PROJECTS/Tecnologias_Digitales")
geoMap <- read_csv("geoMap.csv")
geoMap2 <- read_csv("geoMap2.csv")


#renombrar la variable con la cuenta de búsquedas (es muy larga)
geoMap <- geoMap %>% 
  rename(Searches=`digital humanities + humanidades digitales + humanidades digitais: (10/31/17 - 10/31/22)`)

#juntar con mapa de datos
dhWorld <- world %>% 
  mutate(name_long=gsub("Russian Federation", "Russia", name_long)) %>% 
  rename(Country=name_long) %>% 
  full_join(geoMap) 

#otra forma de juntar bases de datos
dhWorld2 <- world %>% 
  mutate(name_long=gsub("Russian Federation", "Russia", name_long)) %>% 
  full_join(geoMap2, by = c("name_long"="Country")) 

#cargar datos geolocalizados desde archivo shapefile
geoMex <- st_read("MEX_adm/MEX_adm1.shp")

#graficamos con ggplot
#color las visualizaciones
geoMex %>% 
  ggplot(aes())+
  geom_sf()+
  geom_sf_text (aes(label=NAME_1, 
                    color="#ffffff"), size=2)

#activamos visión interactiva del packege tmap
tmap_mode("view")

#vamos a rellenar Searches con 0s
dhWorld <- dhWorld %>% 
  mutate(Searches=replace_na(Searches, 0)) %>% 
  filter(!Country=="Hong Kong")

tm_shape(dhWorld)+
  tm_fill(col = "Searches", #le asignamos el color del "fill" al "total"
          popup.vars = c("Country", "Searches"))+ #decimos qué queremos que aparezca en el pop-up
  tm_bubbles(size="Searches", col = "black",
             scale = 0.5)

#puedes publicar el mapa directamente en rpubs (no permite embed)
#también lo puedes subir a github y publicar la página (sí permite embed)
#https://mancebral.github.io/dh-world-map/geoDHmap.html

#nueva columna
dhWorld2 <- dhWorld %>% 
  mutate(s.p.h=round(Searches/pop*1000000,2))

#otra opción de visualización
tm_shape(dhWorld2)+
  tm_bubbles(size="s.p.h", col = "white",
             scale = 0.8, legend.max.symbol.size = 1)+
  tm_text(text = "s.p.h", clustering = TRUE)

#plotly
library(plotly)

dhWorld_Plotly <- dhWorld %>% 
  mutate(text=paste0(Country, "\n", Searches)) %>% 
  ggplot(aes(fill=Searches, text=text
             ))+
  geom_sf(color="white")+
  theme_void()

ggplotly(dhWorld_Plotly, tooltip = c("text"))
