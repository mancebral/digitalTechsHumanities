library(tidyverse)
library(rtweet)

#autentificación
auth_setup_default()

#autentificación antigua
tw_token <-create_token(app = "",
                        consumer_key = "",
                        consumer_secret = "",
                        access_token = "", 
                        access_secret = "")

#descargar datos por hashtag
dhTweets <- search_tweets("#digitalhumanities", 
                          n= 100)

dhTweets %>% View()

#descargar datos de un usuario
obamaTweets <- get_timeline(user = "BarackObama", n = 100)
View(obamaTweets)

obamaTweets %>% filter(str_detect(text, "//bvote//b")) %>% View()

#descargar tendencias
mxTrends <- get_trends("mexico")
View(mxTrends)

#miembros de una lista
dhList <- lists_members(list_id = "1263929359125090304")

View(dhList)

#tweets de los miembros de una lista
dhListTweets <- get_timeline(dhList$user_id, n=10)

####graficacion ####
obamaTweets %>% ggplot(aes(created_at, retweet_count)) +
  geom_point()

obamaTweets %>% filter(retweet_count>100000) %>% View()

obamaTweets %>% 
  gather(tipo, n, retweet_count:favorite_count) %>% 
  ggplot(aes(created_at, n, color=tipo)) +
  geom_point(size=6, alpha=0.5)+
  scale_x_datetime()+
  ggtitle("Últimos 100 tweets de Obama")+
  xlab("Fecha y hora")

####wikipedia####
library(WikipediaR)
wkTrump_es <- backLinks("Donald Trump", domain = "es")
wkTrump_en <- backLinks("Donald Trump", domain = "en")

wkTrumpL_es <- links("Donald Trump", domain = "es")
wkTrumpL_en <- links("Donald Trump", domain = "en")

###en este caso no es necesario que hagan una visualización, pero sí que comenten los resultados
hd_c_es <- contribs("Humanidades_digitales", domain = "es")
hd_c_en <- contribs("Digital_humanities", domain = "en")
hd_c_fr <- contribs("Humanités_numériques", domain = "fr")

hd_bl_es <- backLinks("Humanidades_digitales", domain = "es")
hd_bl_en <- backLinks("Digital_humanities", domain = "en")
hd_bl_fr <- backLinks("Humanités_numériques", domain = "fr")

hd_l_es <-links("Humanidades_digitales", domain = "es")
hd_l_en <- links("Digital_humanities", domain = "en")
hd_l_fr <- links("Humanités_numériques", domain = "fr")


####spotifyr####
library(spotifyr)

id <- '----tuID----'
secret <- '----tuSecret----'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

dangerous <- get_album_tracks("0oX4SealMgNXrvRDhqqOKg")

dangerous_tracks <- dangerous %>% select(id, name)

features_dangerous <- get_track_audio_features(ids = dangerous_tracks$id)

features_dangerous <- left_join(dangerous_tracks, features_dangerous)

features_dangerous %>%  
  gather(caracteristicas, valor, danceability:valence) %>% 
  ggplot(aes(name, valor, fill=caracteristicas))+
    geom_col(show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~caracteristicas, scales = "free_x")

features_dangerous %>% 
  ggplot()


####Youtube####
#En este caso se descargó el archivo de comentarios de un video a través de la herramienta
#https://tools.digitalmethods.net/netvizz/youtube/index.php en específico la que vale para comentarios:
#https://tools.digitalmethods.net/netvizz/youtube/mod_video_info.php
#Una vez descargado en formato .csv, el archivo se importa a la sesión de R. 
#Hay varias formas para importarlo, pero pueden hacerlo desde el topmenu
#Van a File> impor dataset > Import from text 
comentariosYT %>% 
  ggplot(aes(x=publishedAt, y = likeCount))+
  geom_point()

