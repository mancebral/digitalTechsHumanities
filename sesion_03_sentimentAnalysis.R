###SESION SINCRÓNICA 3###
#Maestría en Humanidades digitales

#emotional messages obetindos de la página: 
#https://matchlessdaily.com/powerful-emotional-text-messages/ 
#descarga este documento en .txt
#crea un directorio para trabajar con este ejercicio
#cambia el directorio de trabajo mediante setwd()
#o mediante la pestaña Files > More > Set as working directory
setwd("~/R/PROJECTS/Tecnologias_Digitales")

#cargamos el archivo
emotionalMessages <- read.delim("emotionalMessages.txt", 
                                header = FALSE)

#aplicamos unnest_tokens sobre el vector V1
emotionalWords <- emotionalMessages %>% 
  #unnest_tokens(message, V1, token = "paragraphs") %>% 
  #separate(message, c("messageNumber", "message"), sep = "\\. ") %>% 
  #mutate(messageNumber=as.numeric(messageNumber)) %>% 
  #group_by(messageNumber) %>% 
  unnest_tokens(word, V1) %>% 
  count(word, sort = TRUE) 
#%>% 
  #ungroup()

#curioso, hay más miedo que sorpresa!
emotionalPlot <- emotionalWords %>% 
  inner_join(diccNRC) %>% 
  group_by(sentiment) %>% 
  summarise(total=sum(n)) %>% 
  ungroup() %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  mutate(sentiment=reorder(as_factor(sentiment), total)) %>% 
  ggplot(aes(sentiment, total, fill= total))+
  geom_col()+
  coord_flip()    

#guardamos en alta resolución
ggsave("emotionalPlot.jpg", emotionalPlot,
       height = 6, width = 10, dpi = 300)

#vamos a ñadir una columna para adjudicar un color a cada emoción:
emotionalNRC <- emotionalWords %>% 
  inner_join(diccNRC) %>% 
  group_by(sentiment, word) %>% 
  summarise(total=sum(n)) %>% 
  ungroup() %>% 
  filter(!sentiment%in%c("positive", "negative")) %>% 
  select(word, total, sentiment) %>% 
  mutate(color=ifelse(sentiment=="trust", "#1d3557", NA),
         color=ifelse(sentiment=="joy", "#457b9d", color),
         color=ifelse(sentiment=="surprise", "#a8dadc",color),
         color=ifelse(sentiment=="anticipation", "#a83252",color),
         color=ifelse(sentiment=="sadness", "#e63946",color),
         color=ifelse(sentiment=="fear", "#ffb703",color),
         color=ifelse(sentiment=="disgust", "#2a9d8f",color),
         color=ifelse(sentiment=="anger", "#ff006e", color))

#cargamos la librería wordcloud2
#sino la tienen iinstalada, recuerden que tienen que instalarla previamente
library(wordcloud2)

emotionalNRC %>%   select(word, total, sentiment) %>% 
  wordcloud2(size=2.5, color= emotionalNRC$color) 
