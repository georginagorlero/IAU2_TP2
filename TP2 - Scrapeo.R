
#Trabajo Práctico n° 2 - Scrapeo

#Comenzamos cargando las librerías que vamos a necesitar

#install.packages("rvest")
library(rvest) # Easily Harvest (Scrape) Web Pages
library(tidyverse) # Easily Install and Load the 'Tidyverse' 

wikipedia_url <- "https://es.wikipedia.org/wiki/Vacunaci%C3%B3n_contra_la_COVID-19_en_Argentina"

vacunascovid_arg <- read_html(wikipedia_url)

#Probamos primero trayendo la tabla a través de html_elements()
tabla <- vacunascovid_arg %>% 
  html_elements(".wikitable") %>%  #Nos trae una lista con las 12 tablas de la URL.
  `[[`(6) %>%  #Fuimos viendo tabla por tabla hasta saber que la que tenía que traer era la 6
  html_table()

tabla

#Probamos con Xpath
tabla2 <- vacunascovid_arg %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[10]') %>%
  html_table(fill=T)

tabla2

#En ambos casos logramos el mismo resultado.

#Algo que sabíamos que iba a pasar por cómo es la tabla en la página de wikipedia, es que el encabezado 
#que necesitamos, está en la fila 3, para esto vamos a tener que hacer algunos arreglos. 

#Convertimos nuestra tabla en un tibble
tabla2 <- tabla2[[1]] 

#Nos quedamos con la fila que será nuestro header
header <- as.vector(tabla2[2,],mode="character")
class(header2)

#Nos quedamos con la fila anterior por si no queremos perder esa info:
info_header <- as.vector(tabla2[1,], mode="character")

#Nos quedamos con la parte de la tabla que necesitamos y le cambiamos el nombre a las columnas por los correctos 
tabla3 <- tabla2[3:78,1:5]%>%
          `colnames<-`(header)
tabla3



