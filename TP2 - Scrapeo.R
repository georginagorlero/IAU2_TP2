
#Trabajo Práctico n° 2 - Scrapeo

#Comenzamos cargando las librerías que vamos a necesitar

#install.packages("rvest")
library(rvest) # Easily Harvest (Scrape) Web Pages
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier

#Vamos a trabajar con la información de la llegada de las vacunas Covid-19 a la Argentina
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
class(header)

#Nos quedamos con la fila anterior por si no queremos perder esa info:
info_header <- as.vector(tabla2[1,], mode="character")


 
tabla3 <- tabla2[3:78,1:5]%>% #Nos quedamos con la parte de la tabla que necesitamos
          `colnames<-`(header)%>% #y le cambiamos el nombre a las columnas por los correctos
          clean_names()%>% #Limpiamos los nombres de las columnas con janitor
          mutate(fecha_de_llegada=dmy(tabla3$fecha_de_llegada))%>% #modificamos la columna de fechas a una con class=date
          mutate(vacuna=as.factor(tabla3$vacuna))%>% #modificamos la columna vacuna y laboratorio por factores
          mutate(laboratorio=as.factor(tabla3$laboratorio)) 
tabla3



