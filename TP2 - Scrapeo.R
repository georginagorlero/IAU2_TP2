
#Trabajo Práctico n° 2 - Scrapeo

#Comenzamos cargando las librerías que vamos a necesitar

#install.packages("rvest")
library(rvest) # Easily Harvest (Scrape) Web Pages
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(stringr)

#cambiamos las opciones para no ver los números en notación científica
options(scipen = 99, digits = 2)

#Vamos a trabajar con la información de la llegada de las vacunas Covid-19 a la Argentina
wikipedia_url <- "https://es.wikipedia.org/wiki/Vacunaci%C3%B3n_contra_la_COVID-19_en_Argentina"

vacunascovid_arg <- read_html(wikipedia_url)

#Probamos primero trayendo la tabla a través de html_elements()
tabla <- vacunascovid_arg %>% 
  html_elements(".wikitable") %>%  #Nos trae una lista con las 12 tablas de la URL.
  `[[`(7) %>%  #Fuimos viendo tabla por tabla hasta saber que la que tenía que traer era la 7
  html_table()

tabla

#Probamos con Xpath
tabla2 <- vacunascovid_arg %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[11]') %>%
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


 
vacunas_covid <- tabla2[3:78,1:5]%>% #Nos quedamos con la parte de la tabla que necesitamos
                 `colnames<-`(header)%>% #y le cambiamos el nombre a las columnas por los correctos
                 clean_names() #Limpiamos los nombres de las columnas con janitor

vacunas_covid$fecha_de_llegada <- str_replace_all(vacunas_covid$fecha_de_llegada, "de","")%>% #para pasar las fechas a inglés tal que el lubridate las reconozca vamos a primero sacar los "de"
                                  str_replace_all(c("enero" = "january",    #pasamos cada mes a su equivalente en inglés  
                                                    "febrero" = "february",
                                                    "marzo" ="march",
                                                    "abril"="april", 
                                                    "mayo"="may", 
                                                    "junio"="june", 
                                                    "julio"="july",
                                                    "agosto"="august",
                                                    "septiembre"="september",
                                                    "octubre" = "october",
                                                    "noviembre" ="november",
                                                    "diciembre" = "december"))

#Arreglamos la columna de cantidad de dosis para convertirla en números                        
vacunas_covid$cantidad_de_dosis <- gsub("[[:space:]]", "", tabla3$cantidad_de_dosis)%>%  #sacamos los espacios en blanco al interior de los números
                                   as.integer() #lo pasamos a integer

vacunas_covid <- vacunas_covid %>%
                 mutate(fecha_de_llegada=dmy(vacunas_covid$fecha_de_llegada), #modificamos la columna de fechas a una con class=date
                        vacuna=as.factor(vacunas_covid$vacuna), #modificamos la columna vacuna y laboratorio por factores
                        laboratorio=as.factor(vacunas_covid$laboratorio))
vacunas_covid

#Agregamos las columnas mes y año para poder tener la información agregada por mes y por año

vacunas_covid <- vacunas_covid %>%
                 mutate(mes_llegada=month(vacunas_covid$fecha_de_llegada, label = TRUE, abbr = FALSE),
                        mes_llegada_num=month(vacunas_covid$fecha_de_llegada),
                        ano_llegada=year(vacunas_covid$fecha_de_llegada),
                        ano_mes=paste(ano_llegada,"/",mes_llegada_num))


#Ahora podemos ver las cantidades totales por labotatorio, por tipo de vacuna, por mes y año

vacunas_covid_lab <- vacunas_covid %>%
                     group_by(laboratorio) %>%
                     summarise(cantidad=sum(cantidad_de_dosis))
  
vacunas_covid_mes <- vacunas_covid %>%
                     group_by(ano_mes) %>%
                     summarise(cantidad=sum(cantidad_de_dosis))%>%
                     mutate(acumulado=cumsum(cantidad))


vacunas_covid_mes_lab <- vacunas_covid %>%
  group_by(ano_mes,laboratorio) %>%
  summarise(cantidad=sum(cantidad_de_dosis))

#Podemos ver rápidamente de qué laboratorios llegaron más vacunas
ggplot(vacunas_covid_lab) +
  geom_col(aes(reorder(laboratorio,cantidad),y=cantidad, fill=laboratorio))+
  coord_flip()

#Podemos ver qué cantidades llegaron por día por laboratorio
ggplot(vacunas_covid) +
  geom_line(aes(x=fecha_de_llegada,y=cantidad_de_dosis, color=laboratorio))+
  geom_point(aes(x=fecha_de_llegada,y=cantidad_de_dosis, color=laboratorio))+
  geom_text(aes(x=fecha_de_llegada,y=(cantidad_de_dosis+100000), label=cantidad_de_dosis, color=laboratorio), size=2)

#También podemos revisar qué meses llegaron más vacunas
ggplot(vacunas_covid_mes) +
  geom_col(aes(x=ano_mes, y=cantidad, color=))+
  coord_flip()

#y a qué laboratorios correspondieron
ggplot(vacunas_covid_mes_lab) +
  geom_col(aes(x=ano_mes, y=cantidad, fill=laboratorio))+
  coord_flip()


ggplot(vacunas_covid_mes) +
  geom_line(aes(x=ano_mes, y=acumulado))+
  geom_col(aes(x=ano_mes, y=cantidad))


  
  