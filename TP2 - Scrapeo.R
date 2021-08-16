
#Trabajo Práctico n° 2 - Scrapeo

#Comenzamos trayendo las librerías que vamos a necesitar

#install.packages("rvest")
library(rvest)
library(tidyverse)

wikipedia_url <- "https://es.wikipedia.org/wiki/Vacunaci%C3%B3n_contra_la_COVID-19_en_Argentina"

vacunascovid_arg <- read_html(wikipedia_url)
