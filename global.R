library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)

conso_commune <- read.csv('data/consommation-electrique-par-secteur-dactivite-commune.csv', sep=";", encoding = "UTF-8")
conso_departement <- read.csv('data/consommation-electrique-par-secteur-dactivite-departement.csv', sep=";", encoding = "UTF-8")
conso_region <- read.csv('data/consommation-electrique-par-secteur-dactivite-region.csv', sep=";", encoding = "UTF-8")
prod_commune <- read.csv('data/production-electrique-par-filiere-a-la-maille-commune.csv', sep=";", encoding = "UTF-8")
prod_departement <- read.csv('data/production-electrique-par-filiere-a-la-maille-departement.csv', sep=";", encoding = "UTF-8")
prod_region <- read.csv('data/production-electrique-par-filiere-a-la-maille-region.csv', sep=";", encoding = "UTF-8")

vect_commune = unique(conso_commune$Nom.Commune)
vect_departements = unique(conso_departement$Nom.Département)
vect_region = unique(conso_region$Nom.Région) #régler pb départements région en majuscule desfois




vect_annee = unique(prod_region$Année)

shinyApp(ui = ui, server = server)
