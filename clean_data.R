library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)

#On clean les données

conso_commune <- read.csv('data/consommation-electrique-par-secteur-dactivite-commune.csv', sep=";", encoding = "UTF-8")
conso_departement <- read.csv('data/consommation-electrique-par-secteur-dactivite-departement.csv', sep=";", encoding = "UTF-8")
conso_region <- read.csv('data/consommation-electrique-par-secteur-dactivite-region.csv', sep=";", encoding = "UTF-8")
prod_commune <- read.csv('data/production-electrique-par-filiere-a-la-maille-commune.csv', sep=";", encoding = "UTF-8")
prod_departement <- read.csv('data/production-electrique-par-filiere-a-la-maille-departement.csv', sep=";", encoding = "UTF-8")
prod_region <- read.csv('data/production-electrique-par-filiere-a-la-maille-region.csv', sep=";", encoding = "UTF-8")

conso_region <- conso_region %>% select("Année", "Code.Région", "Nom.Région", "CODE.GRAND.SECTEUR", "Conso.totale..MWh.", "Conso.moyenne..MWh.")
conso_departement <- conso_departement %>% select("Année", "Code.Département", "Nom.Département", "Code.Région", "Nom.Région", "CODE.GRAND.SECTEUR", "Conso.totale..MWh.", "Conso.moyenne..MWh.")
conso_commune <- conso_commune %>% select("Année", "Code.Commune", "Nom.Commune", "Code.Département", "Nom.Département", "Code.Région", "Nom.Région", "CODE.GRAND.SECTEUR", "Conso.totale..MWh.", "Conso.moyenne..MWh.")

prod_commune <- prod_commune %>% select("Année", "Nom.commune", "Code.commune", "Nom.département", "Code.département", "Nom.région", "Code.région", "Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.", "Energie.produite.annuelle.Eolien.Enedis..MWh.", "Energie.produite.annuelle.Hydraulique.Enedis..MWh.", "Energie.produite.annuelle.Bio.Energie.Enedis..MWh.", "Energie.produite.annuelle.Cogénération.Enedis..MWh.", "Energie.produite.annuelle.Autres.filières.Enedis..MWh." )
prod_departement <- prod_departement %>% select("Année", "Nom.département", "Code.département", "Nom.région", "Code.région", "Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.", "Energie.produite.annuelle.Eolien.Enedis..MWh.", "Energie.produite.annuelle.Hydraulique.Enedis..MWh.", "Energie.produite.annuelle.Bio.Energie.Enedis..MWh.", "Energie.produite.annuelle.Cogénération.Enedis..MWh.", "Energie.produite.annuelle.Autres.filières.Enedis..MWh." )
prod_region <- prod_region %>% select("Année", "Nom.région", "Code.région", "Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.", "Energie.produite.annuelle.Eolien.Enedis..MWh.", "Energie.produite.annuelle.Hydraulique.Enedis..MWh.", "Energie.produite.annuelle.Bio.Energie.Enedis..MWh.", "Energie.produite.annuelle.Cogénération.Enedis..MWh.", "Energie.produite.annuelle.Autres.filières.Enedis..MWh." )

colnames(conso_commune) <- c("annee", "code_commune", "nom_commune", "code_departement", "nom_departement", "code_region", "nom_region", "code_secteur", "conso_tot", "conso_moy")
colnames(conso_departement) <- c("annee", "code_departement", "nom_departement", "code_region", "nom_region", "code_secteur", "conso_tot", "conso_moy")
colnames(conso_region) <- c("annee", "code_region", "nom_region", "code_secteur", "conso_tot", "conso_moy")
colnames(prod_commune) <- c("annee", "nom_commune", "code_commune", "nom_departement", "code_departement", "nom_region", "code_region", "prod_photo", "prod_eol", "prod_hydr", "prod_bio", "prod_cogen", "prod_autre")
colnames(prod_departement) <- c("annee", "nom_departement", "code_departement", "nom_region", "code_region", "prod_photo", "prod_eol", "prod_hydr", "prod_bio", "prod_cogen", "prod_autre")
colnames(prod_region) <- c("annee", "nom_region", "code_region", "prod_photo", "prod_eol", "prod_hydr", "prod_bio", "prod_cogen", "prod_autre")

prod_region <- prod_region %>% group_by(annee, code_region) %>%
  summarise(nom_region = first(nom_region),
            prod_photo = sum(prod_photo),
            prod_eol = sum(prod_eol),
            prod_hydr = sum(prod_hydr),
            prod_bio = sum(prod_bio),
            prod_cogen = sum(prod_cogen),
            prod_autre = sum(prod_autre),
            .groups = 'drop')

prod_departement <- prod_departement %>% group_by(annee, code_departement) %>%
  summarise(nom_departement = first(nom_departement),
            code_region = first(code_region),
            nom_region = first(nom_region),
            prod_photo = sum(prod_photo),
            prod_eol = sum(prod_eol),
            prod_hydr = sum(prod_hydr),
            prod_bio = sum(prod_bio),
            prod_cogen = sum(prod_cogen),
            prod_autre = sum(prod_autre),
            .groups = 'drop')

prod_commune <- prod_commune %>% group_by(annee, code_commune) %>%
  summarise(nom_commune = first(nom_commune),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            code_region = first(code_region),
            nom_region = first(nom_region),
            prod_photo = sum(prod_photo),
            prod_eol = sum(prod_eol),
            prod_hydr = sum(prod_hydr),
            prod_bio = sum(prod_bio),
            prod_cogen = sum(prod_cogen),
            prod_autre = sum(prod_autre),
            .groups = 'drop')

prod_region <- prod_region %>%
  pivot_longer(
    cols = paste0("prod_", c("photo", "eol", "hydr", "bio", "cogen", "autre")), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = TRUE)

prod_departement <- prod_departement %>%
  pivot_longer(
    cols = paste0("prod_", c("photo", "eol", "hydr", "bio", "cogen", "autre")), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = TRUE)

prod_commune <- prod_commune %>%
  pivot_longer(
    cols = paste0("prod_", c("photo", "eol", "hydr", "bio", "cogen", "autre")), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = TRUE)

conso_region <- conso_region %>% group_by(annee, code_region, code_secteur) %>%
  summarise(nom_region = first(nom_region),
            conso_tot = sum(conso_tot),
            conso_moy = mean(conso_moy),
            .groups = 'drop')

conso_departement <- conso_departement %>% group_by(annee, code_departement, code_secteur) %>%
  summarise(nom_departement = first(nom_departement),
            code_region = first(code_region),
            nom_region = first(nom_region),
            conso_tot = sum(conso_tot),
            conso_moy = mean(conso_moy),
            .groups = 'drop')

conso_commune <- conso_commune %>% group_by(annee, code_commune, code_secteur) %>%
  summarise(nom_commune = first(nom_commune),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            code_region = first(code_region),
            nom_region = first(nom_region),
            conso_tot = sum(conso_tot),
            conso_moy = mean(conso_moy),
            .groups = 'drop')
