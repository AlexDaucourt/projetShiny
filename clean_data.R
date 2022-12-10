library(dplyr)
library(tidyverse)

#On clean les données

conso_commune <- read.csv('data/consommation-electrique-par-secteur-dactivite-commune.csv', sep=";", encoding = "UTF-8")
conso_departement <- read.csv('data/consommation-electrique-par-secteur-dactivite-departement.csv', sep=";", encoding = "UTF-8")
conso_region <- read.csv('data/consommation-electrique-par-secteur-dactivite-region.csv', sep=";", encoding = "UTF-8")
prod_commune <- read.csv('data/production-electrique-par-filiere-a-la-maille-commune.csv', sep=";", encoding = "UTF-8")
prod_departement <- read.csv('data/production-electrique-par-filiere-a-la-maille-departement.csv', sep=";", encoding = "UTF-8")
prod_region <- read.csv('data/production-electrique-par-filiere-a-la-maille-region.csv', sep=";", encoding = "UTF-8")

prod_region[is.na(prod_region)] <- 0
prod_departement[is.na(prod_departement)] <- 0
prod_commune[is.na(prod_commune)] <- 0

conso_departement[is.na(conso_departement)] <- 0
conso_region[is.na(conso_region)] <- 0
conso_commune[is.na(conso_commune)] <- 0

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
    values_drop_na = FALSE)

prod_departement <- prod_departement %>%
  pivot_longer(
    cols = paste0("prod_", c("photo", "eol", "hydr", "bio", "cogen", "autre")), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = FALSE)

prod_commune <- prod_commune %>%
  pivot_longer(
    cols = paste0("prod_", c("photo", "eol", "hydr", "bio", "cogen", "autre")), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = FALSE)

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

#Calcul des lignes 'moyennes France'

moy <- prod_region %>% group_by(annee, type_prod) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_region=if_else(2>1, 0, 1)) %>%
                                  mutate(nom_region=if_else(2>1, "Moyenne France", ""))

prod_region <- bind_rows(prod_region, moy)

moy <- prod_departement %>% group_by(annee, type_prod) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_departement=if_else(2>1, 0, 1)) %>%
  mutate(nom_departement=if_else(2>1, "Moyenne France", ""))

prod_departement <- bind_rows(prod_departement, moy)

moy <- prod_commune %>% group_by(annee, type_prod) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_commune=if_else(2>1, 0, 1)) %>%
  mutate(nom_commune=if_else(2>1, "Moyenne France", ""))

prod_commune <- bind_rows(prod_commune, moy)


moy <- conso_region %>% group_by(annee, code_secteur) %>%
  summarise(conso_tot = mean(conso_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_region=if_else(2>1, 0, 1)) %>%
  mutate(nom_region=if_else(2>1, "Moyenne France", ""))

conso_region <- bind_rows(conso_region, moy)

moy <- conso_departement %>% group_by(annee, code_secteur) %>%
  summarise(conso_tot = mean(conso_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_departement=if_else(2>1, 0, 1)) %>%
  mutate(nom_departement=if_else(2>1, "Moyenne France", ""))

conso_departement <- bind_rows(conso_departement, moy)

moy <- conso_commune %>% group_by(annee, code_secteur) %>%
  summarise(conso_tot = mean(conso_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_commune=if_else(2>1, 0, 1)) %>%
  mutate(nom_commune=if_else(2>1, "Moyenne France", ""))

conso_commune <- bind_rows(conso_commune, moy)

prod_region[prod_region == "Nouvelle-Aquitaine"] <- "Nouvelle Aquitaine"


# Ajout des moyennes régionales et départementales


moy_depart <- na.omit(prod_departement) %>% group_by(annee, type_prod, code_region) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            nom_region = first(nom_region),
            .groups = 'drop')
moy_depart["code_departement"] <- moy_depart$code_region * 100
moy_depart["nom_departement"] <- paste("Moyenne",moy_depart$nom_region)

prod_departement <- bind_rows(prod_departement, moy_depart)


moy_commune1 <- na.omit(prod_commune) %>% group_by(annee, type_prod, code_region) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            nom_region = first(nom_region),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune1["code_commune"] <- moy_commune1$code_region * 10000
moy_commune1["nom_commune"] <- paste("Moyenne",moy_commune1$nom_region)

prod_commune <- bind_rows(prod_commune, moy_commune1)


moy_commune2 <- na.omit(prod_commune) %>% group_by(annee, type_prod, code_departement) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            code_region = first(code_region),
            nom_region = first(nom_region),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune2["code_commune"] <- moy_commune2$code_region * 20000
moy_commune2["nom_commune"] <- paste("Moyenne",moy_commune2$nom_departement)

prod_commune <- bind_rows(prod_commune, moy_commune2)




moy_depart <- na.omit(conso_departement) %>% group_by(annee, code_secteur, code_region) %>%
  summarise(conso_tot = mean(conso_tot, na.rm = TRUE),
            nom_region = first(nom_region),
            .groups = 'drop')
moy_depart["code_departement"] <- moy_depart$code_region * 100
moy_depart["nom_departement"] <- paste("Moyenne",moy_depart$nom_region)

conso_departement <- bind_rows(conso_departement, moy_depart)


moy_commune1 <- na.omit(conso_commune) %>% group_by(annee, code_secteur, code_region) %>%
  summarise(conso_tot = mean(conso_tot, na.rm = TRUE),
            nom_region = first(nom_region),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune1["code_commune"] <- moy_commune1$code_region * 10000
moy_commune1["nom_commune"] <- paste("Moyenne",moy_commune1$nom_region)

conso_commune <- bind_rows(conso_commune, moy_commune1)


moy_commune2 <- na.omit(conso_commune) %>% group_by(annee, code_secteur, code_departement) %>%
  summarise(conso_tot = mean(conso_tot, na.rm = TRUE),
            code_region = first(code_region),
            nom_region = first(nom_region),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune2["code_commune"] <- moy_commune2$code_region * 20000
moy_commune2["nom_commune"] <- paste("Moyenne",moy_commune2$nom_departement)

conso_commune <- bind_rows(conso_commune, moy_commune2)
