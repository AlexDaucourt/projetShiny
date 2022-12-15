Projet R Shiny : Bilan électrique de consommation

Il faut tout d'abord télécharger ces 6 tables et les déposer dans un dossier "data" dans le projet R :

https://data.enedis.fr/explore/dataset/consommation-electrique-par-secteur-dactivite-region/information/
https://data.enedis.fr/explore/dataset/consommation-electrique-par-secteur-dactivite-departement/information/?disjunctive.code_categorie_consommation&disjunctive.code_grand_secteur
https://data.enedis.fr/explore/dataset/consommation-electrique-par-secteur-dactivite-commune/information/
https://data.enedis.fr/explore/dataset/production-electrique-par-filiere-a-la-maille-region/information/
https://data.enedis.fr/explore/dataset/production-electrique-par-filiere-a-la-maille-departement/information/
https://data.enedis.fr/explore/dataset/production-electrique-par-filiere-a-la-maille-commune/information/

Ensuite, il faut lancer le fichier "clean_data.R", puis le fichier "ui.R", puis le "server.R" et enfin le "global.R" pour lancer l'application Shiny
