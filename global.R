vect_annee <- unique(prod_region$annee)
vect_region <- unique(conso_region$nom_region)
vect_departements <- unique(conso_departement$nom_departement)
vect_commune <- unique(conso_commune$nom_commune)


shinyApp(ui = ui, server = server)
