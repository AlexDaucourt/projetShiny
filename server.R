server <- function(input, output) {
  output$maille<- renderUI({
    if(input$inp_choix_maille == 'Régions'){
      vect_maille <- vect_region
    }else if(input$inp_choix_maille == 'Départements'){
      vect_maille <- vect_departements
    }
    else{
      vect_maille <- vect_commune
    }

    selectInput(inputId = 'inp_maille',
                label = 'Choisissez la maille',
                choices = vect_maille,
                selected = vect_maille[1])
  })

  output$maille_comp<- renderUI({
    if(input$inp_choix_maille == 'Régions'){
      vect_maille <- vect_region
    }else if(input$inp_choix_maille == 'Départements'){
      vect_maille <- vect_departements
    }
    else{
      vect_maille <- vect_commune
    }

    selectInput(inputId = 'inp_maille_comp',
                label = 'Maille de comparaison',
                choices = vect_maille,
                selected = NULL)
  })

  output$consommation = renderPlot({
    annee_sel <- input$inp_annee[1]
    if (input$inp_choix_maille == 'Régions'){
      reg <- conso_region %>% filter(nom_region == input$inp_maille | nom_region == input$inp_maille_comp)
      cr <- unique(reg$code_region)
      conso_a_plot <- conso_region %>% filter(annee == annee_sel) %>% filter(code_region %in% cr)
      ggplot(conso_a_plot) +
        aes(x = nom_region, y = conso_tot, fill = code_secteur) +
        geom_bar(stat = 'identity') +
        ggtitle("Répartition des consommations entre les différents secteurs pour l'année de référence 2011") +
        ylab("Consommation en Mwh") +
        xlab("Région")
    }
    else{
      if (input$inp_choix_maille == 'Départements'){
        dep <- conso_departement %>% filter(nom_departement == input$inp_maille | nom_departement == input$inp_maille_comp)
        cd <- unique(dep$code_departement)
        conso_a_plot <- conso_departement %>% filter(annee == annee_sel) %>% filter(code_departement %in% cd)
        ggplot(conso_a_plot) +
          aes(x = nom_region, y = conso_tot, fill = code_secteur) +
          geom_bar(stat = 'identity') +
          ggtitle("Répartition des consommations entre les différents secteurs pour l'année de référence 2011") +
          ylab("Consommation en Mwh") +
          xlab("Département")
      }
      else{
        com <- conso_commune %>% filter(nom_commune == input$inp_maille | nom_commune == input$inp_maille_comp)
        cc <- unique(com$code_commune)
        conso_a_plot <- conso_commune %>% filter(annee == annee_sel) %>% filter(code_commune %in% cc)
        ggplot(conso_a_plot) +
          aes(x = nom_region, y = conso_tot, fill = code_secteur) +
          geom_bar(stat = 'identity') +
          ggtitle("Répartition des consommations entre les différents secteurs pour l'année de référence 2011") +
          ylab("Consommation en Mwh") +
          xlab("Commune")
      }
    }

  })

  output$production = renderPlot({
    annee_sel <- input$inp_annee[1]
    if (input$inp_choix_maille == 'Régions'){
      reg <- prod_region %>% filter(nom_region == input$inp_maille | nom_region == input$inp_maille_comp)
      cr <- unique(reg$code_region)
      prod_a_plot <- prod_region %>% filter(annee == annee_sel) %>% filter(code_region %in% cr)
      ggplot(prod_a_plot) +
        aes(x = nom_region, y = prod_tot, fill = type_prod) +
        geom_bar(stat = 'identity') +
        ggtitle("Répartition des productions entre les différents types pour l'année de référence 2011") +
        ylab("Production en Mwh") +
        xlab("Région")
    }
    else{
      if (input$inp_choix_maille == 'Départements'){
        dep <- prod_departement %>% filter(nom_departement == input$inp_maille | nom_departement == input$inp_maille_comp)
        cd <- unique(dep$code_departement)
        prod_a_plot <- prod_departement %>% filter(annee == annee_sel) %>% filter(code_departement %in% cd)
        ggplot(prod_a_plot) +
          aes(x = nom_region, y = prod_tot, fill = type_prod) +
          geom_bar(stat = 'identity') +
          ggtitle("Répartition des productions entre les différents types pour l'année de référence 2011") +
          ylab("Production en Mwh") +
          xlab("Département")
      }
      else{
        com <- conso_commune %>% filter(nom_commune == input$inp_maille | nom_commune == input$inp_maille_comp)
        cc <- unique(com$code_commune)
        prod_a_plot <- prod_commune %>% filter(annee == annee_sel) %>% filter(code_commune %in% cc)
        ggplot(prod_a_plot) +
          aes(x = nom_region, y = prod_tot, fill = type_prod) +
          geom_bar(stat = 'identity') +
          ggtitle("Répartition des productions entre les différents types pour l'année de référence 2011") +
          ylab("Production en Mwh") +
          xlab("Commune")
      }
    }
  })

  output$graph = renderPlot({
    if (input$inp_choix_maille == 'Régions'){
      reg <- prod_region %>% filter(nom_region == input$inp_maille)
      cr <- unique(reg$code_region)
      prod_a_plot1 <- prod_region %>% filter(annee %in% input$inp_annee) %>% filter(code_region %in% cr)
      prod_a_plot2 <- conso_region %>% filter(annee %in% input$inp_annee) %>% filter(code_region %in% cr)

      ggplot() +
        geom_line(data=prod_a_plot1, aes(x = annee, y = prod_tot, color = type_prod)) +
        geom_line(data=prod_a_plot2, aes(x = annee, y = conso_tot, color = code_secteur)) +
        ggtitle("Evolution des consommations par secteur et des productions par type de production selon l'année pour la première maille (Sélectionner au moins 2 années)") +
        guides(color=guide_legend(title="Secteur et type de production")) +
        ylab("En Mwh")
    }
    else{
      if (input$inp_choix_maille == 'Départements'){
        dep <- prod_departement %>% filter(nom_departement == input$inp_maille)
        cd <- unique(dep$code_departement)
        prod_a_plot1 <- prod_departement %>% filter(annee %in% input$inp_annee) %>% filter(code_departement %in% cd)
        prod_a_plot2 <- conso_departement %>% filter(annee %in% input$inp_annee) %>% filter(code_departement %in% cd)

        ggplot() +
          geom_line(data=prod_a_plot1, aes(x = annee, y = prod_tot, color = type_prod)) +
          geom_line(data=prod_a_plot2, aes(x = annee, y = conso_tot, color = code_secteur)) +
          ggtitle("Evolution des consommations par secteur et des productions par type de production selon l'année pour la première maille (Sélectionner au moins 2 années)") +
          guides(color=guide_legend(title="Secteur et type de production")) +
          ylab("En Mwh")
      }
      else{
        com <- conso_commune %>% filter(nom_commune == input$inp_maille)
        cc <- unique(com$code_commune)
        prod_a_plot1 <- prod_commune %>% filter(annee %in% input$inp_annee) %>% filter(code_commune %in% cc)
        prod_a_plot2 <- conso_commune %>% filter(annee %in% input$inp_annee) %>% filter(code_commune %in% cc)

        ggplot() +
          geom_line(data=prod_a_plot1, aes(x = annee, y = prod_tot, color = type_prod)) +
          geom_line(data=prod_a_plot2, aes(x = annee, y = conso_tot, color = code_secteur)) +
          ggtitle("Evolution des consommations par secteur et des productions par type de production selon l'année pour la première maille (Sélectionner au moins 2 années)") +
          guides(color=guide_legend(title="Secteur et type de production")) +
          ylab("En Mwh")
      }
    }
  })

  output$cons_tot <- renderValueBox({
    if (input$inp_choix_maille == 'Régions'){
      reg <- conso_region %>% filter(nom_region == input$inp_maille)
      cr <- unique(reg$code_region)
      conso_tot <- conso_region %>% filter(annee %in% input$inp_annee) %>% filter(code_region %in% cr)
      tot <- sum(conso_tot$conso_tot)
      valueBox(value = as.integer(tot),
               subtitle = "Consommation totale sur la période pour la première maille")
    }
    else{
      if (input$inp_choix_maille == 'Départements'){
        dep <- conso_departement %>% filter(nom_departement == input$inp_maille)
        cd <- unique(dep$code_departement)
        conso_tot <- conso_departement %>% filter(annee %in% input$inp_annee) %>% filter(code_departement %in% cd)
        tot <- sum(conso_tot$conso_tot)
        valueBox(value = as.integer(tot),
                 subtitle = "Consommation totale sur la période pour la première maille")
      }
      else{
        com <- conso_commune %>% filter(nom_commune == input$inp_maille)
        cc <- unique(com$code_commune)
        conso_tot <- conso_commune %>% filter(annee %in% input$inp_annee) %>% filter(code_commune %in% cc)
        tot <- sum(conso_tot$conso_tot)
        valueBox(value = as.integer(tot),
                 subtitle = "Consommation totale sur la période pour la première maille")
      }
    }
  })

  output$prod_tot <- renderValueBox({
    if (input$inp_choix_maille == 'Régions'){
      reg <- prod_region %>% filter(nom_region == input$inp_maille)
      cr <- unique(reg$code_region)
      prod_tot <- prod_region %>% filter(annee %in% input$inp_annee) %>% filter(code_region %in% cr)
      tot <- sum(prod_tot$prod_tot)
      valueBox(value = as.integer(tot),
               subtitle = "Production totale sur la période pour la première maille")
    }
    else{
      if (input$inp_choix_maille == 'Départements'){
        dep <- prod_departement %>% filter(nom_departement == input$inp_maille)
        cd <- unique(dep$code_departement)
        prod_tot <- prod_departement %>% filter(annee %in% input$inp_annee) %>% filter(code_departement %in% cd)
        tot <- sum(prod_tot$prod_tot)
        valueBox(value = as.integer(tot),
                 subtitle = "Production totale sur la période pour la première maille")
      }
      else{
        com <- prod_commune %>% filter(nom_commune == input$inp_maille)
        cc <- unique(com$code_commune)
        prod_tot <- prod_commune %>% filter(annee %in% input$inp_annee) %>% filter(code_commune %in% cc)
        tot <- sum(prod_tot$prod_tot)
        valueBox(value = as.integer(tot),
                 subtitle = "Production totale sur la période pour la première maille")
      }
    }
  })
}



