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

  output$consommation = renderPlot({
  })
  output$production = renderPlot({
  })

  output$maille_choisie_affiche <- renderText(input$inp_maille)
}


