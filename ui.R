ui <- fluidPage(titlePanel(""),

                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "inp_choix_maille",
                      label = "Choisissez votre maille d'intérêts",
                      choices = c('Régions', 'Départements', 'Communes'),
                      selected = 'Régions'
                    )
                    ,
                    uiOutput(outputId = "maille")
                    ,

                    selectInput(
                      inputId = "inp_annee",
                      label = "Choisissez l'année",
                      choices = vect_annee,
                      selected = vect_annee[1],
                      multiple = TRUE
                    )
                  )
                  ,


                  mainPanel(
                    tabsetPanel(
                      tabPanel("Résumé",h2(textOutput("maille_choisie_affiche")), plotOutput("consommation"), plotOutput("production")),
                      tabPanel("Détail")
                    )

                  )
                ))
