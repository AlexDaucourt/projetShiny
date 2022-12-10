ui <- dashboardPage(
  dashboardHeader(title="Bilan conso"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Résumé", tabName = "Résumé"),
      menuItem("Détails", tabName = "Détails")
    ),
    selectInput(
      inputId = "inp_choix_maille",
      label = "Choisissez votre maille d'intérêts",
      choices = c('Régions', 'Départements', 'Communes'),
      selected = 'Régions'
    )
    ,
    uiOutput(outputId = "maille")
    ,
    uiOutput(outputId = "maille_comp")
    ,
    selectInput(
      inputId = "inp_annee",
      label = "Choisissez l'année",
      choices = vect_annee,
      selected = 2011,
      multiple = TRUE
  ))
,
  dashboardBody(
    tabItems(
      tabItem(tabName = "Résumé",
        fluidRow(
          box(plotOutput("consommation")),
          box(plotOutput("production"))
        ),
        fluidRow(
          box(plotOutput("graph")),
          valueBoxOutput("cons_tot"),
          valueBoxOutput("prod_tot")
        )
      ),

      tabItem(tabName = "Détails",
        fluidRow(
          box(dataTableOutput("dtprod"))
        ),
        fluidRow(
          downloadLink('downloadData1', 'Download')
        ),
        fluidRow(
          box(dataTableOutput("dtcons"))
        ),
        fluidRow(
          downloadLink('downloadData2', 'Download')
        )
        )
    )
  )
)
