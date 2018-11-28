fluidPage(
  # shinythemes::themeSelector(),
  navbarPage(
    id = "navbar",
    theme = "default",
    "gftools",
    ########### Onglet 00 ##############################################
    tabPanel(
      "00 - Carte volume/essence (IFN)",
      tags$head(
        tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
      ),
      shinyjs::useShinyjs(),
      sidebarPanel(
        width = 2,
        uiOutput("Essences"),
        actionButton(
          "update00", "Update", icon("refresh"),
          class = "btn btn-primary"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot",
            h2("Carte des volumes en m3/ha des essences sur les points IFN"),
            textOutput("text00"),
            fluidRow(
              plotOutput("plotcsv0", height = 700)
            )
          )
        )
      )
    ),
    ########### Onglet 01 ##############################################
    tabPanel(
      "01 - Accroissement IR5 (IFN)",
      tags$head(
        tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
      ),
      shinyjs::useShinyjs(),
      sidebarPanel(
        width = 2,
        fileInput(
          "zipfile",
          "Choose ZIP file",
          accept = c(".zip")
        ),
        helpText(
          "Note: le fichier zip contient à minima ",
          "shape.shp, shape.shx et shape.dbf."
        ),
        uiOutput("datazip"),
        sliderInput(
          "periode",
          "Year pro group:",
          value = 12,
          min = 1,
          max = 12
        ),
        numericInput("seuilnb", "Seuil nb:", 30, min = 1, max = 20000),
        checkboxInput("enreg", " Save result", FALSE),
        actionButton(
          "update01", "Update", icon("refresh"),
          class = "btn btn-primary"
        )
      ),
      mainPanel(
        width = 10,
        tabsetPanel(
          tabPanel(
            "Plot",
            h2("Analyse des accroissements courants annuels moyens sur 5 ans"),
            textOutput("text01"),
            plotOutput("plotzip1", height = 800)
          ),
          tabPanel(
            "Map",
            leafletOutput("map", height = 800)
          )
        )
      )
    ),
    ########### Onglet 02 ##############################################
    tabPanel(
      "02 - Comparaison Tarifs (LOCAL/EMERGE)",
      tags$head(
        tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
      ),
      shinyjs::useShinyjs(),
      sidebarPanel(
        width = 2,
        conditionalPanel(
          condition = 'input.inTabset02 != "Resultat"',
          helpText("Comparaison du volume bois fort Total (découpe 7cm)"),
          conditionalPanel("!input.hot", fileInput(
            "datafile",
            "Choisir Data CSV",
            accept = c(".csv")
          )),
          conditionalPanel(
            "input.forest && input.agence && input.dt && input.parcelle",
            selectInput("listedata", "Choisir Data BDD", c("Choisir DATA" = ""))
          ),
          conditionalPanel("!input.hot", fileInput(
            "mercufile",
            "Choisir Mercu CSV",
            accept = c(".csv")
          )),
          conditionalPanel(
            "input.forest && input.agence && input.dt && input.parcelle && input.listedata",
            selectInput("listemercu", "Choisir Mercu BDD", c("Choisir MERCU" = ""))
          ),
          fileInput(
            "clausefile",
            "Choisir CCT CSV",
            accept = c(".csv")
          ),
          conditionalPanel(
            "input.agence && input.dt",
            selectInput("listeclause", "Choisir Clause BDD", c("Choisir Clause" = ""))
          ),
          sliderInput(
            "ClasseInf", "Classe Diam Arbre:",
            min = 10, max = 120,
            value = c(25, 80), step = 5
          ),
          helpText(
            "Note: classe de diamètre pour le",
            "calcul, par défaut : 25 - 80."
          ),
          actionButton("update02", "Update", icon("refresh"), class = "btn btn-primary"),
          checkboxInput("mappoint", " Utiliser data IFN", FALSE),
          conditionalPanel("input.mappoint", uiOutput("Essences02")),
          conditionalPanel(
            'input.inTabset02 == "Data/Mercuriale/Clause"',
            sliderInput("exercice", "Exercice:", min = 10, max = 19, value = 17)
          ),
          conditionalPanel(
            'input.inTabset02 == "Data/Mercuriale/Clause" || input.inTabset02 == "Map" || input.inTabset02 == "Graphe"',
            selectInput("dt", "DT :", c(Choisir = "", dtdata$iidtn_dt))
          ),
          conditionalPanel(
            '(input.inTabset02 == "Data/Mercuriale/Clause" || input.inTabset02 == "Map" || input.inTabset02 == "Graphe") && input.dt',
            selectInput("agence", "Agence :", c(Choisir = ""))
          ),
          conditionalPanel(
            'input.inTabset02 == "Data/Mercuriale/Clause" && input.agence && input.mappoint',
            downloadButton(
              "reportagence", "Rapport Agence",
              class = "btn btn-primary"
          )),
          conditionalPanel(
            'input.inTabset02 == "Data/Mercuriale/Clause" && input.agence',
            actionButton("update022", "Comparaison des résultats >>>", icon("refresh"), class = "btn btn-primary")
          )
        ),
        conditionalPanel(
          condition = 'input.inTabset02 == "Resultat"',
          uiOutput("show_vars")
        )
      ),
      mainPanel(
        width = 10,
        tabsetPanel(
          id = "inTabset02",
          tabPanel(
            "Definition",
            includeMarkdown("definition.md"),
            tableOutput("data_table")
          ),
          tabPanel(
            "Protocole",
            includeMarkdown("protocole.md")
          ),
          tabPanel(
            "Graphe",
            h2("Comparaison des volumes par application des tarifs (LOCAL/EMERGE/SchR/SchL/Algan)"),
            textOutput("text02"),
            verbatimTextOutput("Samnbtig"),
            verbatimTextOutput("Tartypvol"),
            verbatimTextOutput("Clauseter"),
            fluidRow(
              column(
                width = 4,
                plotOutput("plotcsv1", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv6", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv2", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv3", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv4", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv5", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv7", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv8", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv9", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv10", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv11", height = 700)
              ),
              column(
                width = 4,
                plotOutput("plotcsv12", height = 700)
              )
            )
          ),
          tabPanel(
            "En bref",
            verbatimTextOutput("Resvol"),
            verbatimTextOutput("summarycsv")
          ),
          tabPanel(
            "Resultat",
            fluidRow(
              column(
                12,
                fluidRow(
                  column(
                    6,
                    h2("Cliquez sur save pour sauvegarder le tableau")
                  ),
                  br(),
                  column(
                    3,
                    downloadButton(
                      "saveBtnData", "Save",
                      class = "btn btn-primary"
                    )
                  ),
                  column(
                    2,
                    conditionalPanel("input.forest && input.agence && input.dt && input.parcelle", textInput(
                      "nomData", NULL
                    ))
                  ),
                  column(
                    1,
                    conditionalPanel("input.forest && input.agence && input.dt && input.parcelle", actionButton(
                      "saveBtnDataBDD", "Save BDD",
                      class = "btn btn-primary"
                    ))
                  )
                )
              )
            ),
            DT::dataTableOutput("tablecsv1")
          ),
          tabPanel(
            "Data/Mercuriale/Clause",
            fluidRow(
              column(
                12,
                fluidRow(
                  column(
                    6,
                    h3("Modifiez la mercuriale, cliquez sur save pour la sauvegarder")
                  ),
                  br(),
                  column(
                    3,
                    downloadButton(
                      "saveBtnMercu", "Save",
                      class = "btn btn-primary"
                    )
                  ),
                  column(
                    2,
                    conditionalPanel("input.forest && input.agence && input.dt && input.parcelle && input.listedata", textInput(
                      "nomMercu", NULL
                    ))
                  ),
                  column(
                    1,
                    conditionalPanel("input.forest && input.agence && input.dt && input.parcelle && input.listedata", actionButton(
                      "saveBtnMercuBDD", "Save BDD",
                      class = "btn btn-primary"
                    ))
                  )
                )
              )
            ), # end fluidrow
            fluidRow(
              column(
                2,
                rHandsontableOutput("datahot")
              ),
              column(
                2,
                rHandsontableOutput("mercuhot")
              ),
              column(
                3,
                selectInput("espar", "Essence :", c(Choisir = "")),
                fluidRow(
                  column(
                    6,
                    selectInput("tarif", "Tarif :", c(Choisir = "", listetarif))
                  ),
                  column(
                    4,
                    conditionalPanel("input.tarif && input.espar", selectInput("numtarif", "Numéro :", c(Choisir = "", 1:20)))
                  )
                ),
                conditionalPanel("input.tarif", actionButton(
                  "transmercu", "<<< Transférer la mercuriale EMERGE <<<",
                  class = "btn btn-primary"
                )),
                h5("Cahier des clauses territoriales :"),
                rHandsontableOutput("clausehot")
              ),
              column(
                2,
                rHandsontableOutput("reshot")
              ),
              column(
                3,
                plotOutput("plotcsv13", height = 700)
              )
            ), # end fluidrow
            fluidRow(
              column(
                4,
                conditionalPanel("input.agence", h3("Comparaison des résultats sur l'agence pour l'essence"))
              ),
              column(
                4,
                conditionalPanel("input.agence", uiOutput("Essences03"))
              ),
              column(
                12,
                plotOutput("plotdatacab")
              ),
              column(
                12,
                # fluidRow(column(6, conditionalPanel("input.agence", h3(" ")))),
                # br(),
                verbatimTextOutput("localmercu")
              )
            ), # end fluidrow
            fluidRow(
              column(12, textOutput("text04"))
            )
          ),
          tabPanel(
            "Map",
            h2("Cliquez sur la carte pour centrer la position des calculs"),
            fluidRow(
              column(
                width = 4,
                conditionalPanel("input.agence && input.dt", selectInput("forest", "Forêt :", c(Choisir = "")))
              ),
              column(
                width = 1,
                conditionalPanel("input.forest && input.agence && input.dt", selectInput("parcelle", "Parcelle :", c(Choisir = "")))
              ),
              column(
                width = 2,
                conditionalPanel("input.mappoint", uiOutput("latitude"))
              ),
              column(
                width = 2,
                conditionalPanel("input.mappoint", uiOutput("longitude"))
              ),
              column(
                width = 2,
                conditionalPanel("input.mappoint", selectInput("zonecalc", "Zone calcul :", c("ser", "rn250", "rf250", "pst")))
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel("input.zonecalc == 'pst' && input.mappoint", uiOutput("pst"))
              )
            ),
            fluidRow(
              column(
                width = 6,
                leafletOutput("map0201", width = 730, height = 700)
              ),
              column(
                width = 6,
                leafletOutput("map0202", width = 730, height = 700)
              )
            )
          )
        )
      )
    ),
    ########### Onglet 03 ##############################################
    tabPanel(
      "03 - Recherche des tarifs (Krigeage Volume IFN)",
      tags$head(
        tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
      ),
      shinyjs::useShinyjs(),
      sidebarPanel(
        width = 2,
        fileInput(
          "zipfile03",
          "Choose ZIP file",
          accept = c(".zip")
        ),
        helpText(
          "Note: le fichier zip contient à minima",
          "shape.shp, shape.shx et shape.dbf."
        ),
        uiOutput("datazip03"),
        sliderInput(
          "seuilcirc",
          "Seuil diametre:",
          value = 15,
          min = 10,
          max = 200,
          step = 5
        ),
        helpText(
          "Note: seuil minimal pour que",
          "l'arbre soit retenu."
        ),
        sliderInput(
          "seuilnb03",
          "Seuil nb:",
          value = 30,
          min = 10,
          max = 2000,
          step = 10
        ),
        helpText(
          "Note: seuil minimal pour qu'une",
          "essence soit retenue."
        ),
        sliderInput(
          "res",
          "Resolution:",
          value = 500,
          min = 100,
          max = 20000,
          step = 100
        ),
        helpText(
          "Note: résolution de la grille servant ",
          "de support au krigeage."
        ),
        sliderInput(
          "distmax",
          "Distance max:",
          value = 20000,
          min = 1000,
          max = 100000,
          step = 1000
        ),
        helpText(
          "Note: distance maximale pour la",
          "recherche de placette IFN."
        ),
        checkboxInput("enreg03", " Save result", FALSE),
        actionButton(
          "update03", "Update", icon("refresh"),
          class = "btn btn-primary"
        )
      ),
      # Main panel for displaying outputs ----
      mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            h2("Recherche des tarifs (Krigeage à partir des volumes IFN)"),
            DT::dataTableOutput("table03"),
            textOutput("text03")
          )
        )
      )
    ),
    absolutePanel(
      bottom = 10,
      left = 10,
      draggable = F,
      width = "100%",
      height = "auto",
      p(a(icon("github fa-2x"), href = "https://github.com/pobsteta/shinyproxy-gftools#boards?repos=116554829", target = "_blank"))
    )
  )
)
