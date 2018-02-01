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
            h1("Carte des volumes en m3/ha des essences sur les points IFN"),
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
          tabPanel("Plot",
            h2("Analyse des accroissements courants annuels moyens sur 5 ans"),
            textOutput("text01"),
            plotOutput("plotzip1", height = 800)
          ),
          tabPanel("Map",
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
      sidebarPanel(width = 2,
        conditionalPanel(condition='input.inTabset02 != "Data"',
                          fileInput(
                            "datafile",
                            "Choose CSV file",
                            accept = c(".csv")
                          ),
                          helpText(
                            "Note: le fichier csv contient :",
                            "Essence, Diam, Htot, Hdec (même vide)."
                          ),
                          fileInput(
                            "mercufile",
                            "Choose CSV file",
                            accept = c(".csv")
                          ),
                          helpText(
                            "Note: le fichier csv contient :",
                            "CDiam, Tarif, Houppier (%), Hauteur."
                          ),
                          selectInput(
                            "Volcompare", "Volume à comparer:",
                            choices = list("Volume bois fort Total (7cm)" = "total", "Volume bois fort Tige (? cm)" = "tige"),
                            selected = "total"
                          ),
                          sliderInput(
                            "Decemerge", "Decoupe tige EMERGE (dec):",
                            min = 0, max = 80,
                            value = 7
                          ),
                          helpText("Note: diamètre à la découpe tige."),
                          sliderInput(
                            "ClasseInf", "Classe Diam Arbre:",
                            min = 10, max = 120,
                            value = c(25, 80), step = 5
                          ),
                          helpText(
                            "Note: classe de diamètre pour le",
                            "calcul, par défaut : 25 - 80."
                          ),
                          actionButton(
                            "update02", "Update", icon("refresh"),
                            class = "btn btn-primary"
                          ),
                          checkboxInput("mappoint", " Utiliser data IFN", FALSE),
                          conditionalPanel("input.mappoint", uiOutput("Essences02"))
        ),
        conditionalPanel(condition='input.inTabset02 == "Data"',
                         uiOutput("show_vars")
        )
      ),
      mainPanel(
        width = 10,
        tabsetPanel(id = "inTabset02",
          tabPanel("Definition",
            includeMarkdown("definition.md"),
            tableOutput("data_table")
          ),
          tabPanel("Plot",
            h1("Comparaison des volumes par application des tarifs (LOCAL/EMERGE/SchR/SchL)"),
            textOutput("text02"),
            verbatimTextOutput("Tartypvol"),
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
              )
            )
          ),
          tabPanel("Summary",
                   verbatimTextOutput("Resvol"),
                   verbatimTextOutput("summarycsv")
          ),
          tabPanel("Data",
                   fluidRow(
                     column(12,
                            fluidRow(column(7,
                                    h2("Cliquez sur save pour sauvegarder le tableau")
                            ),
                            br(),
                            column(5,
                                   downloadButton(
                                     "saveBtnData", "Save",
                                     class = "btn btn-primary"
                                   ))
                            )
                     )),
                   DT::dataTableOutput("tablecsv1")),
          tabPanel("Mercuriale",
                   fluidRow(
                     column(12,
                            fluidRow(column(7,
                                     h2("Modifiez la mercuriale, cliquez sur save pour la sauvegarder")
                            ),
                            br(),
                            column(5,
                                   downloadButton(
                                    "saveBtnMercu", "Save",
                                    class = "btn btn-primary"
                                  ))
                           )
                     )),
                   rHandsontableOutput("hot")),
          tabPanel("Map",
            h1("Cliquez sur la carte pour centrer la position des calculs"),
            fluidRow(
              column(
                width = 1,
                conditionalPanel("input.mappoint", selectInput("dt", "DT :", c(Choisir='', dtdata$iidtn_dt)))
              ),
              column(
                width = 1,
                conditionalPanel("input.mappoint && input.dt", selectInput("agence", "Agence :", c(Choisir='')))
              ),
              column(
                width = 2,
                conditionalPanel("input.agence && input.mappoint && input.dt", selectInput("forest", "Forêt :", c(Choisir='')))
              ),
              column(
                width = 1,
                conditionalPanel("input.forest && input.mappoint && input.agence && input.dt", selectInput("parcelle", "Parcelle :", c(Choisir='')))
              ),
              column(
                width = 1,
                useShinyjs(),
                conditionalPanel("input.forest && input.mappoint && input.agence && input.dt", uiOutput("samples")),
                conditionalPanel("input.forest && input.mappoint && input.agence && input.dt", selectInput("liste", NULL, c(Choisir='')))
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
                width = 1,
                conditionalPanel("input.mappoint", selectInput("zonecalc", "Zone calcul :", c('ser','rn250','rf250', 'pst')))
              ),
              column(
                width = 1,
                conditionalPanel("input.zonecalc == 'pst' && input.mappoint", uiOutput("pst"))
              )),
            fluidRow(
              column(
                width = 6,
                leafletOutput("map0201", width=730, height = 700)
              ),
              column(
                width = 6,
                leafletOutput("map0202", width=730, height = 700)
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
            h1("Recherche des tarifs (Krigeage à partir des volumes IFN)"),
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
