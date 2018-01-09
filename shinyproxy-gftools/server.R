

function(input, output, session) {
  ########### Onglet 00 ##############################################
  output$Essences <- renderUI({
    CodesEssIFN <- gftools::getData("IFNCODE") %>%
      dplyr::filter(donnee %in% "ESPAR")
    if (is.null(CodesEssIFN)) return(NULL)
    items <- unique(CodesEssIFN$libelle)
    names(items) <- items
    selectInput("Essences", "Essences:", multiple = TRUE, items)
  })

  output$plotcsv0 <- renderPlot({
    CodesEssIFN <- gftools::getData("IFNCODE") %>%
      dplyr::filter(donnee %in% "ESPAR")
    input$update00
    if (is.null(input$Essences)) return(NULL)
    if (input$update00 == 0) return(NULL)
    isolate({
      withCallingHandlers(
        {
          shinyjs::html(id = "text00", html = "Go ! ")
          p <- gftools::MapSpeciesSER(essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences)])
        },
        message = function(m) {
          shinyjs::html(id = "text00", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text00", html = m$message, add = TRUE)
        }
      )
      p$Carte
    })
  })

  ########### Onglet 01 ##############################################
  output$map <- renderLeaflet({
    input$update01
    if (is.null(input$datazip)) return(NULL)
    if (input$update01 == 0) return(NULL)
    isolate({
      utils::unzip(input$zipfile$datapath, exdir = dirname(input$zipfile$datapath))
      adm <- readOGR(dsn = dirname(input$zipfile$datapath), layer = basename(tools::file_path_sans_ext(input$datazip)), verbose = F)
      adm <- spTransform(adm, CRS("+init=epsg:4326"))
      popup <- paste0("<strong>Name: </strong>", adm$CCOD_FRT)
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = adm, weight = 2, fillColor = "yellow", popup = popup)
    })
  })

  # This function is repsonsible for loading in the selected zip file
  filezipdata <- reactive({
    infile <- input$zipfile
    if (is.null(infile)) return(NULL)
    fileshp <- grep(pattern = ".shp", unlist(lapply(infile$datapath, FUN = function(x) unzip(x, list = TRUE)[, 1])), value = TRUE)
  })

  output$datazip <- renderUI({
    dfzip <- filezipdata()
    if (is.null(dfzip)) return(NULL)
    items <- dfzip
    names(items) <- items
    selectInput("datazip", "Shapefile:", items)
  })

  output$plotzip1 <- renderPlot({
    input$update01
    if (is.null(input$datazip)) return(NULL)
    if (input$update01 == 0) return(NULL)
    isolate({
      validate(
        need(input$seuilnb >= 30, "A seuil nb at 30 or less produces bad statistics!")
      )
      withCallingHandlers(
        {
          shinyjs::html(id = "text01", html = "Go ! ")
          utils::unzip(input$zipfile$datapath, exdir = dirname(input$zipfile$datapath))
          p <- gftools::AccDIFNSER(fichier = paste0(dirname(input$zipfile$datapath), "/", input$datazip), enreg = input$enreg, seuilNb = input$seuilnb, periode = as.double(input$periode))
        },
        message = function(m) {
          shinyjs::html(id = "text01", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text01", html = m$message, add = TRUE)
        }
      )
      p$Graphe
    })
  })

  ########### Onglet 02 ##############################################
  # fichier des data
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) return(NULL)
    readr::read_tsv(
      infile$datapath,
      locale = readr::locale(encoding = "UTF-8", decimal_mark = ","),
      readr::cols(Essence = readr::col_character(), Diam = readr::col_integer(), Htot = readr::col_double(), Hdec = readr::col_double()),
      col_names = T
    )
  })

  # fichier mercuriale
  filemercu <- reactive({
    infile <- input$mercufile
    if (is.null(infile)) return(NULL)
    readr::read_tsv(
      infile$datapath,
      locale = readr::locale(encoding = "UTF-8", decimal_mark = ","),
      readr::cols(Cdiam = readr::col_integer(), Tarif = readr::col_character(), Houppier = readr::col_integer(), Hauteur = readr::col_double()),
      col_names = T
    )
  })

  # Tartypvol
  output$Tartypvol <- renderText({
    if (is.null(filemercu())) return(NULL)
    Tar <- unique(filemercu()$Tarif)
    Tar <- Tar[!is.na(Tar)]
    Tab <- ListTarONF2[which(ListTarONF2$sibois %in% Tar), c("ess", "TypeVol_tar", "dom_const", "TypeVol_tar", "DefVol", "diam_echant", "haut_echant", "entr1", "entr2", "sibois")]
    Txt <- paste0(
      Tab$sibois, " (", Tab$dom_const, "), est un tarif pour les essences (", Tab$ess, "),\n pour les diamètres de ", Tab$diam_echant,
      " cm, pour les hauteurs de ", Tab$haut_echant, " m,\n nécessite ", Tab$entr1, " et ", Tab$entr2, " et renvoi un ", Tab$DefVol, " (", Tab$TypeVol_tar, ").\n"
    )
  })

  output$Essences02 <- renderUI({
    CodesEssIFN <- gftools::getData("IFNCODE") %>%
      dplyr::filter(donnee %in% "ESPAR")
    if (is.null(CodesEssIFN)) return(NULL)
    items <- unique(CodesEssIFN$libelle)
    names(items) <- items
    selectInput("Essences02", "Essences:", multiple = TRUE, items, selected = c("Chene sessile", "Hetre"))
  })

  # select tabpanel Plot when click on update02
  observe({
    input$update02
    updateTabsetPanel(session, "inTabset02", selected = "Plot")
  })

  plotdata <- reactive({
    input$update02
    if (input$update02 == 0) return(NULL)
    CodesEssIFN <- gftools::getData("IFNCODE") %>%
      dplyr::filter(donnee %in% "ESPAR")
    isolate({
      withCallingHandlers(
        {
          shinyjs::html(id = "text02", html = "Go ! ")
          p <- gftools::TarifFindSch(
            fichier = input$datafile$datapath, mercuriale = input$mercufile$datapath, enreg = F,
            decemerge = input$Decemerge, classearbremin = input$ClasseInf[1],
            mappoint = input$mappoint, classearbremax = input$ClasseInf[2],
            essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)],
            latitude = input$latitude, longitude = input$longitude,
            typvolemerge = input$Volcompare
          )
        },
        message = function(m) {
          shinyjs::html(id = "text02", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text02", html = m$message, add = TRUE)
        }
      )
    })
  })

  output$plotcsv6 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      p <- plotdata()
      p$Graphe6
    })
  })

  output$plotcsv1 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      p <- plotdata()
      p$Graphe1
    })
  })

  output$plotcsv2 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      p <- plotdata()
      p$Graphe2
    })
  })

  output$plotcsv3 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      p <- plotdata()
      p$Graphe3
    })
  })

  output$plotcsv4 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      p <- plotdata()
      p$Graphe4
    })
  })

  output$plotcsv5 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      p <- plotdata()
      p$Graphe5
    })
  })

  tabdata <- reactive({
    input$update02
    if (input$update02 == 0) return(NULL)
    CodesEssIFN <- gftools::getData("IFNCODE") %>%
      dplyr::filter(donnee %in% "ESPAR")
    isolate({
      p <- gftools::TarifFindSch(
        fichier = input$datafile$datapath, mercuriale = input$mercufile$datapath, enreg = F,
        decemerge = input$Decemerge, classearbremin = input$ClasseInf[1],
        mappoint = input$mappoint, classearbremax = input$ClasseInf[2],
        essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)],
        latitude = input$latitude, longitude = input$longitude,
        typvolemerge = input$Volcompare
      )
      p$Tableau1
    })
  })

  output$map0201 <- renderLeaflet({
    leaflet() %>%
      setView(lat = 47.08, lng = 5.68, zoom = 6) %>%
      addTiles() %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Localisation du calcul !",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })

  output$map0202 <- renderLeaflet({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(pointclick$clickedMarker$lat)) return(NULL)
    p <- plotdata()
    if (!is.null(p$Placettes)) {
      zonemap <- sf::st_transform(p$Zone, crs = 4326)
      placettesmap <- sf::st_transform(p$Placettes, crs = 4326)
      popupzone <- paste0("<strong>SER: </strong>", p$Zone$codeser, " - ", p$Zone$NomSER)
      popupplacettes <- paste0("<strong>Année : </strong>", p$Placettes$yrs, " - idp : ", p$Placettes$idp)
      leaflet() %>%
        setView(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, zoom = input$map0201_zoom - 3) %>%
        addTiles() %>%
        addDrawToolbar(
          targetGroup='draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
        addPolygons(data = zonemap, weight = 2, fillColor = "yellow", popup = popupzone) %>%
        addCircles(data = placettesmap, popup=popupplacettes, weight = 3, radius=40,
                   color="#1f27b4", stroke = TRUE) %>%
        addMarkers(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, popup = "Localisation du calcul !")
    }
  })

  pointclick <- reactiveValues(clickedMarker = NULL)

  output$latitude <- renderUI({
    input$mappoint
    numericInput("latitude", "Latitude:", pointclick$clickedMarker$lat)
  })

  output$longitude <- renderUI({
    input$mappoint
    numericInput("longitude", "Longitude:", pointclick$clickedMarker$lng)
  })

  observeEvent(input$map0201_click, {
    pointclick$clickedMarker <- input$map0201_click
    leafletProxy("map0201") %>%
      clearMarkers() %>%
      addMarkers(
        lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng,
        popup = paste0("lat=", pointclick$clickedMarker$lat, ", lng=", pointclick$clickedMarker$lng)
      ) %>%
      setView(lng = pointclick$clickedMarker$lng, lat = pointclick$clickedMarker$lat, input$map0201_zoom)
  })

  # Resvol
  output$Resvol <- renderText({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(filemercu())) return(NULL)
    if (is.null(filedata())) return(NULL)
    res <- gftools::describeBy(tabdata(), group = tabdata()$Essence)
    Txt <- ""
    for (r in length(res):1) {
      Txt <- paste0(
        "Pour l'essence ", names(res[r]), ": l'estimation bois fort tige commerciale ONF cube ", round(100 * (res[[r]]["L_VbftigCom", "sum"] / res[[r]]["E_VbftigCom", "sum"] - 1), 0),
        "% du volume bois fort tige commercial EMERGE,\n- le volume bois fort tige commercial (L_VbftigCom) LOCAL de l'échantillon est de ", res[[r]]["L_VbftigCom", "sum"],
        " m3, le volume bois fort tige commercial (E_VbftigCom) EMERGE de l'échantillon est de ", res[[r]]["E_VbftigCom", "sum"],
        " m3,\n- le volume houppier (L_VHouppiers) LOCAL de l'échantillon est de ", res[[r]]["L_VHouppiers", "sum"],
        " m3, le volume houppier (E_VHouppiers) EMERGE de l'échantillon est de ",
        round(res[[r]]["E_Vbftot7cm", "sum"] - res[[r]]["E_VbftigCom", "sum"], 2),
        " m3,\n- soit un pourcentage de houppiers moyen LOCAL de l'échantillon (L_PHouppiers) de ",
        round(100 * res[[r]]["L_PHouppiers", "mean"], 0),
        "%, et un pourcentage de houppiers moyen EMERGE de l'échantillon (E_PHouppiers) de ",
        round(100 * res[[r]]["E_PHouppiers", "mean"], 0), "%.\n", Txt
      )
    }
    return(Txt)
  })

  # Generate a summary of the result ----
  output$summarycsv <- renderPrint({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(filemercu())) return(NULL)
    if (is.null(filedata())) return(NULL)
    gftools::describeBy(tabdata(), group = tabdata()$Essence)
  })

  output$show_vars <- renderUI({
    data <- tabdata()
    checkboxGroupInput("show_vars", "Colonnes à afficher :",
                       names(data), selected = names(data))
  })

  # Generate an HTML table view of the data
  output$tablecsv1 <- DT::renderDataTable({
    data <- tabdata()
    DT::datatable(data[, input$show_vars, drop = FALSE], options = list(pageLength = 10))
  })

  # Generate an HTML table view of the mercuriale data
  output$tablecsv2 <- DT::renderDataTable(
    filemercu(), options = list(
      pageLength = 15
    ), server = FALSE
  )

  ########## Onglet 03 ##############################################
  # This function is repsonsible for loading in the selected zip file
  filezipdata03 <- reactive({
    infile <- input$zipfile03
    if (is.null(infile)) return(NULL)
    fileshp <- grep(pattern = ".shp", unlist(lapply(infile$datapath, FUN = function(x) unzip(x, list = TRUE)[, 1])), value = TRUE)
  })

  output$datazip03 <- renderUI({
    dfzip <- filezipdata03()
    if (is.null(dfzip)) return(NULL)
    items <- dfzip
    names(items) <- items
    selectInput("datazip03", "Shapefile:", items)
  })

  # Generate an HTML table view of the data ----
  tab03 <- reactive({
    input$update03
    if (is.null(input$datazip03)) return(NULL)
    if (input$update03 == 0) return(NULL)
    isolate({
      validate(
        need(input$seuilnb03 >= 30, "A seuil nb at 30 or less produces bad statistics!")
      )
      withCallingHandlers(
        {
          shinyjs::html(id = "text03", html = "Go ! ")
          utils::unzip(input$zipfile03$datapath, exdir = dirname(input$zipfile03$datapath))
          p <- gftools::TarifIFNSER(
            fichier = paste0(dirname(input$zipfile03$datapath), "/", input$datazip03), enreg = input$enreg03, seuilNb = input$seuilnb03,
            seuilCircf = as.integer(input$seuilcirc * pi), res = input$res, distmax = input$distmax
          )
        },
        message = function(m) {
          shinyjs::html(id = "text03", html = m$message, add = TRUE)
        }
      )
      p$Tableau
    })
  })

  output$table03 <- DT::renderDataTable(
    tab03(), options = list(
      pageLength = 15
    ), server = FALSE
  )
}
