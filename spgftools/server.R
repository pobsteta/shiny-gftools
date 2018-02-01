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
  
  # select tabpanel Map when click on mappoint
  observe({
    input$mappoint
    updateTabsetPanel(session, "inTabset02", selected = "Map")
  })

  plotdata <- reactive({
    input$update02
    if (input$update02 == 0) return(NULL)
    CodesEssIFN <- gftools::getData("IFNCODE") %>%
        dplyr::filter(donnee %in% "ESPAR")
    isolate({
      withProgress(message = 'Calculs en cours', style = 'notification', value = 0.75, {
        Sys.sleep(0.25)
        withCallingHandlers(
          {
            shinyjs::html(id = "text02", html = "Go ! ")
            p <- gftools::TarifFindSch(
              fichier = input$datafile$datapath, mercuriale = input$mercufile$datapath, enreg = F,
              decemerge = input$Decemerge, classearbremin = input$ClasseInf[1],
              mappoint = input$mappoint, classearbremax = input$ClasseInf[2],
              essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)],
              latitude = input$latitude, longitude = input$longitude,
              typvolemerge = input$Volcompare,
              zonecalc = zonecalcul()
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
      incProgress(0.8)
    })
    incProgress(1)
    p
  })

  output$plotcsv1 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = 'Création du graphique (1/6)', style = 'notification', value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(0.8)
      })
      incProgress(1)
      p$Graphe1
    })
  })

  output$plotcsv2 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = 'Création du graphique (2/6)', style = 'notification', value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(0.8)
      })
      incProgress(1)
      p$Graphe2
    })
  })

  output$plotcsv3 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = 'Création du graphique (3/6)', style = 'notification', value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(0.8)
      })
      incProgress(1)
      p$Graphe3
    })
  })

  output$plotcsv4 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = 'Création du graphique (4/6)', style = 'notification', value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(0.8)
      })
      incProgress(1)
      p$Graphe4
    })
  })

  output$plotcsv5 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = 'Création du graphique (5/6)', style = 'notification', value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(0.8)
      })
      incProgress(1)
      p$Graphe5
    })
  })
  
  output$plotcsv6 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = 'Création du graphique (6/6)', style = 'notification', value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(0.8)
      })
      incProgress(1)
      p$Graphe6
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
        typvolemerge = input$Volcompare,
        zonecalc = zonecalcul()
      )
      p$Tableau1
    })
  })
  
  label <- reactive({
    if(input$parcelle != '82') {
      shinyjs::disable("samples")
      label <- "Samples (0)"
    } else {
      shinyjs::enable("samples")
      label <- "Samples (*)"
    }
  })
  
  output$samples <- renderUI({
    actionButton("samples", label = label(), class="btn btn-primary")
  })

  output$latitude <- renderUI({
    input$mappoint
    frt <- polyfrt()
    numericInput("latitude", "Latitude :", pointclick$clickedMarker$lat)
  })

  output$longitude <- renderUI({
    input$mappoint
    frt <- polyfrt()
    numericInput("longitude", "Longitude :", pointclick$clickedMarker$lng)
  })
  
  outAGC = reactive({
    agc <- agencedata %>%
      filter(grepl(input$dt, iidtn_agc))
    agc$iidtn_agc
  })
  
  outFRT = reactive({
   frt <- forestdata %>%
     filter(ccod_cact == input$agence)
   frt$ccod_frt
  })
  
  outPRF = reactive({
    prf <- parcelledata %>%
      filter(ccod_frt == input$forest, ccod_cact == input$agence)
    prf$ccod_prf
  })
  
  outPST = reactive({
    pst <- pstdata %>%
      filter(grepl(input$agence, ccod_cact))
    pst$ccod_ut
  })
  
  output$pst <- renderUI({
    selectizeInput("pst", "UT :", choices = outPST(), multiple = TRUE, 
                   options = list(placeholder = 'Choisir'))
  })
  
  observeEvent(input$dt, {
    updateSelectInput(session, "agence", choices = c(Choisir='', outAGC()))
  })
  
  observeEvent(input$agence, {
    updateSelectInput(session, "forest", choices = c(Choisir='', outFRT()))
  })
  
  observeEvent(input$forest, {
    updateSelectInput(session, "parcelle", choices = c(Choisir='', outPRF()))
  })
  
  polydt <- reactive({
    polydt <- dtdata %>%
      filter(iidtn_dt == input$dt) 
    polydt
  })
  
  observeEvent(input$dt, {
    dt <- polydt()
    if (!is.null(input$dt)) {
      dt <- st_centroid(dt) %>%
        st_geometry()
      leafletProxy('map0201') %>%
        setView(lat = st_coordinates(dt)[2], lng = st_coordinates(dt)[1], zoom = 8)
    }
  })
  
  polyagc <- reactive({
    polyagc <- agencedata %>%
      filter(iidtn_agc == input$agence) 
    polyagc
  })
  
  observeEvent(input$agence, {
    agc <- polyagc()
    if (!is.null(input$agence)) {
      agc <- st_centroid(agc) %>%
        st_geometry()
      leafletProxy('map0201') %>%
        setView(lat = st_coordinates(agc)[2], lng = st_coordinates(agc)[1], zoom = 10)
    }
  })
  
  polyfrt <- reactive({
    polyfrt <- forestdata %>%
      filter(ccod_frt == input$forest) 
    polyfrt
  })
  
  observeEvent(input$forest, {
    frt <- polyfrt()
    if (!is.null(input$forest)) {
      frt <- st_centroid(frt) %>%
        st_geometry()
      leafletProxy('map0201') %>%
        setView(lat = st_coordinates(frt)[2], lng = st_coordinates(frt)[1], zoom = 12)
    }
  })
  
  polyprf <- reactive({
    polyfrt <- parcelledata %>%
      filter(ccod_prf == input$parcelle, ccod_frt == input$forest)
    polyfrt
  })
  
  observeEvent(input$parcelle, {
    prf <- polyprf()
    if (!is.null(input$parcelle)) {
      prf <- st_centroid(prf) %>%
        st_geometry()
      leafletProxy('map0201') %>%
        setView(lat = st_coordinates(prf)[2], lng = st_coordinates(prf)[1], zoom = 14)
    }
  })
  
  polypst <- reactive({
    polypst <- pstdata %>%
      filter(ccod_ut == input$pst)
    polypst
  })
  
  observeEvent(input$pst, {
    pst <- polypst()
    if (!is.null(input$pst)) {
      pst <-st_centroid(st_convex_hull(st_union(pst))) %>%
        st_geometry()
      leafletProxy('map0201') %>%
        setView(lat = st_coordinates(pst)[2], lng = st_coordinates(pst)[1], zoom = 10)
    }
  })
  
  output$map0201 <- renderLeaflet({
    agc <- polyagc()
    popupagc <- paste0("<strong>", agc$iidtn_agc, " - ", agc$llib_agc, "</strong>")
    frt <- polyfrt()
    popupfrt <- paste0("<strong>", frt$llib2_frt, "</strong>")
    prf <- polyprf()
    popupprf <- paste0("<strong>Parcelle : </strong>", prf$ccod_prf)
    leaflet() %>%
      setView(lat = 47.08, lng = 5.68, zoom = 6) %>%
      addTiles() %>%
      addPolygons(data = polyfrt(), weight = 2, color = 'green', fillColor = 'green', popup = popupfrt, group = "Forêt") %>%
      addPolygons(data = polyprf(), weight = 2, color = 'red', fillColor = 'red', popup = popupprf, group = "Parcelle") %>%
      addLayersControl(overlayGroups = c("Forêt", "Parcelle"), options = layersControlOptions(collapsed = TRUE))
  })
  
  pointclick <- reactiveValues(clickedMarker = NULL)
  
  output$map0202 <- renderLeaflet({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(pointclick$clickedMarker$lat)) return(NULL)
    p <- plotdata()
    zonemap <- sf::st_transform(p$Zone, crs = 4326)
    placettesmap <- sf::st_transform(p$Placettes, crs = 4326)
    popupzone <- paste0("<strong>", p$Zone$couche, " (id/nom) : </strong>", p$Zone$id, "/", p$Zone$name)
    popupplacettes <- paste0("<strong>Année : </strong>", p$Placettes$yrs, " - idp : ", p$Placettes$idp)
    leaflet() %>%
      setView(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, zoom = input$map0201_zoom - 3) %>%
      addTiles() %>%
      # addDrawToolbar(
      #   targetGroup='draw',
      #   editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addPolygons(data = zonemap, weight = 2, fillColor = "yellow", popup = popupzone, group = "Zone de calcul") %>%
      addCircles(data = placettesmap, popup=popupplacettes, weight = 3, radius = 60, color = '#ff00e6', fill = TRUE, stroke = TRUE, fillOpacity = 0.1, group = "Placette IFN") %>%
      addMarkers(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, popup = "Localisation du calcul !") %>%
      addLayersControl(overlayGroups = c("Zone de calcul", "Placette IFN"), options = layersControlOptions(collapsed = TRUE))
  })
  
  zonecalcul <- eventReactive({
    input$update02
  }, {
      txt <- paste0("'", input$zonecalc, "' as couche, ")
      if (input$zonecalc == 'ser') {
        txt <- paste0(txt, " name, id,")
      } else if (input$zonecalc == 'rn250') {
        txt <- paste0(txt, " regionn as name, id,")
      } else if (input$zonecalc == 'rf250') {
        txt <- paste0(txt, " regiond as name, id,")
      } else if (input$zonecalc == 'pst') {
        txtpst <- paste0("WITH w2 AS (WITH w1 AS (SELECT clib_pst AS name, ccod_ut AS id, geom FROM pst WHERE ccod_ut::integer IN (", paste(input$pst, collapse=","), 
                         ")) SELECT name, id, st_union(geom) AS geom FROM w1 GROUP BY name, id)")
      }
      if (input$zonecalc != 'pst') {
        BDDQueryONF(query = paste0("SELECT ", txt, " geom FROM ", 
                                 input$zonecalc, " s WHERE st_dwithin(st_transform(st_setsrid(st_makepoint(",
                                 pointclick$clickedMarker$lng, ",",
                                 pointclick$clickedMarker$lat, "),4326),2154), s.geom,0)")) %>%
        sf::st_transform(crs=2154)
      } else {
        print(paste0(txtpst, " SELECT ", txt, "string_agg(name, ',') AS name, string_agg(id, ',') AS id, st_union(geom) AS geom FROM w2"))
        BDDQueryONF(query = paste0(txtpst, " SELECT ", txt, "string_agg(name, ',') AS name, string_agg(id, ',') AS id, st_union(geom) AS geom FROM w2")) %>%
          sf::st_transform(crs=2154)
      }
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
    withProgress(message = 'Comparaison des résultats', style = 'notification', value = 0.5, {
      Sys.sleep(0.25)
      resv <- gftools::describeBy(tabdata(), group = tabdata()$Essence)
      incProgress(1)
    })
    Txt <- ""
    for (r in length(resv):1) {
      Txt <- paste0(
        "Pour l'essence ", names(resv[r]), ", l'estimation ONF cube ", round(100 * (resv[[r]]["L_VbftigCom", "sum"] / resv[[r]]["E_VbftigCom", "sum"] - 1), 0),
        "% du volume bois fort tige commercial EMERGE et ", round(100 * (resv[[r]]["L_Vbftot7cm", "sum"] / resv[[r]]["E_Vbftot7cm", "sum"] - 1), 0), 
        "% du volume bois fort total EMERGE :\n- le volume bois fort tige commercial (L_VbftigCom) LOCAL de l'échantillon est de ", resv[[r]]["L_VbftigCom", "sum"],
        " m3, le volume bois fort tige commercial (E_VbftigCom) EMERGE de l'échantillon est de ", resv[[r]]["E_VbftigCom", "sum"],
        " m3,\n- le volume houppier (L_VHouppiers) LOCAL de l'échantillon est de ", resv[[r]]["L_VHouppiers", "sum"],
        " m3, le volume houppier (E_VHouppiers) EMERGE de l'échantillon est de ",
        round(resv[[r]]["E_Vbftot7cm", "sum"] - resv[[r]]["E_VbftigCom", "sum"], 2),
        " m3,\n- soit un pourcentage de houppiers moyen LOCAL de l'échantillon (L_PHouppiers) de ",
        round(100 * resv[[r]]["L_PHouppiers", "mean"], 0),
        "%, et un pourcentage de houppiers moyen EMERGE de l'échantillon (E_PHouppiers) de ",
        round(100 * resv[[r]]["E_PHouppiers", "mean"], 0), "%.\n", Txt
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
    withProgress(message = 'Tableau des résultats', style = 'notification', value = 0.5, {
      Sys.sleep(0.25)
      resv <- gftools::describeBy(tabdata(), group = tabdata()$Essence)
      incProgress(1)
    })
    resv
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
  
  # save data
  output$saveBtnData <- downloadHandler( 
    # Nom par défaut :
    filename = function() {
      paste0("Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(tabdata(), file, row.names = FALSE) 
    }
  )
  
  # save mercuriale
  output$saveBtnMercu <- downloadHandler(
    # Nom par défaut :
    filename = function() {
      paste0('Mercu_', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(filemercu(), file, row.names = FALSE)
    }
  )
  
  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = filemercu()
    }
    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

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
