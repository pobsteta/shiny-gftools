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
      withCallingHandlers({
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
      withCallingHandlers({
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

  observeEvent(input$datafile, {
    filedata()
    updateTextInput(session, "nomData", value = substr(input$datafile$name, 1, nchar(input$datafile$name) - 4))
    updateTabsetPanel(session, "inTabset02", selected = "Graphe")
  })

  observeEvent(input$mercufile, {
    filemercu()
    updateTextInput(session, "nomMercu", value = substr(input$mercufile$name, 1, nchar(input$mercufile$name) - 4))
    updateTabsetPanel(session, "inTabset02", selected = "Graphe")
  })

  observeEvent(input$clausefile, {
    fileclause()
    updateTabsetPanel(session, "inTabset02", selected = "Graphe")
  })

  observe({
    # enregistre la mercuriale, les clauses
    # et les data apres chaque changement
    if (!is.null(input$mercuhot)) {
      readr::write_tsv(hot_to_r(input$mercuhot), mname)
      print(paste("observe m:", mname))
    }
    if (!is.null(input$datahot)) {
      readr::write_tsv(hot_to_r(input$datahot), fname)
      print(paste("observe f:", fname))
    }
    if (!is.null(input$clausehot)) {
      readr::write_tsv(hot_to_r(input$clausehot), cname)
      print(paste("observe c:", cname))
    }
    if (!is.null(input$cathot)) {
      readr::write_tsv(hot_to_r(input$cathot), tname)
      print(paste("observe t:", tname))
    }
  })


  # fichier des data
  filedata <- reactive({
    datafile <- input$datafile
    if (is.null(datafile)) return(NULL)
    # tableau htot=f(diam) de l echantillon
    echant <<- NULL
    echanttab3 <<- NULL
    echanttab4 <<- NULL
    dataf <<- readr::read_tsv(
      datafile$datapath,
      locale = readr::locale(encoding = "UTF-8", decimal_mark = ","),
      readr::cols(code = readr::col_character(), diam = readr::col_integer(), htot = readr::col_double(), hdec = readr::col_double()),
      col_names = T
    )
    readr::write_tsv(dataf, fname)
    print(paste("filedata:", fname))
    output$datahot <<- renderRHandsontable({
      rhandsontable(dataf, readOnly = TRUE, width = 280, height = 500) %>%
        hot_cols(rowHeaderWidth = 100) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    dataf
  })

  # fichier des mercuriales
  filemercu <- reactive({
    mercufile <- input$mercufile
    if (is.null(mercufile)) return(NULL)
    mercu <<- readr::read_tsv(
      mercufile$datapath,
      locale = readr::locale(encoding = "UTF-8", decimal_mark = ","),
      readr::cols(cdiam = readr::col_integer(), tarif = readr::col_character(), houppier = readr::col_integer(), hauteur = readr::col_double()),
      col_names = T
    )
    readr::write_tsv(mercu, mname)
    print(paste("filemercu:", mname))
    output$mercuhot <<- renderRHandsontable({
      rhandsontable(mercu, width = 240, height = 500, rowHeaders = NULL) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    mercu
  })
  
  # fichier des categories de diametre
  filecat <- reactive({
      cat <- data.frame(ess = rep(c("Feu", "Res"), times=1, each=6), 
                         cat = rep(c("Sem", "Per","PB","BM","GB","TGB"), times=2), 
                         dmin = c(0,10,20,30,50,70,0,10,20,30,45,65), 
                         dmax = c(5,15,25,45,65,200,5,15,25,40,60,200)
      )
      readr::write_tsv(cat, tname)
      print(paste("filecat:", tname))
      output$cathot <<- renderRHandsontable({
        items1 <- c("Feu", "Res")
        items2 <- c("Sem", "Per","PB","BM","GB","TGB")
        dmin <- seq(from = 0, by = 5, length.out = 41)
        dmax <- seq(from = 0, by = 5, length.out = 41)
        rhandsontable(cat, rowHeaders = NULL, height = 200, width = 300) %>%
          hot_cols(colWidths = 70) %>%
          hot_col(col = "ess", type = "dropdown", source = items1) %>%
          hot_col(col = "cat", type = "dropdown", source = items2) %>%
          hot_col(col = "dmin", type = "dropdown", source = dmin) %>%
          hot_col(col = "dmax", type = "dropdown", source = dmax) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })
  })

  # fichier des clauses
  fileclause <- reactive({
    clausefile <- input$clausefile
    if (is.null(clausefile)) {
      clause <<- data.frame(ess = "Defaut", dmin = 10, dmax = 200, dec = 7)
      readr::write_tsv(clause, cname)
      print(paste("fileclause:", cname))
      output$clausehot <<- renderRHandsontable({
        items <- listessence
        dmin <- seq(from = 10, by = 5, length.out = 39)
        dmax <- seq(from = 10, by = 5, length.out = 39)
        rhandsontable(clause, rowHeaders = NULL, height = 300, width = 300) %>%
          hot_cols(colWidths = 70) %>%
          hot_col(col = "ess", type = "dropdown", source = items) %>%
          hot_col(col = "dmin", type = "dropdown", source = dmin) %>%
          hot_col(col = "dmax", type = "dropdown", source = dmax) %>%
          hot_cell(1, "ess", readOnly = TRUE) %>%
          hot_cell(1, "dmin", readOnly = TRUE) %>%
          hot_cell(1, "dmax", readOnly = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })
    } else {
      clause <<- readr::read_tsv(
        clausefile$datapath,
        locale = readr::locale(encoding = "UTF-8", decimal_mark = ","),
        readr::cols(ess = readr::col_character(), dmin = readr::col_integer(), dmax = readr::col_integer(), dec = readr::col_double()),
        col_names = T
      )
      readr::write_tsv(clause, cname)
      print(paste("fileclause:", cname))
      output$clausehot <<- renderRHandsontable({
        items <- listessence
        dmin <- seq(from = 10, by = 5, length.out = 39)
        dmax <- seq(from = 10, by = 5, length.out = 39)
        rhandsontable(clause, height = 300, width = 300, rowHeaders = NULL) %>%
          hot_cols(colWidths = 70) %>%
          hot_col(col = "ess", type = "dropdown", source = items) %>%
          hot_col(col = "dmin", type = "dropdown", source = dmin) %>%
          hot_col(col = "dmax", type = "dropdown", source = dmax) %>%
          hot_cell(1, "ess", readOnly = TRUE) %>%
          hot_cell(1, "dmin", readOnly = TRUE) %>%
          hot_cell(1, "dmax", readOnly = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })
    }
    clause
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
    updateTabsetPanel(session, "inTabset02", selected = "Graphe")
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
      withProgress(message = "Calculs en cours", style = "notification", value = 0.75, {
        Sys.sleep(0.25)
        withCallingHandlers({
          shinyjs::html(id = "text02", html = "Go ! ")
          p <- gftools::TarifFindSch(
            fichier = fname,
            mercuriale = mname,
            clause = cname,
            enreg = F,
            classearbremin = input$ClasseInf[1],
            mappoint = input$mappoint,
            classearbremax = input$ClasseInf[2],
            essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)],
            latitude = input$latitude,
            longitude = input$longitude,
            zonecalc = zonecalcul(),
            houppier = mhouppier,
            echantillon = echant
          )
          if (!input$mappoint) {
            echant <<- p$Tableau5
            echanttab3 <<- p$Tableau3
            echanttab4 <<- p$Tableau4
            echantg <<- p$Graphe6
          }
        },
        message = function(m) {
          shinyjs::html(id = "text02", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text02", html = m$message, add = TRUE)
        }
        )
      })
      incProgress(1)
    })
    mhouppier <<- "N"
    p
  })

  output$plotcsv1 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (1/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe1
    })
  })

  output$plotcsv2 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (2/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe2
    })
  })

  output$plotcsv3 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (3/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe3
    })
  })

  output$plotcsv4 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (4/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe4
    })
  })

  output$plotcsv5 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (5/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe5
    })
  })

  output$plotcsv6 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (6/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe6
    })
  })

  output$plotcsv7 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (7/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe7
    })
  })

  output$plotcsv8 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (8/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe8
    })
  })

  output$plotcsv9 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (9/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe9
    })
  })

  output$plotcsv10 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (10/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe10
    })
  })

  output$plotcsv11 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (11/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe11
    })
  })

  output$plotcsv12 <- renderPlot({
    input$update02
    if (input$update02 == 0) return(NULL)
    isolate({
      withProgress(message = "Création du graphique (12/12)", style = "notification", value = 0.5, {
        Sys.sleep(0.25)
        p <- plotdata()
        incProgress(1)
      })
      p$Graphe12
    })
  })

  plotgraphe <- eventReactive(c(input$espar, input$tarif), {
    if (!is.null(input$tarif) & input$tarif != "" & input$espar != "Toutes" & !is.null(input$espar) & !(is.null(plotdata()$Tableau4))) {
      no1 <- plotdata()$Tableau4$Type[4 - as.integer(input$tarif)]
      mt1 <- plotdata()$Tableau4 %>%
        filter(essence == input$espar & Type == no1)
      mt1$Fill <- ifelse(input$mappoint, toupper(input$zonecalc), "ECH")
      dt1 <- plotdata()$Tableau3 %>%
        filter(essence == input$espar & Type == no1)
      dt1$Fill <- ifelse(input$mappoint, toupper(input$zonecalc), "ECH")
      
      if (input$mappoint) {
        no2 <- echanttab4$Type[4 - as.integer(input$tarif)]
        mt2 <- echanttab4 %>%
          filter(essence == input$espar & Type == no2)
        mt2$Fill <- "ECH"
        dt2 <- echanttab3 %>%
          filter(essence == input$espar & Type == no2)
        dt2$Fill <- "ECH"
        dt <- rbind(dt1, dt2)
        mt <- rbind(mt1, mt2)
      } else {
        dt <- dt1
        mt <- mt1
      }
      
      isolate({
        withProgress(message = "Graphique en cours", style = "notification", value = 0.75, {
          Sys.sleep(0.25)
          withCallingHandlers({
            shinyjs::html(id = "text02", html = "Go ! ")
            h <- ggplot(dt, aes(x = diam, y = Num, fill = Fill, order = Fill)) + 
              geom_point(alpha = 0.1) +
              facet_grid(essence ~ Type) +
              geom_boxplot(outlier.colour = "green", outlier.size = 2, notch = TRUE, alpha = 0.1) +
              geom_label(data = mt, aes(x = DMax + 15, label = round(Num, 0), y = Num), col = "red") +
              geom_label(data = mt, aes(x = DMax + 15, label = round(m_plus_sd, 0), y = m_plus_sd, fill = Fill), alpha = 0.1) +
              geom_label(data = mt, aes(x = DMax + 15, label = round(m_moins_sd, 0), y = m_moins_sd, fill = Fill), alpha = 0.1) +
              geom_errorbar(data = mt, mapping = aes(x = DMax + 10, ymin = m_moins_sd, ymax = m_plus_sd, fill = Fill), size = 0.5, width = 2) +
              geom_point(data = mt, aes(x = DMax + 10), color = "red") +
              guides(fill=guide_legend(title=" ")) +
              theme(legend.position = "bottom")
          },
          message = function(m) {
            shinyjs::html(id = "text02", html = m$message, add = TRUE)
          },
          warning = function(m) {
            shinyjs::html(id = "text02", html = m$message, add = TRUE)
          }
          )
        })
        incProgress(1)
      })
      h
    }
  })

  output$plotcsv13 <- renderPlot({
    if (!is.null(input$tarif) & input$tarif != "" & input$espar != "Toutes" & !is.null(input$espar) & !(is.null(plotdata()$Tableau4))) {
      plotgraphe()
    }
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

  outAGC <- reactive({
    agc <- agencedata %>%
      filter(grepl(input$dt, iidtn_agc))
    agc$iidtn_agc
  })

  outFRT <- reactive({
    frt <- forestdata %>%
      filter(ccod_cact == input$agence)
    frt$ccod_frt
  })

  outPRF <- reactive({
    prf <- parcelledata %>%
      filter(ccod_frt == input$forest, ccod_cact == input$agence)
    prf$ccod_prf
  })

  outPST <- reactive({
    pst <- pstdata %>%
      filter(grepl(input$agence, ccod_cact))
    pst$ccod_ut
  })
  
  outUT <- reactive({
    if (input$forest == " " || input$parcelle == " ") return("Toutes")
    ut <- parcelledata %>%
      filter(ccod_frt == input$forest, ccod_cact == input$agence, ccod_prf == input$parcelle)
    ut$ccod_pst
  })
  
  output$text05 <- renderPrint({
    return(paste("Poste de l'échantillon :", outUT()))
  })

  output$pst <- renderUI({
    selectizeInput("pst", NULL,
      choices = outPST(), multiple = TRUE,
      options = list(placeholder = "Choisir PST")
    )
  })

  outDATA <- reactive({
    ssample <- files %>%
      filter(dt == input$dt, agence == input$agence, forest == input$forest, parcelle == input$parcelle)
    if (length(ssample$id) > 0) {
      data <- filed %>%
        filter(sample == ssample$id)
      return(data$name)
    } else {
      return(NULL)
    }
  })

  outMERCU <- reactive({
    mfile <- filed %>%
      filter(grepl(input$listedata, name))
    if (length(mfile$id) > 0) {
      mercu <- filem %>%
        filter(id %in% mfile$id)
      if (length(mercu$id) > 0) {
        return(mercu$name)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })

  outCCT <- reactive({
    cfile <- agencedata %>%
      filter(grepl(input$agence, iidtn_agc))
    if (length(cfile$id) > 0) {
      cct <- filec %>%
        filter(agence %in% cfile$id)
      if (length(cct$id) > 0) {
        return(cct$name)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })

  observeEvent(input$listedata, {
    if (!is.null(input$listemercu)) {
      # recherche l identifiant du sample
      dt <- dtdata %>% filter(iidtn_dt == input$dt)
      frt <- forestdata %>% filter(ccod_frt == input$forest)
      agc <- agencedata %>% filter(iidtn_agc == input$agence)
      prf <- parcelledata %>% filter(ccod_cact == input$agence, ccod_frt == input$forest, ccod_prf == input$parcelle)
      query <- sprintf(
        "SELECT id FROM sample WHERE dt=%s AND agence=%s AND forest=%s AND parcelle=%s",
        dt$id, agc$id, frt$id, prf$id
      )
      sid <- loadData(query = query)
      if (!is.null(sid)) {
        # recherche l identifiant du datafile
        query <- sprintf(
          "SELECT id FROM filedata WHERE sample=%s AND name='%s'",
          sid[[1]], input$listedata
        )
        did <- loadData(query = query)
        # recherche les data du datafile si existe
        if (!is.null(did)) {
          if (nrow(did) > 0) {
            query <- sprintf(
              "SELECT code, diam, htot, hdec FROM datadata WHERE filedata=%s",
              did[[1]]
            )
            ddata <- loadData(query = query)
            if (!is.null(ddata)) {
              updateTextInput(session, "nomData", value = ddata)
              output$datahot <<- renderRHandsontable({
                rhandsontable(ddata, readOnly = TRUE, width = 280, height = 500) %>%
                  hot_cols(colWidths = 53) %>%
                  hot_table(highlightCol = TRUE, highlightRow = TRUE)
              })
            }
          } else {
            return(NULL)
          }
        }
      }
      updateSelectInput(session, "listemercu", choices = c("Choisir MERCU" = "", outMERCU()))
    } else {
      return(NULL)
    }
  })

  observeEvent(input$listemercu, {
    if (!is.null(input$listemercu)) {
      # recherche l identifiant du sample
      dt <- dtdata %>% filter(iidtn_dt == input$dt)
      frt <- forestdata %>% filter(ccod_frt == input$forest)
      agc <- agencedata %>% filter(iidtn_agc == input$agence)
      prf <- parcelledata %>% filter(ccod_cact == input$agence, ccod_frt == input$forest, ccod_prf == input$parcelle)
      query <- sprintf(
        "SELECT id FROM sample WHERE dt=%s AND agence=%s AND forest=%s AND parcelle=%s",
        dt$id, agc$id, frt$id, prf$id
      )
      sid <- loadData(query = query)
      if (!is.null(sid)) {
        # recherche l identifiant du datafile
        query <- sprintf(
          "SELECT id FROM filedata WHERE sample=%s AND name='%s'",
          sid[[1]], input$listedata
        )
        did <- loadData(query = query)
        if (!is.null(did)) {
          # recherche l identifiant du filemercuriale selectionne
          query <- sprintf(
            "SELECT id FROM filemercuriale WHERE name='%s' AND filedata=%s",
            input$listemercu, did[[1]]
          )
          fid <- loadData(query = query)
          # recherche les data du filemercuriale si existe
          if (!is.null(fid)) {
            if (nrow(fid) > 0) {
              query <- sprintf(
                "SELECT cdiam, tarif, houppier, hauteur FROM datamercuriale WHERE filemercuriale=%s",
                fid[[1]]
              )
              mercu <- loadData(query = query)
              if (!is.null(input$listemercu)) {
                updateTextInput(session, "nomMercu", value = as.character(input$listemercu))
                output$mercuhot <<- renderRHandsontable({
                  rhandsontable(mercu, width = 240, height = 500, rowHeaders = NULL) %>%
                    hot_table(highlightCol = TRUE, highlightRow = TRUE)
                })
              }
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      }
    }
  })

  observeEvent(input$listeclause, {
    if (!is.null(input$listeclause)) {
      # recherche l identifiant de la liste
      dt <- dtdata %>% filter(iidtn_dt == input$dt)
      agc <- agencedata %>% filter(iidtn_agc == input$agence)
      query <- sprintf(
        "SELECT id FROM cahierclausedt WHERE dt=%s AND agence=%s",
        dt$id, agc$id
      )
      cid <- loadData(query = query)
      if (!is.null(cid)) {
        # recherche les data du cahierclausedt si existe
        if (nrow(cid) > 0) {
          query <- sprintf(
            "SELECT essence as ess, dmin, dmax, decoupe AS dec FROM itemcahierclausedt WHERE cahierclausedt=%s",
            cid
          )
          cct <- loadData(query = query)
          if (!is.null(cct)) {
            readr::write_tsv(cct, cname)
            print(paste("fileclause:", cname))
            output$clausehot <<- renderRHandsontable({
              items <- listessence
              dmin <- seq(from = 10, by = 5, length.out = 39)
              dmax <- seq(from = 10, by = 5, length.out = 39)
              rhandsontable(cct, rowHeaders = NULL, height = 300, width = 300) %>%
                hot_cols(colWidths = 70) %>%
                hot_col(col = "ess", type = "dropdown", source = items) %>%
                hot_col(col = "dmin", type = "dropdown", source = dmin) %>%
                hot_col(col = "dmax", type = "dropdown", source = dmax) %>%
                hot_cell(1, "ess", readOnly = TRUE) %>%
                hot_cell(1, "dmin", readOnly = TRUE) %>%
                hot_cell(1, "dmax", readOnly = TRUE) %>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE)
            })
          }
        } else {
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  })
  
  ## Rapport fiche
  output$reportfiche <- downloadHandler(
    filename = function() {
      outNameFicheReport()
    },
    content = function(file) {
      progress <- Progress$new(session, min = 1, max = 9)
      on.exit(progress$close())
      progress$set(
        message = "Création du rapport fiche",
        detail = "Ça prend du temps...", value = 1
      )
      # creer le rapport dans un repertoire temporaire avant de le télécharger
      tempReport <- file.path(tempdir(), "fiche.Rmd")
      styleReport <- file.path(tempdir(), "mystyles.docx")
      file.copy("fiche.Rmd", tempReport, overwrite = TRUE)
      file.copy("mystyles.docx", styleReport, overwrite = TRUE)
      progress$set(value = 2)
      # on cree la carte ici pour éviter les messages dans le docx
      frt <- polyfrt()
      prf <- polyprf()
      poi <- st_point(x = c(input$longitude, input$latitude), dim = "XY") %>% st_sfc(crs = 4326)

      cpoi <- poi %>%
        st_centroid() %>%
        st_coordinates()
      poib <- poi %>%
        st_transform(2154) %>%
        st_buffer(dist = 10) %>%
        st_transform(4326) %>%
        fortify()

      if (input$forest == " ") {
        frt1 <- poib
        frt <- frt1[[1]] %>%
          fortify()
        nomfrt <- "POI"
      } else {
        nomfrt <- frt$llib2_frt
      }

      if (input$parcelle == " ") {
        prf1 <- poib
        prf <- prf1[[1]] %>%
          fortify()
        nomprf <- "POI"
      } else {
        nomprf <- prf$ccod_prf
      }

      cfrt <- frt %>%
        st_centroid() %>%
        st_coordinates()
      cprf <- prf %>%
        st_centroid() %>%
        st_coordinates()

      if (nomfrt == "POI" | nomprf == "POI") {
        map01 <- ggmap(get_googlemap(
          center = c(lon = cpoi[1], lat = cpoi[2]),
          zoom = 15, scale = 2, size = c(640, 640), maptype ='satellite', messaging = FALSE), language = "fr-FR", extent="device") +
          geom_sf(data = frt, aes(alpha=0.9), color = "green", inherit.aes = FALSE) +
          geom_sf(data = prf, aes(alpha=0.9), color = "red", inherit.aes = FALSE) +
          scale_alpha_continuous(guide = F) +
          theme(axis.text = element_blank())
        map02 <- ggmap(get_googlemap(
          center = c(lon = cpoi[1], lat = cfrt[2]),
          zoom = 14, scale = 2, size = c(640, 640), maptype ='terrain', messaging = FALSE), language = "fr-FR", extent="device") +
          scale_alpha_continuous(guide = F)  +
          theme(axis.text = element_blank()) 
      } else {
        map01 <- ggmap(get_googlemap(
          center = c(lon = cfrt[1], lat = cfrt[2]),
          zoom = 15, scale = 2, size = c(640, 640), maptype ='satellite', messaging = FALSE), language = "fr-FR", extent="device") +
          geom_sf(data = frt, aes(alpha=0.9), color = "green", inherit.aes = FALSE) +
          geom_sf(data = prf, aes(alpha=0.9), color = "red", inherit.aes = FALSE) +
          scale_alpha_continuous(guide = F) +
          geom_point(aes(x = cfrt[1], y = cfrt[2]), stroke = 2, colour="green", data = frt, size =1) +
          geom_label_repel(aes(x= cfrt[1], y = cfrt[2], label = frt$ccod_frt), data = frt, family = 'Times', 
                           size = 4, box.padding = 0.2, point.padding = 0.3, segment.color = 'green') +
          geom_point(aes(x = cprf[1], y = cprf[2]), stroke = 2, colour="red", data = prf, size =1) +
          geom_label_repel(aes(x = cprf[1], y = cprf[2], label = prf$ccod_prf), data = prf, family = 'Times',
                           size = 4, box.padding = 0.2, point.padding = 0.3, segment.color = 'red') +
          theme(axis.text = element_blank()) 
        map02 <- ggmap(get_googlemap(
          center = c(lon = cfrt[1], lat = cfrt[2]),
          zoom = 14, scale = 2, size = c(640, 640), maptype ='terrain', messaging = FALSE), language = "fr-FR", extent="device") +
          geom_sf(data = frt, aes(alpha=0.9), color = "green", inherit.aes = FALSE) +
          geom_sf(data = prf, aes(alpha=0.9), color = "red", inherit.aes = FALSE) +
          scale_alpha_continuous(guide = F)  +
          theme(axis.text = element_blank()) 
      }
      
      progress$set(value = 3)
      if (!is.null(input$reshot)) {
        DF <- hot_to_r(input$reshot) %>%
          rename(cdiam = Classe) %>%
          mutate(
            tarif = paste0(names(listetarif)[as.integer(input$tarif)], formatC(as.integer(input$numtarif), width = 2, flag = "0")),
            hauteur = 0,
            houppier = round(100 * E_PHouppiers / (100 + E_PHouppiers), 0)
          )
      } else if (!is.null(outRESHOT())) {
        DF <- outRESHOT() %>%
          rename(cdiam = Classe) %>%
          mutate(
            tarif = paste0(names(listetarif)[as.integer(input$tarif)], formatC(as.integer(input$numtarif), width = 2, flag = "0")),
            hauteur = 0,
            houppier = round(100 * E_PHouppiers / (100 + E_PHouppiers), 0)
          )
      }
      mercutop <- DF[, c("cdiam", "tarif", "houppier")]
      tab <- tablocalmercu()$Tableau1 %>%
        filter(essence %in% input$Essences03 & ut %in% input$ut)
      progress$set(value = 4)
      # knit le document en passant les parametres params
      isolate({
        withCallingHandlers({
          shinyjs::html(id = "text04", html = "Go ! Soyez très patient...")
          # on prend les parametres en compte pour le rapport markdown
          params <- list(
            agence = input$agence,
            exercice = input$exercice,
            frt = nomfrt,
            prf = nomprf,
            echantillon = toupper(substr(input$datafile$name, 1, nchar(input$datafile$name) - 4)),
            echant = echanttab3,
            data = plotdata()$Tableau3,
            espar = input$espar,
            tarif = input$tarif,
            graphe = plotgraphe(),
            texte01 = Samnbtig(),
            texte02 = Tartypvol(),
            texte03 = echanttab1,
            texte04 = gftools::describeBy(tab, group = list(tab$essence, tab$ut)),
            zonecalc = toupper(input$zonecalc),
            mercutop = mercutop,
            map01 = map01,
            map02 = map02,
            graphe1 = echantg,
            graphe2 = g2
          )
          progress$set(value = 7)
          # suppress warnings
          storeWarn <- getOption("warn")
          options(warn = -1)
          rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
          options(warn = storeWarn)
          progress$set(value = 8)
        },
        message = function(m) {
          shinyjs::html(id = "text04", html = m$message, add = TRUE)
        }
        )
      })
    }
  )
  
  ## Nom du rapport fiche
  outNameFicheReport <- reactive({
    filename <- paste0("rapport_fiche_", input$agence, "_", toupper(substr(input$datafile$name, 1, nchar(input$datafile$name) - 4)),"_", input$espar, "_au_", format(Sys.Date(), "%Y%m%d"), ".docx")
    return(filename)
  })

  ## Nom du rapport agence
  outNameAgenceReport <- reactive({
    filename <- paste0("rapport_agence_", input$agence, "_", toupper(input$zonecalc), "_au_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    return(filename)
  })

  ## Rapport agence
  output$reportagence <- downloadHandler(
    filename = function() {
      outNameAgenceReport()
    },
    content = function(file) {
      # creer le rapport dans un repertoire temporaire avant de le télécharger
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      CodesEssIFN <- gftools::getData("IFNCODE") %>%
        dplyr::filter(donnee %in% "ESPAR")

      # on recherche les SER, RN250 ou RF250 intersectant l agence
      shapefileDF <- BDDQueryONF(query = paste0(
        "SELECT s.id, a.iidtn_agc AS agence, ",
        ifelse(input$zonecalc == "ser", "s.code", ifelse(input$zonecalc == "rn250", "s.regn", "s.regiond")), " AS code, ",
        ifelse(input$zonecalc == "ser", "s.name", ifelse(input$zonecalc == "rn250", "s.regionn", "s.regionn")), " AS reg, s.geom FROM ",
        input$zonecalc, " s, agence a WHERE st_intersects(a.geom,s.geom) AND a.iidtn_agc='", input$agence, "'"
      ))
      # on recherche les UTs de l agence
      poste <- BDDQueryONF(query = paste0("SELECT ccod_cact, ccod_ut, clib_pst, geom FROM pst WHERE ccod_ut LIKE '", input$agence, "%' ORDER BY ccod_ut"))
      # instancie la barre de proression
      maxi <- length(unique(shapefileDF["code"]$code)) * length(CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)]) + 5
      progress <- Progress$new(session, min = 1, max = maxi)
      on.exit(progress$close())
      progress$set(
        message = "Création du rapport agence",
        detail = "Ça prend du temps...", value = 1
      )

      # knit le document en passant les parametres params
      isolate({
        withCallingHandlers({
          shinyjs::html(id = "text04", html = "Go ! Soyez très patient...")
          # on calclule le besttarif
          resu <- BestTarifFindSch(
            mercuriale = mname,
            zonecalc = shapefileDF,
            clause = cname,
            agence = input$agence,
            exercice = input$exercice,
            classearbremin = input$ClasseInf[1],
            classearbremax = input$ClasseInf[2],
            essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)],
            barre = progress,
            typzonecalc = input$zonecalc,
            categorie = tname
          )
          # on prend les parametres en compte pour le rapport markdown
          params <- list(
            agence = input$agence,
            exercice = input$exercice,
            clause = cname,
            classearbremin = input$ClasseInf[1],
            classearbremax = input$ClasseInf[2],
            essence = CodesEssIFN$code[which(CodesEssIFN$libelle %in% input$Essences02)],
            shape = shapefileDF,
            pst = poste,
            barre = progress,
            res = resu,
            typzonecalc = input$zonecalc
          )
          progress$set(value = maxi - 3)
          # suppress warnings
          storeWarn <- getOption("warn")
          options(warn = -1)
          rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
          options(warn = storeWarn)
          progress$set(value = maxi - 2)
        },
        message = function(m) {
          shinyjs::html(id = "text04", html = m$message, add = TRUE)
        }
        )
      })
    }
  )

  polydt <- reactive({
    polydt <- dtdata %>%
      filter(iidtn_dt == input$dt)
    polydt
  })
  
  observeEvent(input$zonecalc, {
    leafletProxy("map0203")
  })

  observeEvent(input$dt, {
    updateSelectInput(session, "agence", choices = c(Choisir = "", outAGC()))
    dt <- polydt()
    if (!is.null(input$dt) && !is.na(st_bbox(dt)$xmin)) {
      dt <- st_transform(dt, 2154) %>%
        st_centroid() %>%
        st_transform(4326) %>%
        st_geometry()
      leafletProxy("map0201") %>%
        setView(lat = st_coordinates(dt)[2], lng = st_coordinates(dt)[1], zoom = 8)
    }
    leafletProxy("map0203")
  })

  polyagc <- reactive({
    polyagc <- agencedata %>%
      filter(iidtn_agc == input$agence)
    polyagc
  })

  observeEvent(input$agence, {
    updateSelectInput(session, "forest", choices = c(Choisir = "", outFRT()))
    updateSelectInput(session, "listeclause", choices = c("Choisir CCT" = "", outCCT()))
    updateSelectInput(session, "parcelle", choices = c(Choisir = " ", outPRF()))
    agc <- polyagc()
    if (!is.null(input$agence) && !is.na(st_bbox(agc)$xmin)) {
      agc <- st_transform(agc, 2154) %>%
        st_centroid() %>%
        st_transform(4326) %>%
        st_geometry()
      leafletProxy("map0201") %>%
        setView(lat = st_coordinates(agc)[2], lng = st_coordinates(agc)[1], zoom = 10)
    }
  })

  polyfrt <- reactive({
    if (input$forest != " ") {
      polyfrt <- forestdata %>%
        filter(ccod_frt == input$forest, ccod_cact == input$agence)
    } else {
      polyfrt <- forestdata %>%
        filter(id == 0)
    }
    polyfrt
  })

  observeEvent(input$forest, {
    updateSelectInput(session, "parcelle", choices = c(Choisir = " ", outPRF()))
    frt <- polyfrt()
    if (input$forest != " " && !is.na(st_bbox(frt)$xmin)) {
      frt <- st_transform(frt, 2154) %>%
        st_centroid() %>%
        st_transform(4326) %>%
        st_geometry()
      leafletProxy("map0201") %>%
        setView(lat = st_coordinates(frt)[2], lng = st_coordinates(frt)[1], zoom = 12)
    }
  })

  polyprf <- reactive({
    if (input$parcelle != " " && input$forest != " "){
      polyprf <- parcelledata %>%
        filter(ccod_prf == input$parcelle, ccod_frt == input$forest, ccod_cact == input$agence)
    } else {
      polyprf <- parcelledata %>%
        filter(id == 0)
    }
    polyprf
  })

  observeEvent(input$parcelle, {
    updateSelectInput(session, "listedata", choices = c("Choisir DATA" = "", outDATA()))
    prf <- polyprf()
    if (input$parcelle != " " && !is.na(st_bbox(prf)$xmin)) {
      prf <- st_transform(prf, 2154) %>%
        st_centroid() %>%
        st_transform(4326) %>%
        st_geometry()
      leafletProxy("map0201") %>%
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
      pst <- st_centroid(st_convex_hull(st_union(pst))) %>%
        st_geometry()
      leafletProxy("map0201") %>%
        setView(lat = st_coordinates(pst)[2], lng = st_coordinates(pst)[1], zoom = 10)
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

  output$map0201 <- renderLeaflet({
    agc <- polyagc()
    popupagc <- paste0("<strong>", agc$iidtn_agc, " - ", agc$llib_agc, "</strong>")
    frt <- polyfrt()
    popupfrt <- paste0("<strong>", frt$llib2_frt, "</strong>")
    prf <- polyprf()
    popupprf <- paste0("<strong>Parcelle : </strong>", prf$ccod_prf)
    leaflet(options = leafletOptions(doubleClickZoom = FALSE)) %>%
      setView(lat = 47.08, lng = 5.68, zoom = 6) %>%
      addTiles() %>%
      addPolygons(data = polyfrt(), weight = 2, color = "green", fillColor = "green", popup = popupfrt, group = "Forêt") %>%
      addPolygons(data = polyprf(), weight = 2, color = "red", fillColor = "red", popup = popupprf, group = "Parcelle") %>%
      addLayersControl(overlayGroups = c("Forêt", "Parcelle"), options = layersControlOptions(collapsed = TRUE))
  })

  pointclick <- reactiveValues(clickedMarker = NULL)

  output$map0202 <- renderLeaflet({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(pointclick$clickedMarker$lat)) return(NULL)
    p <- plotdata()
    if (!is.null(p$Zone) | !is.null(p$Placettes)) {
      zonemap <- sf::st_transform(p$Zone, crs = 4326)
      placettesmap <- sf::st_transform(p$Placettes, crs = 4326)
    } else {
      return(NULL)
    }
    popupzone <- paste0("<strong>", p$Zone$couche, " (id/nom) : </strong>", p$Zone$id, "/", p$Zone$name)
    popupplacettes <- paste0("<strong>Année : </strong>", p$Placettes$yrs, " - idp : ", p$Placettes$idp)
    leaflet() %>%
      setView(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, zoom = input$map0201_zoom - 3) %>%
      addTiles() %>%
      addPolygons(data = zonemap, weight = 2, fillColor = "yellow", popup = popupzone, group = "Zone de calcul") %>%
      addCircles(data = placettesmap, popup = popupplacettes, weight = 3, radius = 60, color = "#ff00e6", fill = TRUE, stroke = TRUE, fillOpacity = 0.1, group = "Placette IFN") %>%
      addMarkers(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, popup = "Localisation du calcul !") %>%
      addLayersControl(overlayGroups = c("Zone de calcul", "Placette IFN"), options = layersControlOptions(collapsed = TRUE))
  })
  
  output$map0203 <- renderLeaflet({
    if (!input$mappoint | input$agence == '') return(NULL)
    if (is.null(pointclick$clickedMarker$lat)) return(NULL)
    shapefileDF <- BDDQueryONF(query = paste0(
      "SELECT s.id, a.iidtn_agc AS agence, ",
      ifelse(input$zonecalc == "ser", "s.code", ifelse(input$zonecalc == "rn250", "s.regn", "s.regiond")), " AS code, ",
      ifelse(input$zonecalc == "ser", "s.name", ifelse(input$zonecalc == "rn250", "s.regionn", "s.regionn")), " AS reg, s.geom FROM ",
      input$zonecalc, " s, agence a WHERE st_intersects(a.geom,s.geom) AND a.iidtn_agc='", input$agence, "'"
    ))
    if (is.null(shapefileDF$id)) {
      return(NULL)
    } else {
      box <- st_bbox(shapefileDF)
      factpal <- colorFactor(rep(unique(yarrr::piratepal("basel")),
                                 length.out = nrow(shapefileDF)),
                             shapefileDF$code)
      popupzone <- paste0("<strong>", shapefileDF$code, " - </strong>", shapefileDF$reg)
      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                 group = "OpenTopoMap"
        ) %>%
        # addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
                 group = "CartoDB"
        ) %>%
        # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                 group = "Satellite"
        ) %>%
        # view and controls
        addPolygons(data = shapefileDF, weight = 2, fillColor = ~factpal(shapefileDF$code), popup = popupzone, group = "Zone") %>%
        addLayersControl(overlayGroups = c("Zone"), 
                         baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        addMarkers(lat = pointclick$clickedMarker$lat, lng = pointclick$clickedMarker$lng, popup = "Localisation du calcul !") %>%
        fitBounds(lng1 = box[[1]], lat1 = box[[2]], lng2 = box[[3]], lat2 = box[[4]])
    }
  })

  zonecalcul <- eventReactive({
    input$update02
  }, {
    txt <- paste0("'", input$zonecalc, "' as couche, ")
    if (input$zonecalc == "ser") {
      txt <- paste0(txt, " name, id,")
    } else if (input$zonecalc == "rn250") {
      txt <- paste0(txt, " regionn as name, id,")
    } else if (input$zonecalc == "rf250") {
      txt <- paste0(txt, " regiond as name, id,")
    } else if (input$zonecalc == "pst") {
      txtpst <- paste0(
        "WITH w2 AS (WITH w1 AS (SELECT clib_pst AS name, ccod_ut AS id, geom FROM pst WHERE ccod_ut::integer IN (", paste(input$pst, collapse = ","),
        ")) SELECT name, id, st_union(geom) AS geom FROM w1 GROUP BY name, id)"
      )
    }
    if (input$zonecalc != "pst") {
      BDDQueryONF(query = paste0(
        "SELECT ", txt, " geom FROM ",
        input$zonecalc, " s WHERE st_dwithin(st_transform(st_setsrid(st_makepoint(",
        pointclick$clickedMarker$lng, ",",
        pointclick$clickedMarker$lat, "),4326),2154), s.geom,0)"
      )) %>%
        sf::st_transform(crs = 2154)
    } else {
      BDDQueryONF(query = paste0(txtpst, " SELECT ", txt, "string_agg(name, ',') AS name, string_agg(id, ',') AS id, st_union(geom) AS geom FROM w2")) %>%
        sf::st_transform(crs = 2154)
    }
  })

  # Resvol
  output$Resvol <- renderText({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(filemercu())) return(NULL)
    if (is.null(filedata())) return(NULL)
    withProgress(message = "Graphique des résultats", style = "notification", value = 0.5, {
      Sys.sleep(0.25)
      resv <- gftools::describeBy(tabdata(), group = tabdata()$essence)
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

  # tableau des donnees calculees
  tabdata <- reactive({
    input$update02
    if (input$update02 == 0) return(NULL)
    plotdata()$Tableau1
  })

  # Generate a summary of the result
  output$summarycsv <- renderPrint({
    input$update02
    if (input$update02 == 0) return(NULL)
    if (is.null(filemercu())) return(NULL)
    if (is.null(filedata())) return(NULL)
    withProgress(message = "Tableau des résultats", style = "notification", value = 0.5, {
      Sys.sleep(0.25)
      resv <- gftools::describeBy(tabdata(), group = tabdata()$essence)
      incProgress(1)
    })
    if (!input$mappoint) {
      echanttab1 <<- resv
    }
    resv
  })

  output$show_vars <- renderUI({
    data <- tabdata()
    checkboxGroupInput("show_vars", "Colonnes à afficher :",
      names(data),
      selected = names(data)
    )
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

  # filedata --- fichier des donnees -----
  observeEvent(input$saveBtnDataBDD, {
    # enregistre ou met à jour sample
    dt <- dtdata %>% filter(iidtn_dt == input$dt)
    frt <- forestdata %>% filter(ccod_frt == input$forest)
    agc <- agencedata %>% filter(iidtn_agc == input$agence)
    prf <- parcelledata %>% filter(ccod_cact == input$agence, ccod_frt == input$forest, ccod_prf == input$parcelle)
    query <- sprintf(
      "INSERT INTO sample (create_date, create_uid, dt, agence, forest, parcelle) SELECT now(), 1, %s, %s, %s, %s 
      WHERE NOT EXISTS (SELECT 1 FROM sample WHERE dt=%s AND agence=%s AND forest=%s AND parcelle=%s)",
      dt$id, agc$id, frt$id, prf$id,
      dt$id, agc$id, frt$id, prf$id
    )
    saveData(query = query)
    # recherche l identifiant du sample
    query <- sprintf(
      "SELECT id FROM sample WHERE dt=%s AND agence=%s AND forest=%s AND parcelle=%s",
      dt$id, agc$id, frt$id, prf$id
    )
    sid <- loadData(query = query)
    # enregistre ou met a jour filedata
    query <- sprintf(
      "INSERT INTO filedata (create_date, create_uid, name, sample) SELECT now(), 1, '%s', %s
      WHERE NOT EXISTS (SELECT 1 FROM filedata WHERE name='%s' AND sample=%s)",
      input$nomData, sid[[1]],
      input$nomData, sid[[1]]
    )
    saveData(query = query)
    # recherche l identifiant du filedata juste cree
    query <- sprintf(
      "SELECT id FROM filedata WHERE name='%s' AND sample=%s",
      input$nomData, sid[[1]]
    )
    fid <- loadData(query = query)
    # enregistre ou met a jour les data
    DF <- as.data.frame(filedata()) %>%
      mutate(create_date = toString(as.POSIXlt(Sys.time(), "Europe/Paris")), create_uid = 1, filedata = fid[[1]])
    # efface les data existantes
    query <- sprintf(
      "DELETE FROM datadata WHERE filedata=%s",
      fid[[1]]
    )
    delData(query = query)
    # insert les nouvelles data
    insertData("datadata", DF)
    # on met a jour la liste deroulante data csv
    query <- sprintf(
      "SELECT * FROM filedata WHERE sample=%s",
      sid[[1]]
    )
    filed <<- loadData(query = query)
    updateSelectInput(session, "listedata", choices = c("Choisir DATA" = "", filed$name))
  })

  # save mercuriale
  output$saveBtnMercu <- downloadHandler(
    # Nom par défaut :
    filename = function() {
      paste0("Mercu_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(input$mercuhot)) {
        write.csv(as.data.frame(hot_to_r(input$mercuhot)), file, row.names = FALSE)
      } else {
        write.csv(filemercu(), file, row.names = FALSE)
      }
    }
  )

  # filemercuriale --- fichier mecuriale -------
  observeEvent(input$saveBtnMercuBDD, {
    # recherche l identifiant du sample
    dt <- dtdata %>% filter(iidtn_dt == input$dt)
    frt <- forestdata %>% filter(ccod_frt == input$forest)
    agc <- agencedata %>% filter(iidtn_agc == input$agence)
    prf <- parcelledata %>% filter(ccod_cact == input$agence, ccod_frt == input$forest, ccod_prf == input$parcelle)
    query <- sprintf(
      "SELECT id FROM sample WHERE dt=%s AND agence=%s AND forest=%s AND parcelle=%s",
      dt$id, agc$id, frt$id, prf$id
    )
    sid <- loadData(query = query)
    # recherche l identifiant du datafile
    query <- sprintf(
      "SELECT id FROM filedata WHERE sample=%s AND name='%s'",
      sid[[1]], input$listedata
    )
    did <- loadData(query = query)
    # enregistre ou met a jour filemercuriale
    query <- sprintf(
      "INSERT INTO filemercuriale (create_date, create_uid, name, filedata) SELECT now(), 1, '%s', %s
      WHERE NOT EXISTS (SELECT 1 FROM filemercuriale WHERE name='%s' AND filedata=%s)",
      input$nomMercu, did[[1]],
      input$nomMercu, did[[1]]
    )
    saveData(query = query)
    # recherche l identifiant du filemercuriale juste cree
    query <- sprintf(
      "SELECT id FROM filemercuriale WHERE name='%s' AND filedata=%s",
      input$nomMercu, did[[1]]
    )
    fid <- loadData(query = query)
    # enregistre ou met a jour les data
    if (!is.null(input$mercuhot)) {
      DF <- hot_to_r(input$mercuhot)
    } else {
      DF <- mercu
    }
    DF <- as.data.frame(DF) %>%
      mutate(create_date = toString(as.POSIXlt(Sys.time(), "Europe/Paris")), create_uid = 1, filemercuriale = fid[[1]])
    # efface les data existantes
    query <- sprintf(
      "DELETE FROM datamercuriale WHERE filemercuriale=%s",
      fid[[1]]
    )
    delData(query = query)
    # insert les nouvelles data
    insertData("datamercuriale", DF)
    # on met a jour la liste deroulante mercu csv
    query <- sprintf(
      "SELECT * FROM filemercuriale WHERE filedata=%s",
      did[[1]]
    )
    filem <<- loadData(query = query)
    updateSelectInput(session, "listemercu", choices = c("Choisir MERCU" = "", filem$name))
  })
  
  # Catdiam
  Catdiam <- reactive({
    if (!is.null(input$cathot)) {
      as.data.frame(hot_to_r(input$cathot))
    } else if (!is.null(filecat())) {
      filecat()
    } else {
      return(NULL)
    }
  })

  # Clauseter
  Clauseter <- reactive({
    if (!is.null(input$clausehot)) {
      as.data.frame(hot_to_r(input$clausehot))
    } else if (!is.null(fileclause())) {
      filecat()
      fileclause()
    } else {
      return(NULL)
    }
  })

  # Affichage Clauseter
  output$Clauseter <- renderText({
    if (is.null(Clauseter())) return(NULL)
    Txt <- paste0(
      "Pour l'essence ", Clauseter()$ess, ", de la classe ", Clauseter()$dmin, " à la classe ", Clauseter()$dmax, ", la découpe fin bout est de ", Clauseter()$dec, " cm.\n"
    )
  })

  # Samnbtig
  Samnbtig <- reactive({
    if (!is.null(input$datahot)) {
      as.data.frame(hot_to_r(input$datahot))
    } else if (!is.null(filedata())) {
      filedata()
    } else {
      return(NULL)
    }
  })

  # Affichage Samnbtig
  output$Samnbtig <- renderText({
    if (is.null(Samnbtig())) return(NULL)
    nbtig <- nrow(Samnbtig())
    nbess <- unique(Samnbtig()$code)
    listess <- paste(nbess, collapse = ", ")
    Txt <- paste0(
      " L'échantillon contient ", nbtig, " tige(s) désignée(s) et ", length(nbess), " essence(s) : ", listess, "."
    )
  })

  # Tartypvol
  Tartypvol <- reactive({
    if (!is.null(input$mercuhot)) {
      as.data.frame(hot_to_r(input$mercuhot))
    } else if (!is.null(filemercu())) {
      filemercu()
    } else {
      return(NULL)
    }
  })

  # Affichage Tartypvol
  output$Tartypvol <- renderText({
    if (is.null(Tartypvol())) return(NULL)
    Tar <- unique(Tartypvol()$tarif)
    Tar <- Tar[!is.na(Tar)]
    Tab <- ListTarONF3[which(ListTarONF3$sibois %in% Tar), c("ess", "type_v", "contexte", "defvol", "dmin", "dmax", "hmin", "hmax", "entr1", "entr2", "sibois")]
    Txt <- paste0(
      Tab$sibois, " (", Tab$contexte, "), est un tarif pour les essences (", Tab$ess, "), pour les diamètres de ", Tab$dmin, " à ", Tab$dmax,
      " cm, pour les hauteurs de ", Tab$hmin, " à ", Tab$hmax, " m,\n nécessite ", Tab$entr1, " et ", Tab$entr2, " et renvoi un ", Tab$defvol, " (", Tab$type_v, ").\n"
    )
  })

  # essences presentes dans l'echantillon
  outESPAR <- reactive({
    input$update02
    if (input$update02 == 0) return(NULL)
    c("Toutes", pull(plotdata()$Tableau2, essence))
  })

  # affichage des essences
  observeEvent(input$update02, {
    updateSelectInput(session, "espar", selected = "Toutes", choices = c(Choisir = "", outESPAR()))
  })

  # tableau des mercuriales
  outRESHOT <- eventReactive(input$espar, {
    if (!is.null(plotdata())) {
      res1 <- plotdata()$Tableau1 %>%
        group_by(Classe, essence) %>%
        summarise_at(c("E_PHouppiers"), funs(mean)) %>%
        mutate_at(c("E_PHouppiers"), funs(as.integer(100 * round(., 2))))
      res2 <- plotdata()$Tableau1 %>%
        group_by(Classe) %>%
        summarise_at(c("E_PHouppiers"), funs(mean)) %>%
        mutate_at(c("E_PHouppiers"), funs(as.integer(100 * round(., 2)))) %>%
        mutate(essence = "Toutes")
      bind_rows(res1, res2) %>%
        filter(essence == input$espar)
    }
  })

  # affichage du fichier des mercuriales
  output$reshot <- renderRHandsontable({
    if (!is.null(outRESHOT())) {
      DF <- outRESHOT()
    } else {
      return(NULL)
    }
    rhandsontable(DF, width = 300, height = 200, rowHeaders = NULL) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  # numtarif
  observeEvent({
    input$tarif
    input$espar
  }, {
    if (!is.null(input$tarif) & !is.null(input$espar) & !(is.null(plotdata()$Tableau2))) {
      res1 <- plotdata()$Tableau2
      res2 <- res1 %>%
        summarise_at(c("SchR", "SchL", "Alg"), funs(mean)) %>%
        mutate(essence = "Toutes")
      res <- bind_rows(res2, res1)
      arrondi <- if (is.numeric(res[[which(res$essence == input$espar), as.integer(input$tarif)]])) {
        round(res[[which(res$essence == input$espar), as.integer(input$tarif)]], 0)
      } else {
        ""
      }
      updateSelectInput(session, "numtarif", selected = arrondi)
    }
  })

  # bouton transmercu
  observeEvent(input$transmercu, {
    if (!is.null(input$reshot)) {
      DF <- hot_to_r(input$reshot) %>%
        rename(cdiam = Classe) %>%
        mutate(
          tarif = paste0(names(listetarif)[as.integer(input$tarif)], formatC(as.integer(input$numtarif), width = 2, flag = "0")),
          hauteur = 0,
          houppier = round(100 * E_PHouppiers / (100 + E_PHouppiers), 0)
        )
    } else if (!is.null(outRESHOT())) {
      DF <- outRESHOT() %>%
        rename(cdiam = Classe) %>%
        mutate(
          tarif = paste0(names(listetarif)[as.integer(input$tarif)], formatC(as.integer(input$numtarif), width = 2, flag = "0")),
          hauteur = 0,
          houppier = round(100 * E_PHouppiers / (100 + E_PHouppiers), 0)
        )
    } else {
      return(NULL)
    }
    mercu <<- DF[, c("cdiam", "tarif", "houppier", "hauteur")]
    mhouppier <<- "O"
    output$mercuhot <<- renderRHandsontable({
      rhandsontable(mercu, width = 240, height = 500, rowHeaders = NULL) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    mercu
  })

  # tableau des donnees calculees
  tablocalmercu <- reactive({
    input$update022
    if (input$update022 == 0) return(NULL)
    withProgress(message = "Calcul en cours", style = "notification", value = 0.5, {
      Sys.sleep(0.25)
      query <- sprintf(
        "SELECT exercice, agence, ut, ess AS essence, diam, haut, nb, tahd AS l_phouppiers, tacomd, volcu AS l_vbftigcom, volcu*(tahd/100.0) AS l_vhouppiers, 
      volcu*(1+tahd/100.0) AS l_vbftot7cm FROM datacab WHERE agence='%s' AND exercice=%s AND diam>0 AND tacomd!='0'",
        input$agence, input$exercice
      )
      res <- loadData(query = query)
      if (!is.null(res)) {
        if (nrow(res) > 0) {
          mer <- readr::read_tsv(
            mname,
            locale = readr::locale(encoding = "UTF-8", decimal_mark = "."),
            readr::cols(cdiam = readr::col_integer(), tarif = readr::col_character(), houppier = readr::col_integer(), hauteur = readr::col_double()),
            col_names = T
          ) %>%
            filter(!is.na(tarif))
          res <- res %>%
            mutate(classe = floor(diam / 5 + 0.5) * 5) %>%
            inner_join(mer, by = c(classe = "cdiam"))
          tab <- res %>%
            mutate(
              e_vbftot7cm = as.numeric(TarifONF3v(tarif = tarif, entr1 = diam, entr2 = haut, details = FALSE)),
              e_vhouppiers = e_vbftot7cm * houppier / 100,
              e_vbftigcom = e_vbftot7cm - e_vhouppiers,
              e_phouppiers = houppier / 100
            )
          tab.r <- tab %>%
            mutate(tl_vbftigcom = l_vbftigcom * nb, tl_vhouppiers = l_vhouppiers * nb, te_vbftigcom = e_vbftigcom * nb, te_vhouppiers = e_vhouppiers * nb) %>%
            group_by(exercice, agence, essence, classe, ut) %>%
            summarise_at(c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers", "nb"), sum, na.rm = TRUE)
          
          tab.s <- tab %>%
            mutate(tl_vbftigcom = l_vbftigcom * nb, tl_vhouppiers = l_vhouppiers * nb, te_vbftigcom = e_vbftigcom * nb, te_vhouppiers = e_vhouppiers * nb) %>%
            group_by(exercice, agence, classe, ut) %>%
            summarise_at(c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers", "nb"), sum, na.rm = TRUE) %>%
            mutate(essence = "Toutes")
          tabi <- bind_rows(tab.r, tab.s)
          
          tab.u <- tab %>%
            mutate(tl_vbftigcom = l_vbftigcom * nb, tl_vhouppiers = l_vhouppiers * nb, te_vbftigcom = e_vbftigcom * nb, te_vhouppiers = e_vhouppiers * nb) %>%
            group_by(exercice, agence, classe, essence) %>%
            summarise_at(c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers", "nb"), sum, na.rm = TRUE) %>%
            mutate(ut = "Toutes")
          tabj <- bind_rows(tabi, tab.u)
          
          tab.v <- tab %>%
            mutate(tl_vbftigcom = l_vbftigcom * nb, tl_vhouppiers = l_vhouppiers * nb, te_vbftigcom = e_vbftigcom * nb, te_vhouppiers = e_vhouppiers * nb) %>%
            group_by(exercice, agence, classe) %>%
            summarise_at(c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers", "nb"), sum, na.rm = TRUE) %>%
            mutate(essence = "Toutes", ut = "Toutes")
          tabf <- bind_rows(tabj, tab.v)
          
          tab.t <- reshape2::melt(tabf, id.vars = c("exercice", "agence", "essence", "classe", "ut"), measure.vars = c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers")) %>%
            mutate(Type = substr(variable, 1, 2), variable = substr(variable, 4, 12))
          names(tab.t) <- c("exercice", "agence", "essence", "classe", "ut", "tarif", "vol", "type")
          tab.t$type[which(tab.t$type == "tl")] <- "LOCAL"
          tab.t$type[which(tab.t$type == "te")] <- "EMERCU"
          tab.t$tarif[which(tab.t$tarif == "vhouppier")] <- "VHouppiers"
          tab.t$tarif[which(tab.t$tarif == "vbftigcom")] <- "VbftigCom"
          out <- list(tabf, tab.t)
          names(out) <- c("Tableau1", "Tableau2")
          return(out)
        }
      }
      incProgress(1)
    })
  })

  # Affichage du graphe de comparaison des volumes agences
  output$plotdatacab <- renderPlot({
    if (!is.null(tablocalmercu()$Tableau2)) {
      if (is.null(input$Essences03)) return(NULL)
      tab <- tablocalmercu()$Tableau2 %>%
        filter(essence %in% input$Essences03 & ut %in% input$ut)
      g <- ggplot(tab, aes(x = type, y = vol, group = type, fill = type, alpha = tarif)) +
        scale_fill_manual(values = c("red", "darkgreen")) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(ut ~ essence + classe) +
        scale_alpha_manual(values = c(1, 0.1))
      g2 <<- g
      g
    }
  })

  # localmercu
  output$localmercu <- renderText({
    input$update022
    if (input$update022 == 0) return(NULL)
    if (is.null(input$Essences03)) return(NULL)
    if (is.null(input$ut)) return(NULL)
    tab <- tablocalmercu()$Tableau1 %>%
      filter(essence %in% input$Essences03 & ut %in% input$ut)
    withProgress(message = "Comparaison des résultats", style = "notification", value = 0.5, {
      Sys.sleep(0.25)
      resv <- gftools::describeBy(tab, group = list(tab$essence, tab$ut))
      incProgress(1)
    })
    Txt <- paste0("- AGENCE ", input$agence, ", EXERCICE ", input$exercice, " -\n")
    for (r in length(colnames(resv)):1) {
      Txt <- paste0(Txt, "- UT ", colnames(resv)[r], " -\n")
      for (s in length(rownames(resv)):1) {
        Txt <- paste0(
          Txt,
          "Pour l'essence ", rownames(resv)[s], " :\n - l'estimation LOCAL cube ", round(100 * ((resv[[s, r]]["tl_vbftigcom", "sum"] + resv[[s, r]]["tl_vhouppiers", "sum"]) / (resv[[s, r]]["te_vbftigcom", "sum"] + resv[[s, r]]["te_vhouppiers", "sum"]) - 1), 0),
          "% du volume bois fort total decoupe 7cm EMERCU, ", round(100 * (resv[[s, r]]["tl_vbftigcom", "sum"] / resv[[s, r]]["te_vbftigcom", "sum"] - 1), 0),
          "% du volume bois fort tige EMERCU et ", round(100 * (resv[[s, r]]["tl_vhouppiers", "sum"] / resv[[s, r]]["te_vhouppiers", "sum"] - 1), 0),
          "% du volume houppiers EMERCU :\n - le volume bois fort tige commercial LOCAL est de ", round(resv[[s, r]]["tl_vbftigcom", "sum"], 0),
          " m3, le volume bois fort tige commercial EMERCU est de ", round(resv[[s, r]]["te_vbftigcom", "sum"], 0),
          " m3,\n - le volume houppier LOCAL est de ", round(resv[[s, r]]["tl_vhouppiers", "sum"], 0),
          " m3, le volume houppier EMERCU est de ", round(resv[[s, r]]["te_vhouppiers", "sum"], 0), " m3.\n"
        )
      }
    }
    return(Txt)
  })

  output$Essences03 <- renderUI({
    if (input$update022 == 0) return(NULL)
    items <- unique(tablocalmercu()$Tableau1$essence)
    names(items) <- items
    selectInput("Essences03", " ", multiple = TRUE, items, selected = c("Toutes"))
  })
  
  output$ut <- renderUI({
    if (input$update022 == 0) return(NULL)
    items <- unique(tablocalmercu()$Tableau1$ut)
    names(items) <- items
    selectInput("ut", " ", multiple = TRUE, items, selected = c(outUT()))
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
      withCallingHandlers({
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
    tab03(),
    options = list(
      pageLength = 15
    ), server = FALSE
  )
}
