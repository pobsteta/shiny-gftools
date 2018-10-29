# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(shiny,shinythemes,shinyjs,leaflet,ggvis,ggrepel,dplyr,RColorBrewer,raster,gstat,rgdal,Cairo,ggmap,ggplot2,DT,tools,leaflet.extras,pool,RPostgreSQL,devtools)
# pacman::p_load_gh('hadley/tidyverse','tidyverse/ggplot2','tidyverse/dplyr','r-spatial/sf','jrowen/rhandsontable')
# pacman::p_load_gh('pobsteta/gftools')

library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(ggvis)
library(ggrepel)
library(raster)
library(gstat)
library(rgdal)
library(Cairo)
library(ggmap)
library(ggplot2)
library(DT)
library(tools)
library(leaflet.extras)
library(pool)
library(RPostgreSQL)
library(gftools)
library(rhandsontable)
library(tibble)
library(tidyr)
library(RColorBrewer)
library(dplyr)
library(xtable)
library(sf)

options(pgsql = list(
  "host" = "0.0.0.0",
  "port" = 35432,
  #"host" = "172.19.128.27",
  #"port" = 35432,
  "user" = "tryton",
  "password" = "tryton",
  "dbname" = "tryton"
))

pool <- dbPool(
  drv = "PostgreSQL",
  port = options()$pgsql$port,
  dbname = options()$pgsql$dbname,
  host = options()$pgsql$host,
  user = options()$pgsql$user,
  password = options()$pgsql$password
)
onStop(function() {
  poolClose(pool)
})


# fichiers temporaires
mname = tempfile(fileext = ".csv")
fname = tempfile(fileext = ".csv")
cname = tempfile(fileext = ".csv")

# liste des tarifs
listetarif <- setNames(1:3, c("SR","SL","AL"))
listessence <- c("Defaut", "Chene", "Hetre", "Autres feuillus", "Epicea", "Sapin", "Pin", "Autres resineux")

# houppier compris
mhouppier <- 'N'

# Vectorize TarONF3
TarifONF3v <- Vectorize(gftools::TarONF3)

# verifie le seuil des 16 connexions ouvertes autorisees
getConnection <- function(group) {
  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect("PostgreSQL", dbname = options()$pgsql$dbname, host = options()$pgsql$host, 
                              port = options()$pgsql$port, user = options()$pgsql$user, 
                              password = options()$pgsql$password, group=group)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect("PostgreSQL", dbname = options()$pgsql$dbname, host = options()$pgsql$host, 
                              port = options()$pgsql$port, user = options()$pgsql$user, 
                              password = options()$pgsql$password, group=group)
  }
  return(.connection)
}

#' BDDQueryONF
#'
#' @param query = requête au format SQL en texte
#'
#' @return Le résultat de la requête sur la base de données
#'
#' @examples
#' BDDQueryONF(query="SELECT ccod_cact, ccod_frt, llib2_frt, geom FROM forest")
#' 
BDDQueryONF <- function(query) {
  ## query posgresql database onf
  # set up connection
  conn <- dbConnect("PostgreSQL", dbname = options()$pgsql$dbname, host = options()$pgsql$host, 
                  port = options()$pgsql$port, user = options()$pgsql$user, 
                  password = options()$pgsql$password)
  # dummy query (obviously), including a spatial subset and ST_Simplify to simplify geometry (optionel)
  result <- sf::st_read(conn, query=query) %>%
    sf::st_transform(result, crs = 4326)
  dbDisconnect(conn)
  return(result)
}

#' delData
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
delData <- function(query) {
  pool::dbGetQuery(pool, query)
}

#' saveData
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
saveData <- function(query) {
  pool::dbGetQuery(pool, query)
}


#' loadData
#'
#' @param query 
#'
#' @return
#' @export
#'
#' @examples
loadData <- function(query) {
  if (length(query) == 0) {
    return(NULL)
  } else {
    data <- pool::dbGetQuery(pool, query)
    return(data)
  }
}


#' insertData
#'
#' @param table 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
insertData <- function(table, df) {
  dbWriteTable(pool, table, df, append = TRUE, row.names = FALSE)
}


# Data
dtdata <- BDDQueryONF(query = "SELECT id, iidtn_dt, llib_dt, geom FROM dt ORDER BY iidtn_dt")
agencedata <- BDDQueryONF(query = "SELECT id, iidtn_agc, llib_agc, geom FROM agence ORDER BY iidtn_agc")
forestdata <- BDDQueryONF(query="SELECT id, ccod_cact, ccod_frt, llib2_frt, geom FROM forest ORDER BY ccod_frt")
parcelledata <- BDDQueryONF(query="SELECT id, ccod_cact, ccod_frt, llib_frt, ccod_prf, geom FROM parcelle ORDER BY iidtn_prf")
pstdata <- BDDQueryONF(query = "SELECT ccod_cact, ccod_ut, clib_pst, geom FROM pst ORDER BY ccod_ut")
files <- loadData("SELECT s.id AS id, d.iidtn_dt AS dt, a.iidtn_agc AS agence, f.ccod_frt AS forest, p.ccod_prf AS parcelle 
                  FROM sample s, dt d, agence a, forest f, parcelle p 
                  WHERE s.dt=d.id AND s.agence=a.id AND s.forest=f.id AND s.parcelle=p.id")
filed <- loadData("SELECT * FROM filedata")
filem <- loadData("SELECT * FROM filemercuriale")
filec <- loadData("SELECT * FROM cahierclausedt")

#' BestTarifFindSch
#'
#' @param decemerge 
#' @param typvolemerge 
#' @param zonecalc 
#' @param clause 
#' @param essence 
#' @param classearbremin 
#' @param classearbremax 
#' @param barre 
#' @param agence 
#' @param exercice 
#' @param typzonecalc 
#'
#' @return
#' @export
#'
#' @examples
BestTarifFindSch <- function(decemerge = 7, typvolemerge = 'total', zonecalc = NULL, clause = NULL, essence = c('02', '09'), classearbremin = 20, classearbremax = 80,
                             barre = NULL, agence = 8415, exercice = 17, typzonecalc = 'ser') {
  split <- function(texte) {
    strsplit(texte, " ")[[1]][1]
  }
  splitv <- Vectorize(split)
  TarONF3v <- Vectorize(gftools::TarONF3)
  message("Extract mercuriale file...")
  mer <- data.frame(cdiam = seq(from = 10, to = 120, by = 5), tarif = rep("SR14", 23), houppier = c(rep(0,3), rep(30,20)), hauteur = rep(0,23))
  if (!is.null(clause)) {
    clo <- readr::read_tsv(
      clause,
      locale = readr::locale(encoding = "UTF-8", decimal_mark = "."),
      readr::cols(ess = readr::col_character(), dmin = readr::col_integer(), dmax = readr::col_integer(), dec = readr::col_double()),
      col_names = T)
  } else {
    clo <- data.frame(ess = 'Defaut', dmin = 10, dmax = 200, dec = 7)
  }
  message("Extract IFN data...")
  vecteur_annee <- c(2008:2016)
  dossier <- system.file("extdata/IFN", package = "gftools")
  # Extract placettes et arbres
  message("Extract data placettes...")
  plac <- data.frame()
  for (i in 1:length(vecteur_annee)) {
    yrs <- as.numeric(vecteur_annee[i])
    ifn <- gftools::getFich_IFN(obj = c("Pla"), Peup = FALSE, Morts = FALSE, Doc = TRUE, ans = yrs, doss = dossier)
    placet <- data.frame(yrs = yrs, ifn$Pla[[1]][, c("idp", "ser", "xl93", "yl93")])
    plac <- rbind(plac, placet)
  }
  message("Extract data arbres...")
  arb <- data.frame()
  for (i in 1:length(vecteur_annee)) {
    yrs <- as.numeric(vecteur_annee[i])
    ifn <- gftools::getFich_IFN(obj = c("Arb"), Peup = FALSE, Morts = FALSE, Doc = TRUE, ans = yrs, doss = dossier)
    arbre <- data.frame(yrs = yrs, ifn$Arb[[1]][, c("idp", "espar", "c13", "w", "htot", "hdec", "veget")])
    arb <- rbind(arb, arbre)
  }
  message(paste0("Extract data zonecalc: ", toupper(typzonecalc), "..."))
  zone <- sf::st_transform(zonecalc, crs = 2154)
  codereg <- unique(zone["code"]$code)
  listres <- vector("list", length(codereg)) 
  nreg <- length(codereg)
  for (reg in 1:nreg) {
    regzone <- zone["code"] %>% dplyr::filter(code == codereg[reg])
    message(paste0("Calcul pour la zone ", toupper(typzonecalc), " : ", codereg[reg], " (", reg, "/", nreg, ")"))
    placettes <- sf::st_intersection(sf::st_as_sf(plac, coords = c("xl93", "yl93"), crs = 2154, agr = "constant"), sf::st_geometry(regzone))
    # premier tableau data arbres
    tab <- arb %>%
      dplyr::filter(espar %in% essence) %>%
      # dplyr::filter(espar %in% c("02","03","09","17C","52","54","61","62","64","65")) %>%
      dplyr::filter(idp %in% placettes$idp) %>%
      dplyr::filter(veget == "0") %>%
      dplyr::select(espar, c13, htot, hdec) %>%
      dplyr::mutate(diam = round(c13 / pi, 0)) %>%
      dplyr::mutate(espar = as.character(espar)) %>%
      dplyr::left_join(gftools::code_ifn_onf, by = c(espar = "espar")) %>%
      dplyr::filter(!is.na(htot)) %>%
      dplyr::mutate(esscct = ifelse(splitv(essence) %in% c("Hetre","Chene","Pin"), splitv(essence), fr))
    ## creation des tableaux
    tab <- tab %>%
      dplyr::mutate(Classe = as.integer(floor(diam / 5 + 0.5) * 5)) %>%
      dplyr::inner_join(mer, by = c(Classe = "cdiam"))
    ## recherche la decoupe emerge dans les clauses ou prend decemerge
    defo <- ifelse(!is.null(clause), clo[clo$ess %in% 'Defaut', "dec"][[1]], decemerge)
    tab1 <- tab %>%
      dplyr::mutate(clo = splitv(esscct)) %>%
      dplyr::left_join(clo, by = c(clo = "ess"))
    tab2 <- tab1 %>%
      dplyr::filter(!is.na(dmin))
    tab3 <- tab1 %>%
      dplyr::filter(is.na(dmin)) %>%
      dplyr::select(espar,htot,hdec,diam,essence,Classe,fr,clo,tarif,houppier,hauteur) %>%
      dplyr::left_join(clo, by = c(fr = "ess"))
    tab4 <- dplyr::bind_rows(tab2, tab3) %>%
      dplyr::mutate(defaut = defo)
    tab4$decemerge <- ifelse(!is.na(tab4$dmin) & tab4$Classe >= tab4$dmin, tab4$dec, tab4$defaut)
    tab <- tab4 %>%
      dplyr::select(espar,essence,diam,Classe,htot,hdec,decemerge,tarif,hauteur,houppier)
    tab <- cbind(tab, E_VbftigCom = TarEmerge(c130 = pi * tab$diam, htot = tab$htot, hdec = tab$hdec, espar = tab$espar, typevol = "tige", dec = tab$decemerge))
    tab <- cbind(tab, E_VHouppiers = TarEmerge(c130 = pi * tab$diam, htot = tab$htot, hdec = tab$hdec, espar = tab$espar, typevol = "houp", dec = tab$decemerge))
    tab <- cbind(tab, E_Vbftot7cm = TarEmerge(c130 = pi * tab$diam, htot = tab$htot, hdec = tab$hdec, espar = tab$espar, typevol = "total", dec = 7)) %>%
      dplyr::mutate(E_PHouppiers = E_Vbftot7cm / E_VbftigCom - 1) %>%
      dplyr::filter(!is.na(E_Vbftot7cm))
    tab1 <- tab %>%
      mutate("Essence_Hou" = paste(tab$essence, "Hou", sep="_")) %>%
      dplyr::select(Essence_Hou, Classe, E_PHouppiers) %>%
      dplyr::mutate_if(is.numeric, funs(round(., 2))) %>%
      group_by(Classe, Essence_Hou) %>%
      summarise_at(c("E_PHouppiers"), funs(mean)) %>%
      mutate_at(c("E_PHouppiers"), funs(as.integer(round(100 * . / (1 + .), 0)))) %>%
      arrange(Essence_Hou, Classe) %>%
      spread(Essence_Hou, E_PHouppiers)
    tab <- tab %>%
      dplyr::filter(diam >= classearbremin & diam <= classearbremax) %>%
      dplyr::mutate(
        numSchR = E_Vbftot7cm / 5 * 70000 / (diam - 5) / (diam - 10) - 8,
        numSchL = E_Vbftot7cm / 5 * 90000 / diam / (diam - 5) - 8,
        numAlg = E_Vbftot7cm * 28000000 / (310000 - 45200 * diam + 2390 * diam ^ 2 - 2.9 * diam ^ 3) - 8
      )
    res <- tab %>%
      dplyr::group_by(essence) %>%
      dplyr::summarise_at(c("numSchR", "numSchL", "numAlg"), funs(mean, var))
    res[, 5:7] <- round(res[, 5:7] ^ 0.5 / res[, 2:4], 2)
    res[, 2:4] <- round(res[, 2:4], 2)
    names(res) <- c("essence", "SR", "SL", "AL", "SRcv", "SLcv", "ALcv")
    tab2 <- as.data.frame(res) %>%
      dplyr::mutate(tar = names(.)[max.col(.[5:7]*-1)+1L],
                    Best_tarif = case_when(
                      tar == "SR" ~ paste0("SR", as.character(formatC(round(.[, c("SR")], 0), width=2, flag="0"))),
                      tar == "SL" ~ paste0("SL", as.character(formatC(round(.[, c("SL")], 0), width=2, flag="0"))),
                      tar == "AL" ~ paste0("AL", as.character(formatC(round(.[, c("AL")], 0), width=2, flag="0")), "+"))) %>%
      dplyr::select(essence, SR, SL, AL, SRcv, SLcv, ALcv, Best_tarif)
    tab3 <- tab %>%
      dplyr::distinct(essence, Classe) %>%
      group_by(Classe, essence) %>%
      dplyr::right_join(tab2[, c("essence", "Best_tarif")], by = c(essence = "essence")) %>%
      dplyr::ungroup()
    tab4 <- tab3 %>%
      dplyr::mutate("Essence_Tar" = paste(tab3$essence, "Tar", sep="_")) %>%
      dplyr::select(Essence_Tar, Classe, Best_tarif) %>%
      arrange(Essence_Tar, Classe) %>%
      spread(Essence_Tar, Best_tarif) 
    tab5 <- merge(tab1, tab4)
    # calcul des graphes des essences comparant Best EMERGE avec LOCAL de ProdBois
    ness <- length(essence)
    # ness <- 10
    lres <- vector("list", ness)
    for (sp in 1:ness) {
      if (!is.null(barre)) {barre$set(value = ness*(reg-1)+sp)}
      species <- essence[sp]
      codess <- gftools::code_ifn_onf %>%
        filter(espar == species) %>%
        pull(code)
      nomess <- gftools::code_ifn_onf %>%
        filter(espar == species) %>%
        pull(essence)
      message(paste0("Graphe de l'essence : ", nomess, " (", species, " - ", sp, "/", ness, ")"))
      mercuess <- tab5 %>%
        dplyr::select(Classe, starts_with(nomess))
      mercuess$haut <- 0
      if (length(mercuess) == 4) {
        names(mercuess) <- c("cdiam", "houppier", "tarif", "hauteur")
      } else {
        next
      }
      if (species %in% c("02")) {
        qess <- "(ess='CHP' OR ess='CHX')" 
      } else if (species %in% c('03')) {
        qess <- "(ess='CHS' OR ess='CHX')"
      } else if (species %in% c('61')) {
        qess <- "(ess='SAP' OR ess='S.P')"
      } else {
        qess <- paste0("ess='", codess, "'")
      }
      cc <- codereg[reg]
      idreg <- zone %>% filter(code == cc) %>% pull(id) %>% paste(., collapse = ',')
      query <- sprintf("SELECT exercice, agence, ess AS essence, diam, haut, nb, tahd AS l_phouppiers, tacomd, volcu AS l_vbftigcom, volcu*(tahd/100.0) AS l_vhouppiers, 
                volcu*(1+tahd/100.0) AS l_vbftot7cm FROM datacab d, forest f, %s r WHERE agence='%s' AND exercice=%s AND diam>0 AND tacomd!='0' AND %s
                AND cofrt=ccod_frt AND agence=ccod_cact AND st_intersects(f.geom,r.geom) AND r.id IN (%s)",
                       typzonecalc, agence, exercice, qess, idreg
      )
      res <- loadData(query = query)
      if (!is.null(res)) {
        if (nrow(res) > 0) {
          res <- res %>%
            mutate(classe = floor(diam / 5 + 0.5) * 5) %>%
            inner_join(mercuess, by = c(classe = "cdiam"))
          tab <- res %>%
            mutate(
              e_vbftot7cm = as.numeric(TarifONF3v(tarif = tarif, entr1 = diam, entr2 = haut, details = FALSE)),
              e_vhouppiers = e_vbftot7cm * houppier / 100,
              e_vbftigcom = e_vbftot7cm - e_vhouppiers,
              e_phouppiers = houppier / 100
            )
          # on ne veut q'une essence
          if (species %in% c("02", "03")) {
            tab$essence <- 'CHX'
          } else if (species %in% c('61')) {
            tab$essence <- 'SAP'
          } else {
            tab$essence <- paste0("ess='", codess, "'")
          }
          tab.r <- tab %>%
            mutate(tl_vbftigcom = l_vbftigcom * nb, tl_vhouppiers = l_vhouppiers * nb, te_vbftigcom = e_vbftigcom * nb, te_vhouppiers = e_vhouppiers * nb) %>%
            group_by(exercice,agence,essence,classe) %>%
            summarise_at(c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers", "nb"), sum, na.rm=TRUE)
          resv <- gftools::describeBy(tab.r, group = tab.r$essence)
          txt <- paste0("ESSENCE ", nomess, " - AGENCE ", tab.r$agence[1], " - EXERCICE ", tab.r$exercice[1], " : ")
          for (r in length(resv):1) {
            txt[2] <- paste0("Pour l'essence ", names(resv[r]), ",")
            txt[3] <- paste0(" l'estimation LOCAL cube ", round(100 * ((resv[[r]]["tl_vbftigcom", "sum"] + resv[[r]]["tl_vhouppiers", "sum"]) /
                                                                         (resv[[r]]["te_vbftigcom", "sum"] + resv[[r]]["te_vhouppiers", "sum"]) - 1), 0),
                             "% du volume bois fort total decoupe 7cm EMERCU, ", round(100 * (resv[[r]]["tl_vbftigcom", "sum"] / resv[[r]]["te_vbftigcom", "sum"] - 1), 0),
                             "% du volume bois fort tige EMERCU et ", round(100 * (resv[[r]]["tl_vhouppiers", "sum"] / resv[[r]]["te_vhouppiers", "sum"] - 1), 0),
                             "% du volume houppiers EMERCU (")
            txt[4] <- paste0("le volume bois fort tige commercial LOCAL est de ", round(resv[[r]]["tl_vbftigcom", "sum"], 0),
                             " m3, le volume bois fort tige commercial EMERCU est de ", round(resv[[r]]["te_vbftigcom", "sum"], 0),
                             " m3,")
            txt[5] <- paste0("le volume houppier LOCAL est de ", round(resv[[r]]["tl_vhouppiers", "sum"], 0),
                             " m3 et le volume houppier EMERCU est de ", round(resv[[r]]["te_vhouppiers", "sum"], 0), " m3."
            )
          }
          txt[6] <- paste0("Les tarifs locaux utilisés sont : ", paste(unique(res$tacomd), collapse = ", "), ".")
          table1 <- tab.r %>%
            dplyr::select(exercice, agence, essence, classe, tl_vbftigcom, tl_vhouppiers, te_vbftigcom, te_vhouppiers) %>%
            dplyr::mutate(voltot_E = te_vbftigcom + te_vhouppiers, 
                          voltot_L = tl_vbftigcom + tl_vhouppiers,
                          vbftigcom_L_E = 100 * (tl_vbftigcom / te_vbftigcom - 1),
                          vhouppiers_L_E = 100 * (tl_vhouppiers / te_vhouppiers - 1),
                          voltot_L_E = 100 * (voltot_L / voltot_E - 1))
          names(table1) <- c("exercice","agence","essence","classe","vbftigcom_L","vhouppiers_L","vbftigcom_E","vhouppiers_E","voltot_E","voltot_L",
                             "%_vbftigcom_L_E","%_vhouppiers_L_E","%_voltot_L_E")
          table2 <- table1 %>%
            gather("typvol", "vol", 5:13)
          table3 <- table2 %>%
            dplyr::mutate("Class" = as.character(formatC(table2$classe, width=3, flag="0"))) %>%
            dplyr::select(exercice, agence, essence, Class, typvol, vol) %>%
            arrange(Class) %>%
            spread(Class, vol) %>%
            mutate_if(is.numeric, funs(round(.,0)))
          
          table4 <- as.data.frame(table3[,-c(1:3)])
          table5 <- cbind(table4, Total = rowSums(table4[, -c(1)]))
          table5$Total[1] <- 100 * (table5$Total[5] / table5$Total[4] - 1)
          table5$Total[2] <- 100 * (table5$Total[7] / table5$Total[6] - 1)
          table5$Total[3] <- 100 * (table5$Total[9] / table5$Total[8] - 1)
          table5 <- table5 %>%
            mutate_if(is.numeric, funs(round(.,0)))
          tab.t <- reshape2::melt(tab.r, id.vars = c("exercice","agence","essence","classe"), measure.vars = c("tl_vbftigcom", "tl_vhouppiers", "te_vbftigcom", "te_vhouppiers")) %>%
            mutate(Type = substr(variable, 1, 2), variable = substr(variable, 4, 12))
          names(tab.t) <- c("exercice","agence","essence","classe","tarif","vol","type")
          tab.t$type[which(tab.t$type=='tl')] <- "LOCAL"
          tab.t$type[which(tab.t$type=='te')] <- "EMERCU"
          tab.t$tarif[which(tab.t$tarif=='vhouppier')] <- "VHouppiers"
          tab.t$tarif[which(tab.t$tarif=='vbftigcom')] <- "VbftigCom"
          p <- ggplot(tab.t, aes(x = type, y = vol, group = type, fill=type, alpha = tarif)) +
            scale_fill_manual(values = c("red", "darkgreen")) +
            geom_bar(stat="identity", position = "stack") +
            facet_grid(agence + essence ~ classe) +
            scale_alpha_manual(values=c(1,0.1))
          lres[[sp]] <- list(tab.r, p, txt, table5)
          names(lres[[sp]]) <- c("Tableau3", "Graphe1", "Texte", "Tableau4")
        }
      }
      
      # uniquement la région
      creg <- codereg[reg]
      m <- zone %>%
        dplyr::filter(code == creg)
      poste <- BDDQueryONF(query = paste0("SELECT ccod_cact, ccod_ut, clib_pst, geom FROM pst WHERE ccod_ut LIKE '", agence, "%' ORDER BY ccod_ut"))
      pst <- sf::st_intersection(m, sf::st_transform(poste, crs = 2154)) %>%
        fortify()
      pstbbox <- sf::st_bbox(pst)
      carte <- ggplot() +
        ggplot2::geom_sf(data = sf::st_as_sf(m)) +
        ggplot2::geom_sf(data = pst, aes(fill=ccod_ut)) +
        scale_fill_brewer(palette = "Set3", name = "Poste") +
        labs(title=paste("Agence - ", agence, " : ", pst$code[1], "-", pst$reg[1])) +
        coord_sf(xlim = c(pstbbox[[1]], pstbbox[[3]]), ylim = c(pstbbox[[2]], pstbbox[[4]])) +
        theme(axis.text = element_blank(),
              line = element_blank(),
              plot.title = element_text(size = 10, color = "DarkGreen"))
      listres[[reg]] <- list(tab5, tab2, lres, carte)
      names(listres[[reg]]) <- c("Tableau1", "Tableau2", "Species", "Carte")
    }
  }
  message("...Calculation realized!")
  return(listres)
}
