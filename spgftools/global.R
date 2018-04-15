# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(shiny,shinythemes,shinyjs,leaflet,ggvis,ggrepel,dplyr,RColorBrewer,raster,gstat,rgdal,Cairo,ggmap,ggplot2,DT,tools,data.table,leaflet.extras,pool,RPostgreSQL,devtools)
# pacman::p_load_gh('pobsteta/gftools','hadley/tidyverse','tidyverse/ggplot2','tidyverse/dplyr','r-spatial/sf','jrowen/rhandsontable')

library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(ggvis)
library(ggrepel)
library(dplyr)
library(RColorBrewer)
library(raster)
library(gstat)
library(rgdal)
library(Cairo)
library(ggmap)
library(ggplot2)
library(DT)
library(tools)
library(data.table)
library(leaflet.extras)
library(pool)
library(RPostgreSQL)
library(gftools)
library(rhandsontable)

options(pgsql = list(
  "host" = "167.99.143.72",
  "port" = 35432,
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
  result <- st_read_db(conn, query=query, geom="geom") %>%
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
  dbGetQuery(pool, query)
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
  dbGetQuery(pool, query)
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
  data <- dbGetQuery(pool, query)
  return(data)
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
