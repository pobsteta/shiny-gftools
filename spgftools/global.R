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
library(gftools)
library(ggplot2)
library(DT)
library(tools)
library(data.table)
library(leaflet.extras)
library(gftools)
library(rhandsontable)
library(RPostgreSQL)

options(pgsql = list(
  "host" = "167.99.143.72",
  "port" = 35432,
  "user" = "tryton",
  "password" = "tryton"
))
dbname <- "tryton"


# fichiers temporaires
mname = tempfile(fileext = ".csv")
fname = tempfile(fileext = ".csv")

# liste des tarifs
listetarif <- setNames(1:3, c("SR","SL","AL"))

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
  db <- dbConnect("PostgreSQL", dbname = dbname, host = options()$pgsql$host, 
                  port = options()$pgsql$port, user = options()$pgsql$user, 
                  password = options()$pgsql$password)
  # dummy query (obviously), including a spatial subset and ST_Simplify to simplify geometry (optionel)
  result <- st_read_db(db, query=query, geom="geom") %>%
    sf::st_transform(result, crs = 4326)
  dbDisconnect(db)
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
  # Connect to the database
  db <- dbConnect("PostgreSQL", dbname = dbname, host = options()$pgsql$host, 
                  port = options()$pgsql$port, user = options()$pgsql$user, 
                  password = options()$pgsql$password)
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
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
  # Connect to the database
  db <- dbConnect("PostgreSQL", dbname = dbname, host = options()$pgsql$host, 
                  port = options()$pgsql$port, user = options()$pgsql$user, 
                  password = options()$pgsql$password)
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
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
  # Connect to the database
  db <- dbConnect("PostgreSQL", dbname = dbname, host = options()$pgsql$host, 
                  port = options()$pgsql$port, user = options()$pgsql$user, 
                  password = options()$pgsql$password)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
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
  # Connect to the database
  db <- dbConnect("PostgreSQL", dbname = dbname, host = options()$pgsql$host, 
                  port = options()$pgsql$port, user = options()$pgsql$user, 
                  password = options()$pgsql$password)
  # upsert the dataframe
  dbWriteTable(db, table, df, append = TRUE, row.names = FALSE)
  dbDisconnect(db)
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


