library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(ggvis)
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
  conn <- dbConnect("PostgreSQL", user="tryton", password="tryton", port=5432, dbname="onf")
  # dummy query (obviously), including a spatial subset and ST_Simplify to simplify geometry (optionel)
  result <- st_read_db(conn, query=query, geom="geom") %>%
    sf::st_transform(result, crs = 4326)
  dbDisconnect(conn)
  return(result)
}
dtdata <- BDDQueryONF(query = "SELECT iidtn_dt, llib_dt, geom FROM dt ORDER BY iidtn_dt")
agencedata <- BDDQueryONF(query = "SELECT iidtn_agc, llib_agc, geom FROM agence ORDER BY iidtn_agc")
forestdata <- BDDQueryONF(query="SELECT ccod_cact, ccod_frt, llib2_frt, geom FROM forest ORDER BY ccod_frt")
parcelledata <- BDDQueryONF(query="SELECT ccod_cact, ccod_frt, llib_frt, ccod_prf, geom FROM parcelle ORDER BY iidtn_prf")
pstdata <- BDDQueryONF(query = "SELECT ccod_cact, ccod_ut, clib_pst, geom FROM pst ORDER BY ccod_ut")
