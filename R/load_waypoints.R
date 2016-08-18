#' @title Import waypoints from .gpx file.
#' @description  Imports .gpx file prior to cleaning to human readable
#' values in a data.frame.
#'
#' @param x Name of .gpx file location
#'
#' @return Function \code{load_waypoints} generates a
#' data.frame with points from .gpx waypoints.
#' @export
#'
#' @examples
#' \dontrun{
#' }

load_waypoints <- function(x){
  myfile <- x$aname

  gp1 <- plotKML::readGPX(myfile, metadata = TRUE, bounds = TRUE,
                 waypoints = TRUE, tracks = FALSE, routes = FALSE)

  if (length(gp1$waypoints)< 1){
    df1 <- data.frame(lon = c(-200,-200), lat = c(-200,-200), ele = c("0","0"),
                      time = c("2001-01-01T16:18:35Z","2001-01-01T16:22:35Z"),
                      name = NA, sym = NA, type = NA, extensions = NA, cmt = NA, desc = NA)
  } else {
    df1 <-  data.frame(gp1$waypoints)
  }
  # correct date in cases when .gpx file is saved and transfered
 if(length(names(df1)) < 9){
   #library(stringr)
   df1$adate <- stringr::str_sub(df1$time,1,10)
  df1$atime <- stringr::str_sub(df1$time,-9,-2)
  df1$adatetime <- paste(df1$adate, df1$atime)
  df1$adt <- as.POSIXct(strptime(df1$adatetime,"%Y-%m-%d %H:%M:%S", tz="UTC"))
 }else{
   Sys.setlocale(category = "LC_TIME", locale = "English")
   #library(stringr)
   df1$adate <- format(as.Date(stringr::str_sub(df1$cmt,1,9),"%d-%b-%y"), "%Y-%m-%d")
   df1$atime <- stringr::str_sub(df1$cmt,10,18)
   df1$adatetime <- paste(df1$adate, df1$atime)
   df1$adt <- as.POSIXct(strptime(df1$adatetime,"%Y-%m-%d %H:%M:%S", tz="America/Belem"))
   df1$adt <- as.POSIXct(strptime(format(df1$adt, "%Y-%m-%d %H:%M:%S", tz="UTC"),
                       "%Y-%m-%d %H:%M:%S", tz="UTC"))

   Sys.setlocale(category = "LC_TIME", locale = "Portuguese_Brazil.1252")
   }
  df1
}
