

#' @title Import tracklog data
#' @description  Imports .gpx file prior to cleaning to human readable
#' values in a data.frame.
#'
#' @param x Name of .gpx file location
#'
#' @return Function \code{load_tracks} generates a
#' data.frame with points from .gpx tracklogs.
#' @export
#'
#' @examples
#' \dontrun{
#' }
load_tracks <- function(x){
  myfile <- x$aname

  gp1 <- plotKML::readGPX(myfile, metadata = TRUE, bounds = TRUE,
                 waypoints = FALSE, tracks = TRUE, routes = FALSE)

  if (length(gp1$tracks)< 1){
    df1 <- data.frame(lon = c(-200,-200), lat = c(-200,-200), ele = c("0","0"),
                      time = c("2001-01-01T16:18:35Z","2001-01-01T16:22:35Z"))
  } else {
    mydataf <- function(x){
      if (length(names(data.frame(x))) %in% c(4,5)) {
        df1 <- data.frame(x)
        df1 <- df1[, 1:4]
        names(df1) <- c("lon", "lat", "ele", "time")
        df1}else{
          df1 <- data.frame(x,time=NA)
          names(df1) <- c("lon", "lat", "ele", "time")
          df1}
    }

    df1 <- plyr::ldply(gp1$tracks, mydataf)
    df1
  }
}
