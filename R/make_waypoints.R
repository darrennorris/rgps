

#' @title Import and summarise waypoints.
#' @description Import waypoints from .gpx files and convert to
#'  .csv and shapefiels.
#' @param folder_gpx Location of folder containing .gpx files.
#' @param river_shape Location of folder containing shapefile to define survey zones
#' @param folder_result Location where results are saved.
#' @param track_data Optional. List with track data.frame in function \code{make_tracks}.
#' Used to include trip details.
#' @param date_start Optional. Character of start date ("dd/mm/yyyy").
#' @param date_end Character of end date ("dd/mm/yyyy").
#' @param make_shape_wp Logical (TRUE/FALSE). Generate shapefile with waypoints.
#'
#' @return Writes a .csv file and shapefiles with waypoints from .gpx files.
#' @export
#' @importFrom "stats" "na.omit" "sd"
#' @importFrom "utils" "write.csv2"
#'
#' @examples
#' \dontrun{
#' }
make_waypoints <- function(folder_gpx, river_shape, folder_result,
                           track_data = NA, date_start = NA, date_end = NA,
                           make_shape_wp = FALSE){

setwd(folder_gpx)
input_path<-getwd() # target directory
myfileinfo<-file.info(list.files(input_path, pattern= ".gpx",
                                 ignore.case=TRUE,full.names=TRUE))
myfileinfo$aname <- rownames(myfileinfo)

df_points_in <- plyr:: ddply(myfileinfo, .variables="aname",
                             .fun = load_waypoints)

selGood <- which(is.na(df_points_in$time)==FALSE &
                   is.na(df_points_in$name)==FALSE &
                   is.na(df_points_in$lon)==FALSE &
                   is.na(df_points_in$lat)==FALSE )

dfc <- df_points_in[selGood, ]

#remove duplicates based on time and name
dfc1 <- dfc[!duplicated(paste(dfc$time, dfc$name)), ]

# convert GPS time to time in survey zone
if("ele" %in% names(dfc1)==FALSE){dfc1$ele <- NA}
dfc1$ele <- as.numeric(dfc1$ele)
dfc1$adtnum <- as.numeric(dfc1$adt)
dfc1$ayear <- format(dfc1$adt,"%Y", tz="America/Belem")
dfc1$adtAP <- format(dfc1$adt, tz="America/Belem",usetz=TRUE)
dfc1$adate <- as.Date(format(dfc1$adt,"%d/%m/%Y",tz="America/Belem"),
                      format="%d/%m/%Y")
dfc1$atimeAP <- format(dfc1$adt, "%H:%M:%S",tz="America/Belem")

# select waypoints collected on specified dates
#date_start = NA
if (is.na(date_start)==FALSE){
  mdates <- seq(as.Date(date_start, format="%d/%m/%Y",tz="America/Belem"),
                as.Date(date_end, format="%d/%m/%Y",tz="America/Belem"), by ="day")
  selD <- which(dfc1$adate %in% mdates)
  if(length(selD) > 0) {dfc1 <- dfc1[selD, ]}else{stop("no waypoints in specified dates")}
  #all_tracks_summary <- all_tracks_summary[selD, ]
}

## 3) add zones
#require(raster)
s1 <- raster::shapefile(river_shape)

#require(sp)
sp1 <- dfc1
sp::coordinates(sp1) <- ~lon+lat
# assign CRS/projection
sp::proj4string(sp1) <- sp::proj4string(s1)
dfc1$zone <- sp::over(sp1, s1)[,2]

## add trip_id# join with trips, exclude NA trips
# sequce of valid trip points
# merge to add trip to waypoints
#track_data <- list_track_data
if(is.na(track_data)==FALSE){
trips <- track_data$track_summary
trips$trip_startpx <- as.POSIXct(strptime(trips$trip_start,"%Y-%m-%d %H:%M:%S",
                                          tz="America/Belem"))
trips$trip_endpx <- as.POSIXct(strptime(trips$trip_end,"%Y-%m-%d %H:%M:%S",
                                        tz="America/Belem"))
selZ <- which(is.na(trips$zone)==FALSE)
trips <- trips[selZ, ]

if(nrow(trips)>0){
# need to add ifelse here so when trip zones are all NA, dftrips is created
mseq <- function(x){
  dfout <- data.frame(sadt = seq(x$trip_startpx, x$trip_endpx, by="sec"))
}
#require(plyr)
dftrips <- plyr::ddply(trips, c("zone","trip_id", "trip_start", "trip_end"),
                       .fun=mseq)
dftrips$adtAP <- format(dftrips$sadt, usetz=TRUE)
}else{
dftrips <- data.frame(zone = NA, trip_id = NA, trip_start = NA, trip_end = NA,
                      sadt = NA, adtAP = NA)
}

gc()

dfc2 <- merge(dfc1, dftrips, all.x=TRUE, by.x = c('zone', 'adtAP'),
              by.y = c('zone', 'adtAP'))

dfc2$tipo_censo <- ifelse(is.na(dfc2$trip_id)==FALSE,"censo","extracenso")

dfc3 <- dfc2[order(dfc2$adtnum), ]
rownames(dfc3) <- NULL

df_outwp <- data.frame(aname = dfc3$'aname', tipo_censo = dfc3$tipo_censo,
                       zone = dfc3$'zone',
                       trip_id = dfc3$trip_id,
                       trip_start = dfc3$trip_start, trip_end = dfc3$trip_end,
                       gps_time = dfc3$'time',
                       adate = format(dfc3$'adate', "%d/%m/%Y"), hora = dfc3$'atimeAP',
                       gps_name = dfc3$'name',
                       lon = dfc3$'lon', lat = dfc3$'lat', ele = dfc3$'ele',
                       transecto = NA, clima = NA, nivel_rio = NA, pessoal = NA)

df_outwp2 <- data.frame(aname = dfc3$'aname', tipo_censo = dfc3$tipo_censo,
                       zone = dfc3$'zone',
                       trip_id = dfc3$trip_id,
                       trip_start = dfc3$trip_start, trip_end = dfc3$trip_end,
                       gps_time = dfc3$'time',
                       adate = format(dfc3$'adate', "%d/%m/%Y"), hora = dfc3$'atimeAP',
                       gps_name = dfc3$'name',
                       lon = dfc3$'lon', lat = dfc3$'lat', ele = dfc3$'ele',
                       lon_x = dfc3$'lon', lat_y = dfc3$'lat')
}else{
  df_outwp <- data.frame(aname = dfc1$'aname', tipo_censo = NA,
                         zone = dfc1$'zone',
                         trip_id = NA,
                         trip_start = NA, trip_end = NA,
                         gps_time = dfc1$'time',
                         adate = format(dfc1$'adate', "%d/%m/%Y"), hora = dfc1$'atimeAP',
                         gps_name = dfc1$'name',
                         lon = dfc1$'lon', lat = dfc1$'lat', ele = dfc1$'ele',
                         transecto = NA, clima = NA, nivel_rio = NA, pessoal = NA)

  df_outwp2 <- data.frame(aname = dfc1$'aname', tipo_censo = NA,
                          zone = dfc1$'zone',
                          trip_id = NA,
                          trip_start = NA, trip_end = NA,
                          gps_time = dfc1$'time',
                          adate = format(dfc1$'adate', "%d/%m/%Y"), hora = dfc1$'atimeAP',
                          gps_name = dfc1$'name',
                          lon = dfc1$'lon', lat = dfc1$'lat', ele = dfc1$'ele',
                          lon_x = dfc1$'lon', lat_y = dfc1$'lat')

}

df_outwp <- df_outwp[order(df_outwp$adate, df_outwp$hora), ]
rownames(df_outwp) <- NULL

# 6) export waypoints
fileend <- paste(format(Sys.time(),"%Y%m%d"), format(Sys.time(),"%H%M%S"), sep="_")
# 6.1 waypoints .csv
setwd(folder_result)
namew <- paste(paste("all_waypoints", fileend, sep="_"),".csv",sep="")
write.csv2(df_outwp, namew)

#6.3 write shape
if(make_shape_wp!=FALSE){
  setwd(folder_result)
  dir.create(paste("shapes_waypoints", fileend, sep="_"))
  newf <- paste(getwd(),paste("shapes_waypoints", fileend, sep="_"), sep="/")
  setwd(newf)
  #require(raster)
  #require(sp)
  sppoint <- df_outwp2[, c('aname', 'tipo_censo','zone','trip_id', 'trip_start','gps_time',
                                'adate', 'hora', 'gps_name','lon', 'lat', 'ele', 'lon_x', 'lat_y')]
  sp::coordinates(sppoint) <- ~lon_x+lat_y
  sp::proj4string(sppoint) <- sp::CRS("+init=epsg:4326")
  namepoint <- paste(paste("waypoints", fileend, sep="_"),".shp",sep="")
  raster::shapefile(sppoint, filename = namepoint)
}

# 7) export as list
return(list_waypoints <- list(waypoints = df_outwp))
}
