
#' @title Import and summarise tracklogs and waypoints.
#' @description Import and summarise tracklogs and waypoints.
#' @param folder_gpx Location of folder containing .gpx files.
#' @param river_shape Location of folder containing shapefile to define survey zones
#' @param folder_result Location where results are saved.
#' @param date_start Optional. Character of start date ("dd/mm/yyyy")
#' @param date_end Optional. Character of end date ("dd/mm/yyyy")
#' @param make_shape Optional. Logical (TRUE/FALSE). Write a shapefile with all tracklog points.
#' @param do_waypoints Optional. Logical (TRUE/FALSE). Generate .csv with waypoints.
#' @param make_shape_wp Optional. Logical (TRUE/FALSE). Generate shapefile with waypoints.
#' @param type_csv Required. What type of .csv to export? ".csv" or ".csv2"
#' 
#' @return Will generate R objects and export .csv and shapefiles.
#'
#' @export
#' @importFrom "stats" "na.omit" "sd"
#' @importFrom "utils" "write.csv2"
#' @importFrom "utils" "write.csv"
#'
#' @examples
#' \dontrun{
#' }
make_tracks <- function(folder_gpx, river_shape, folder_result,
                        date_start = NA, date_end = NA, make_shape = FALSE,
                        do_waypoints = FALSE, make_shape_wp = FALSE,
                        type_csv = NA){
  # 1) load gpx

  # 1.1) list of .gpx files
  setwd(folder_gpx)
  input_path<-getwd() # target directory
  myfileinfo<-file.info(list.files(input_path, pattern= ".gpx",
                                   ignore.case=TRUE,full.names=TRUE))
  myfileinfo$aname <- rownames(myfileinfo)
  # 1.2 run
  gc()
  #library(plyr)
  df_tracks_in <- plyr::ddply(myfileinfo, .variables="aname",
                              .fun = load_tracks)

  # 2) cleaning, should clean tracklogs, remove duplicates and common import errors
  gc()
  #library(plyr)
  df_tracks_out <- plyr::ddply(df_tracks_in, .variables="aname",
                               .fun = clean_tracks)
  # 2.1 finish
  selg <- which(is.na(df_tracks_out$lon)==FALSE)
  df_tracks_out <- df_tracks_out[selg, ]
  #remove duplicates
  df_tracks_out <- df_tracks_out[!duplicated(df_tracks_out$adtnum), ]
  # drop first distance
  selSt <- which(df_tracks_out$trip_start_flag == 1)
  df_tracks_out[selSt, 'dist_m_census'] <- NA

  ## 3) add zones
  #library(raster)
  s1 <- raster::shapefile(river_shape)
  #library(sp)
  sp1 <- df_tracks_out
  sp::coordinates(sp1) <- ~lon+lat
  # assign CRS/projection
  sp::proj4string(sp1) <- sp::proj4string(s1)
  df_tracks_out$zone <- sp::over(sp1, s1)[,2]
  df_tracks_out <- df_tracks_out[order(df_tracks_out$adtnum), ]
  rownames(df_tracks_out) <- NULL

  # 4) summary
  all_tracks_summary <- plyr::ddply(df_tracks_out,
                                    .variables=c("aname", "seg_id", "zone"),
                                    plyr::summarize,
                              transect = NA,
                              Day = NA,
                              min_t = min(adt),
                              max_t = max(adt),
                              aname = min(aname),
                              ayear = min(ayear),
                              adate = as.Date(format(min(adt),"%d/%m/%Y",tz="America/Belem"),
                                              format="%d/%m/%Y"),
                              anomes = format(min(adt),"%Y%m",tz="America/Belem"),
                              trip_start = format(min(adt), tz="America/Belem"),
                              trip_end = format(max(adt), tz="America/Belem"),
                              trip_time_sec = (max(as.numeric(adt)) - min(as.numeric(adt))),
                              trip_time_hr = ((max(as.numeric(adt)) - min(as.numeric(adt)))/60)/60,
                              speed_mean = round(mean(na.omit(vel)),3),
                              V_min = round(min(na.omit(vel)),2),
                              V_max = round(max(na.omit(vel)),2),
                              V_sd = round(sd(na.omit(vel)),2),
                              dist_tot_km = sum(na.omit(dist_m_clean))/1000,
                              dist_tot_census_km = sum(na.omit(dist_m_census))/1000
  )

  all_tracks_summary$trip_id <- paste(all_tracks_summary$zone,
                                      all_tracks_summary$trip_start, sep="_")

 # select dates
 if (is.na(date_start)==FALSE){
   mdates <- seq(as.Date(date_start, format="%d/%m/%Y",tz="America/Belem"),
                 as.Date(date_end, format="%d/%m/%Y",tz="America/Belem"), by ="day")
selD <- which(all_tracks_summary$adate %in% mdates)
if(length(selD) > 0) {all_tracks_summary <- all_tracks_summary[selD, ]}else{stop("no tracks in specified dates")}
#all_tracks_summary <- all_tracks_summary[selD, ]
}

  # select  trips lasting 3 minutes or more
  selT <- which(all_tracks_summary$trip_time_sec > 179)
  all_tracks_summary <- all_tracks_summary[selT, ]

  # count of number of survey days, plus sorts by date
  all_tracks_summary <- merge(all_tracks_summary,
                              data.frame(adate = unique(all_tracks_summary$adate),
                                         day_count = rank(unique(all_tracks_summary$adate))), all.x = TRUE)
  all_tracks_summary <- all_tracks_summary[order(all_tracks_summary$trip_start), ]
  rownames(all_tracks_summary) <- NULL

  # 5) select only valid track points
  # Result =  data frame with track points for trips in all_tracks_summary
  t_seq <- function(x){
    df1 <- data.frame(mydt = seq(from = x$min_t, to = x$max_t, by = "sec"))
  }

  #require(plyr)
  d_seq <- plyr::ddply(all_tracks_summary,
                       .variables=c("aname", "seg_id", "zone",
                                       "trip_start", "trip_id"), .fun = t_seq)
  # data frame with track points for trips in all_tracks_summary
  dt <- merge(df_tracks_out, d_seq, by.x=c("zone", "adt" ), by.y=c("zone","mydt"))
  dt  <- dt [order(dt$adtnum), ]
  rownames(dt ) <- NULL

  df_tracks_out2 <- data.frame(aname = dt$aname.x, trip_id = dt$trip_id,
                               trip_start = dt$trip_start, zone = dt$zone,
                               gpstime = dt$time, adt = dt$adt, adtnum = dt$adtnum,
                                adtAP = dt$adtAP, ayear = dt$ayear, adate = dt$adate,
                               ele = dt$ele,
                               lon = dt$lon, lat = dt$lat, lon_lead = dt$lon_lead,
                               lat_lead = dt$lat_lead,
                                tdiff_clean = dt$tdiff_clean,
                               bear_deg_clean = dt$bear_deg_clean, vel = dt$vel,
                               dist_m_clean = dt$dist_m_clean, dist_m_census = dt$dist_m_census
  )

  all_tracks_summary_out <- data.frame(aname = all_tracks_summary$aname,
                                       trip_id = all_tracks_summary$trip_id,
                                       day_count = all_tracks_summary$day_count,
                                       ayear = all_tracks_summary$ayear,
                                       anomes = all_tracks_summary$anomes,
                                       adate = all_tracks_summary$adate,
                                       zone = all_tracks_summary$zone,
                                       transect = all_tracks_summary$transect,
                                       Day = all_tracks_summary$Day,
                                       trip_start = all_tracks_summary$trip_start,
                                       trip_end = all_tracks_summary$trip_end,
                                       trip_time_sec = all_tracks_summary$trip_time_sec,
                                       trip_time_hr = all_tracks_summary$trip_time_hr,
                                       speed_mean = all_tracks_summary$speed_mean,
                                       V_min = all_tracks_summary$V_min,
                                       V_max = all_tracks_summary$V_max,
                                       V_sd = all_tracks_summary$V_sd,
                                       dist_tot_km = all_tracks_summary$dist_tot_km,
                                       dist_tot_census_km = all_tracks_summary$dist_tot_census_km)

  list_track <- list(track_summary = all_tracks_summary_out,
                     tracks = df_tracks_out2)
  # 6) make waypoints
  if(do_waypoints!=FALSE){

    list_waypoints <- make_waypoints( folder_gpx = folder_gpx, river_shape = river_shape,
                                        folder_result = folder_result, track_data = list_track,
                                        date_start = date_start, date_end = date_end,
                                        make_shape_wp = make_shape_wp,
                                      way_csv = type_csv)
    }

  # 7) export tracks and summary
  fileend <- paste(format(Sys.time(),"%Y%m%d"), format(Sys.time(),"%H%M%S"), sep="_")
  if(type_csv == ".csv2"){
  # 7.1 tracks .csv
  setwd(folder_result)
  namet <- paste(paste("all_tracks", fileend, sep="_"),".csv",sep="")
  write.csv2(df_tracks_out2, namet)

  # 7.2 summary .csv
  namesum <- paste(paste("summary_all_tracks", fileend, sep="_"),".csv",sep="")
  write.csv2(all_tracks_summary_out, namesum)
  }else{
    # 7.1 tracks .csv
    setwd(folder_result)
    namet <- paste(paste("all_tracks", fileend, sep="_"),".csv",sep="")
    write.csv(df_tracks_out2, namet)
    
    # 7.2 summary .csv
    namesum <- paste(paste("summary_all_tracks", fileend, sep="_"),".csv",sep="")
    write.csv(all_tracks_summary_out, namesum)
    
  }


  #7.3 write shape
  if(make_shape!=FALSE){
    setwd(folder_result)
    dir.create(paste("shapes_tracks", fileend, sep="_"))
    newf <- paste(getwd(),paste("shapes_tracks", fileend, sep="_"), sep="/")
    setwd(newf)

    sppoint <- df_tracks_out2[, c('aname', 'trip_id', 'trip_start','zone','gpstime', 'adtAP',
                             'ayear', 'adate', 'ele','lon', 'lat', 'bear_deg_clean',
                             'vel', 'dist_m_clean', 'dist_m_census')]
    sp::coordinates(sppoint) <- ~lon+lat
    sp::proj4string(sppoint) <- sp::CRS("+init=epsg:4326")
    namepoint <- paste(paste("trackpoints", fileend, sep="_"),".shp",sep="")
    raster::shapefile(sppoint, filename = namepoint)
  }

  # 8) export as list
 return(list_track <- list(track_summary = all_tracks_summary_out,
                           tracks = df_tracks_out2))
}
