

#' @title Clean tracklogs imported via function \code{load_tracks}
#'
#' @description Cleans tracklog points. Calculates a variety of metrics
#' necessary for summaries of effort (date, time, distance, speed etc).
#' @param x Data.frame as returned by function \code{load_tracks} .
#'
#' @return Data.frame with cleaned .gpx tracklog points.
#'
#' @importFrom "stats" "na.omit" "sd"
#' @export
#'
#' @examples
#' \dontrun{
#' }
clean_tracks <- function(x){

  #remove null
  selNA <- which(is.na(x$time)==FALSE)
  dfc <- x[selNA, ]
  #remove bad lat/lon
  selNAl <- which(is.na(dfc$lat)==FALSE)
  dfc <- dfc[selNAl, ]
  selC <- which(dfc$lon > -200 & dfc$lat > -90)
  dfc1 <- dfc[selC, ]
  #remove duplicates
  dfc2 <- dfc1[!duplicated(dfc1$time), ]

  dfc2$ele <- as.numeric(dfc2$ele)
  dfc2$ayear <- stringr::str_sub(dfc2$time,1,4)
  dfc2$adate <- stringr::str_sub(dfc2$time,1,10)
  dfc2$atime <- stringr::str_sub(dfc2$time,-9,-2)
  dfc2$adatetime <- paste(dfc2$adate, dfc2$atime)
  dfc2$adt <- as.POSIXct(strptime(dfc2$adatetime,"%Y-%m-%d %H:%M:%S", tz="UTC"))
  dfc2$adtnum <- as.numeric(dfc2$adt)
  dfc2$adtAP <- format(dfc2$adt, tz="America/Belem",usetz=TRUE)

  # remove failed time
  selT <- which(is.na(dfc2$adtAP)==FALSE)
  dfc2 <- dfc2[selT, ]

  #select only year with census
  selYear <- which(dfc2$ayear %in% c("2015", "2016"))
  dfc2 <- dfc2[selYear, ]

  #sort
  dfc3 <- dfc2[order(dfc2$adtnum), ]

  if (nrow(dfc3)<2){
    df2 <- data.frame(
      adt = NA, aname = x$aname[1], lon = NA, lat = NA, ele = NA, time = NA, ayear = NA,
      adate = NA, atime = NA, adatetime = NA, adtnum = NA, adtAP = NA, tdiff = NA, trip_end_flag = NA,
      trip_start_flag = NA, seg_id = NA, tdiff_clean = NA, lon_lead = NA, lat_lead = NA,
      dist_m = NA, dist_m_clean = NA, bear_deg = NA, bear_deg_clean = NA,
      vel = NA, dist_m_census = NA
    )
  } else {
    dfc3$tdiff <- dplyr::lead(dfc3$adt) - dfc3$adt
    dfc3$trip_end_flag <- ifelse(dfc3$tdiff > 60, 1,NA)
    dfc3$trip_start_flag <- dplyr::lag(dfc3$trip_end_flag)
    dfc3[1,'trip_start_flag'] <- 1
    dfc3[nrow(dfc3),'trip_end_flag'] <- 1

    tstart<- na.omit(
      as.POSIXct((dfc3$trip_start_flag*as.numeric(dfc3$adt)),
                 origin="1970-01-01 00:00.00 UTC", tz="UTC")
    )

    tend <- na.omit(
      as.POSIXct((dfc3$trip_end_flag*as.numeric(dfc3$adt)),
                 origin="1970-01-01 00:00.00 UTC", tz="UTC")
    )

    dft <- data.frame(seg_id = rank(tstart, ties.method= "first"),
                      adt_start = tstart, adt_end = tend)

    mseq <- function(x){
      dfout <- data.frame(sadt = seq(x$adt_start, x$adt_end, by="sec"))
    }

    dft2 <- plyr::ddply(dft, ("seg_id"), .fun=mseq)
    gc()
    # all values agree with Garmin bascamp
    #time
    df2 <- merge(dfc3,dft2, by.x = "adt",by.y = "sadt", all.x=TRUE)
    df2$tdiff_clean <- df2$tdiff
    df2[which(df2$trip_end_flag == 1), 'tdiff_clean'] <- NA

    # distance
    df2$lon_lead <- dplyr::lead(df2$lon)
    df2$lat_lead <- dplyr::lead(df2$lat)

    df2$dist_m <- geosphere::distHaversine(as.matrix(df2[,c('lon', 'lat')]),
                                as.matrix(df2[,c('lon_lead', 'lat_lead')])
    )
    df2$dist_m_clean <- df2$dist_m
    df2[which(df2$trip_end_flag == 1), 'dist_m_clean'] <- NA

    #bearing
    df2$bear_deg <- geosphere::bearing(as.matrix(df2[,c('lon', 'lat')]),
                            as.matrix(df2[,c('lon_lead', 'lat_lead')])
    )

    df2$bear_deg_clean <- df2$bear_deg
    df2[which(df2$trip_end_flag == 1), 'bear_deg_clean'] <- NA
    df2$bear_deg_clean <- ifelse(df2$bear_deg_clean < 0, 360 + df2$bear_deg_clean,
                                 df2$bear_deg_clean)
    #speed km/h
    df2$vel <- (df2$dist_m_clean / as.numeric(df2$tdiff_clean))*3.6
    df2[which(df2$vel  > 30), 'vel'] <- NA

    # exclude when too slow or too fast
    df2$dist_m_census <- df2$dist_m_clean
    df2[which(df2$vel < 1 | df2$vel > 16), 'dist_m_census'] <- NA
  }
  dffinal <- df2
}
