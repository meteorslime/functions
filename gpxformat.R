## Function to separate tracks from GPX files by segment
GPXFormat <- function(path = path, file = file, observer = observer) {
#path <- "D:/Google Drive/GIS/GPX/"
#file <- "20191122.gpx"
#observer <- "RBC"

library(rgdal)
library(lubridate)

## Read in GPX file
file <- paste(path, file, sep = "")
trackpoints <- readOGR(file, layer = "track_points")

## Calculate local time
trackpoints$time <- ymd_hms(trackpoints$time, tz = "Pacific/Guam")

## Rename elevation field
names(trackpoints)[names(trackpoints) == "ele"] <- "elev"

## Add observer
trackpoints$observer <- observer

## Reduce fields
trackpoints$track_seg_id <- NULL
trackpoints$track_seg_point_id <- NULL
trackpoints$magvar <- NULL
trackpoints$geoidheight <- NULL
trackpoints$name <- NULL
trackpoints$cmt <- NULL
trackpoints$desc <- NULL
trackpoints$src <- NULL
trackpoints$link1_href <- NULL
trackpoints$link1_text <- NULL
trackpoints$link1_type <- NULL
trackpoints$link2_href <- NULL
trackpoints$link2_text <- NULL
trackpoints$link2_type <- NULL
trackpoints$sym <- NULL
trackpoints$type <- NULL
trackpoints$fix <- NULL
trackpoints$sat <- NULL
trackpoints$hdop <- NULL
trackpoints$vdop <- NULL
trackpoints$pdop <- NULL
trackpoints$ageofdgpsdata <- NULL
trackpoints$dgpsid <- NULL

## Rearrange fields
trackpoints <- trackpoints[c("track_fid", "observer", "time", "elev")]

##
for (i in 1:length(unique(trackpoints$track_fid))) {
    track <- trackpoints[trackpoints$track_fid == i-1, ]
    track$track_fid <- NULL
    writeOGR(
        track, 
        dsn = "D:/Google Drive/GIS/GPX/!Temp", 
        layer = paste(
            strftime(track$time[1], format = "%Y%m%d"), 
            "_", 
            strftime(track$time[1], format = "%H%M%S"), 
            sep = ""
        ), 
        driver = "ESRI Shapefile"
    )
}

## Manual subsetting
#i <- 1
#track <- trackpoints[trackpoints$track_fid == i-1, ]
}
