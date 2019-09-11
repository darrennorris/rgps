# rgps
Functions to import and summarise tracklogs and waypoints from .gpx files.

## Installation
Install development version from github.

1. install current devtools package from CRAN: `install.packages("devtools")` .

2. Use devtools to install the rgps package from github: `devtools::install_github("darrennorris/rgps")` .

3. load: `library("rgps")`

## Package example:
1) Need to specify 3 locations.

1.1) Location of shapefile with river zones
1.2) Location of folder with .gpx files censu
1.3) Location of folder for results
 
` rs <- system.file("shapes/riverzones.shp", package = "rgps")`

` fg <- system.file("gpx", package = "rgps")`

` fr <- "/Location/of/result/folder"`

2) run

` list_track_data <- rgps::make_tracks(`

`   folder_gpx = fg, folder_result = fr,  river_shape = rs,`

`   do_waypoints = TRUE, make_shape = FALSE,`

`  make_shape_wp = FALSE,`

`   type_csv = ".csv")`
