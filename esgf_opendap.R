## necessary packages
## library(ncdf4)
## library(ncdf4.helpers)
## library(xml2)
## library(rgdal)
## library(rgeos)
## library(sp)
## library(raster)
library(magrittr)


                                        # Read data from netcdf from OpenDAP link


## create credentials and cookies in .esg and .dodsrc files
## https://esgf.github.io/esgf-user-support/user_guide.html#access-data-with-the-command-line-via-opendap

nc <- ncdf4::nc_open("http://esgf-ictp.hpc.cineca.it/thredds/dodsC/esg_dataroot/cordex/output/EUR-11/ICTP/MOHC-HadGEM2-ES/historical/r1i1p1/ICTP-RegCM4-6/v1/day/huss/v20190502/huss_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_ICTP-RegCM4-6_v1_day_19700601-19701230.nc")
print(nc)

# get coordinates
lon <- ncdf4::ncvar_get(nc, "lon")
lat <- ncdf4::ncvar_get(nc, "lat")
x <- ncdf4::ncvar_get(nc, "x")
y <- ncdf4::ncvar_get(nc, "y")
time <- ncdf4.helpers::nc.get.time.series(nc)


xycoords <- expand.grid(x = x, y = y)
xycoords$Id <- seq.int(nrow(xycoords))
sp::coordinates(xycoords) <- ~ x + y
sp::proj4string(xycoords) <- sp::CRS(ncdf4::ncatt_get(nc, "crs")$"proj4_params")
grd_xycoords = as(xycoords, "SpatialPixels")


## read shape file of the Tech and the Lez
shape <- rgdal::readOGR("BassinsMed_ShapeFile", "bassins2")
## keep only Tech
tech <- subset(shape, Id == 151)
sp::proj4string(tech) <- sp::CRS("+proj=longlat +datum=WGS84")
tech_projected <- sp::spTransform(tech, sp::proj4string(xycoords))

sp::plot(tech_projected)
sp::plot(grd_xycoords, add = TRUE)

## coordinates of gridcells, the center of which is inside of the Tech
iinside <- sp::over(xycoords, tech_projected)
icoords <- which(!is.na(iinside))
icoords_array <- arrayInd(icoords, dim(lon))

sp::plot(xycoords[icoords, ], add = TRUE)
points(lon[iinside == 151], lat[iinside == 151])


## coordinates of gridcells intersercting the Tech
tol <- 3
xind <-
    xycoords$x >= x[max(min(icoords_array[, 1]) - tol, 0 )] &
    xycoords$x <= x[min(max(icoords_array[, 1]) + tol, length(x))]
yind <-
    xycoords$y >= x[max(min(icoords_array[, 2]) - tol, 0 )] &
    xycoords$y <= x[min(max(icoords_array[, 2]) + tol, length(y))]

xysubset <- xycoords[xind & yind, ]
xysubset <- as(xysubset, "SpatialPixelsDataFrame")
xysubset <- as(xysubset, "SpatialPolygonsDataFrame")

iintersect <- raster::intersect(tech_projected, xysubset)
areas <- sapply(seq.int(nrow(iintersect)), function(i) raster::area(iintersect[i, ]))
iintersect$area <- areas
icoords_array <- arrayInd(iintersect@data[, "Id.2"], dim(lon))

sp::plot(iintersect, col = rev(heat.colors(5))[cut(areas, 5)])


## extract only data points related to the Tech
datapoints <- apply(
    icoords_array,
    1,
    function(coord) ncdf4::ncvar_get(nc, varid = "huss", start = c(coord[1], coord[2], 1), count = c(1, 1, -1))
)

ncdf4::nc_close(nc)



                                        # Parse XML from ESGF Searh


## source functions used to parse esgf requests
source("esgf_opendap_algo.R")

## show how to navigate this xml document web browser
esgf_search <- xml2::read_xml("https://esg-dn1.nsc.liu.se/esg-search/search/?offset=0&limit=100&type=Dataset&replica=false&latest=true&domain=EUR-11&rcm_name=RegCM4-6&project=CORDEX&time_frequency=day&experiment=historical%2Crcp45%2Crcp85&driving_model=MOHC-HadGEM2-ES&variable=huss%2Cpr%2CsfcWind%2Ctas%2Ctasmax%2Ctasmin&ensemble=r1i1p1&facets=project%2Cproduct%2Cdomain%2Cinstitute%2Cdriving_model%2Cexperiment%2Cexperiment_family%2Censemble%2Crcm_name%2Crcm_version%2Ctime_frequency%2Cvariable%2Cvariable_long_name%2Ccf_standard_name%2Cdata_node&format=application%2Fsolr%2Bxml")
#!esgf_search <- read_xml("esg-dn1.nsc.liu.se.xml")


## show how to navigate this xml document web browser
doc <- xml2::xml_child(esgf_search, "result") %>% xml2::xml_child(search =  1)

## retrieve opendap urls 
opendap_urls <- get_opendap_url(doc)

## read data for variable "huss"
## only for grid points the center of which is in the Tech
df <- load_data_from_opendap(
    opendap_urls,
    "huss",
    shape = subset(shape, Id == 151)
)

## read all data from esgf-search xml files
## only for grid points the center of which is in the Tech
dat <- load_data_from_opendap_in_xml(
    esgf_search,
    shape = subset(shape, Id == 151)
)
