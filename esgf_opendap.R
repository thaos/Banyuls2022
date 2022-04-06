# create credentials and cookies in .esg
# https://esgf.github.io/esgf-user-support/user_guide.html#access-data-with-the-command-line-via-opendap
library(ncdf4)
library(ncdf4.helpers)
# test
nc_open("http://esgf-ictp.hpc.cineca.it/thredds/dodsC/esg_dataroot/cordex/output/SAM-22/ICTP/ECMWF-ERAINT/evaluation/r1i1p1/ICTP-RegCM4-7/v0/fx/areacella/v20190502/areacella_SAM-22_ECMWF-ERAINT_evaluation_r1i1p1_ICTP-RegCM4-7_v0_fx.nc")

library(rjson)
library(xml2)
library(rgdal)
library(rgeos)
library(sp)
# library(XML)


esgf_search <- fromJSON(file = "https://esg-dn1.nsc.liu.se/esg-search/search/?offset=0&limit=100&type=Dataset&replica=false&latest=true&domain=EUR-11&rcm_name=RegCM4-6&project=CORDEX&time_frequency=day&experiment=historical%2Crcp45%2Crcp85&driving_model=MOHC-HadGEM2-ES&variable=huss%2Cpr%2CsfcWind%2Ctas%2Ctasmax%2Ctasmin&ensemble=r1i1p1&facets=project%2Cproduct%2Cdomain%2Cinstitute%2Cdriving_model%2Cexperiment%2Cexperiment_family%2Censemble%2Crcm_name%2Crcm_version%2Ctime_frequency%2Cvariable%2Cvariable_long_name%2Ccf_standard_name%2Cdata_node&format=application%2Fsolr%2Bjson")

esgf_search <- read_xml("https://esg-dn1.nsc.liu.se/esg-search/search/?offset=0&limit=100&type=Dataset&replica=false&latest=true&domain=EUR-11&rcm_name=RegCM4-6&project=CORDEX&time_frequency=day&experiment=historical%2Crcp45%2Crcp85&driving_model=MOHC-HadGEM2-ES&variable=huss%2Cpr%2CsfcWind%2Ctas%2Ctasmax%2Ctasmin&ensemble=r1i1p1&facets=project%2Cproduct%2Cdomain%2Cinstitute%2Cdriving_model%2Cexperiment%2Cexperiment_family%2Censemble%2Crcm_name%2Crcm_version%2Ctime_frequency%2Cvariable%2Cvariable_long_name%2Ccf_standard_name%2Cdata_node&format=application%2Fsolr%2Bxml")
# esgf_search <- read_xml("esg-dn1.nsc.liu.se.xml")

xml_name(esgf_search)
xml_children(esgf_search)[2] |> xml_child(search =  1)

doc <- xml_child(esgf_search, "result") |> xml_child(search =  1)

get_metadata <- function(doc){
  metadata <- xml_find_all(doc, "arr")
  metadata <- metadata[
    xml_attr(metadata, "name") %in% 
      c(
        "variable",
        "domain",
        "driving_model",
        "ensemble",
        "experiment",
        "rcm_name",
        "time_frequency"
      )
  ]
  metatext <- xml_text(metadata)
  names(metatext) <- xml_attr(metadata, "name")
  return(metatext)
}


get_opendap_url <- function(doc){
  data_node <- xml_child(
    doc,
    search = which(
      xml_attr(xml_children(doc), attr = "name") == "data_node"
    )
  ) |>
    xml_text()
  url_thredd <-  xml_child(
    doc,
    search = which(xml_attr(xml_children(doc), attr = "name") == "url")
  ) |> xml_text()
  # url_thredd <- "cordex.output.EUR-11.ICTP.MOHC-HadGEM2-ES.historical.r1i1p1.RegCM4-6.v1.day.huss.v20190502.xml"
  thredd <- read_xml(url_thredd)  |>
    xml_ns_strip()
  opendap <- xml_child(
    thredd,
    search = which(
      xml_attr(xml_children(thredd), attr = "serviceType") == "OpenDAP"
    )
  )
  base <- xml_attr(opendap, attr = "base")
  datasets <- xml_child(thredd, search = "dataset") |>
    xml_find_all(xpath = "dataset")
  fnames <- xml_attr(datasets, "name")
  iagg <- grepl("aggregation", fnames)
  fnames <- fnames[!iagg]
  datasets <- datasets[!iagg]
  opendap_urls <- xml_find_all(datasets, "access")
  opendap_urls <- opendap_urls[
    xml_attr(opendap_urls, "serviceName") == "OpenDAPServer"
  ] |>
    xml_attr(attr = "urlPath")
  opendap_urls <- paste0("https://", data_node, base, opendap_urls)
  return(opendap_urls)
}

opendap_urls <- get_opendap_url(doc)
nc <- nc_open(opendap_urls[1])
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

coords <- data.frame(lon = c(lon), lat = c(lat))
coordinates(coords) <- ~ lon + lat
nc_close(nc)

shape <- readOGR("BassinsMed_ShapeFile", "bassins2")
iinside <- over(coords, shape)
icoords <- arrayInd(which(iinside == 151), dim(lon))
points(lon[iinside == 151], lat[iinside == 151])

datapoints <- apply(icoords, 1, function(coord) ncvar_get(nc, varid = "huss", start = c(coord[1], coord[2], 1), count = c(1, 1, -1)))



load_data_from_opendap <- function (
    opendap_urls,
    varname,
    shape
){
  for(i in seq_along(opendap_urls)){
    df <- data.frame()
    nc <- nc_open(opendap_urls[i])
    if(i == 1){
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      coords <- data.frame(lon = c(lon), lat = c(lat))
      coordinates(coords) <- ~ lon + lat
      iinside <- over(coords, shape)
      iinside <- which(!is.na(iinside))
      icoords <- arrayInd(iinside, dim(lon))
      attr(df, "lon") <- lon[iinside]
      attr(df, "lat") <- lat[iinside]   
    }
    datapoints <- apply(icoords, 1, function(coord) ncvar_get(nc, varid = varname, start = c(coord[1], coord[2], 1), count = c(1, 1, -1)))
    colnames(datapoint) <- varname
    time <- ncdf4.helpers::nc.get.time.series(nc)
    nc_close(nc)
    df <- rbind(df, cbind(time = time, datapoints))
  }
  return(df[order(df$time),])
}

load_data_from_doc <- function (
    doc,
    shape
){
  metadata <- get_metadata(doc)
  opendap_urls <- get_opendap_url(doc)
  df <- load_data_from_opendap(doc, metadata["variable"], shape = shape)
  attr(df, "metadata") <- metadata
  return(df)
}

load_data_from_opendap_in_xml <- function (
    esgf_search,
    shape =  sp::Polygon(
      matrix(
        c(0, 90,
          360, 90,
          360, -90,
          0, -90),
        byrow = TRUE
      )
    )
){
  docs <- xml_child(esgf_search, "result") 
  return(lapply(docs, load_data_from_doc, shape = shape))
}
load_data_from_opendap_in_xml(
    esgf_search,
    shape = shape@polygons[1]
)
