## xml of esgf search for cordex data
## lst responseHeader / result response / lst facet_counts
## result response: doc1 / doc2 / doc3 / ...
## where a doc contains information about a dataset defined by variable, experiment, gcm and rcm
## among those pieces of information, the node with name "url" points to another xml document
## which contains the link to access the netcdf files through opendap



get_metadata <- function(doc){
  metadata <- xml2::xml_find_all(doc, "arr")
  metadata <- metadata[
    xml2::xml_attr(metadata, "name") %in% 
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
  metatext <- xml2::xml_text(metadata)
  names(metatext) <- xml2::xml_attr(metadata, "name")
  return(metatext)
}


get_opendap_url <- function(doc){
  data_node <- xml2::xml_child(
    doc,
    search = which(
      xml2::xml_attr(xml2::xml_children(doc), attr = "name") == "data_node"
    )
  ) |>
    xml2::xml_text()
  url_thredd <- xml2::xml_child(
    doc,
    search = which(xml2::xml_attr(xml2::xml_children(doc), attr = "name") == "url")
  ) |> xml2::xml_text()
  # url_thredd <- "cordex.output.EUR-11.ICTP.MOHC-HadGEM2-ES.historical.r1i1p1.RegCM4-6.v1.day.huss.v20190502.xml"
  thredd <- xml2::read_xml(url_thredd)  |>
    xml2::xml_ns_strip()
  opendap <- xml2::xml_child(
    thredd,
    search = which(
      xml2::xml_attr(xml_children(thredd), attr = "serviceType") == "OpenDAP"
    )
  )
  base <- xml2::xml_attr(opendap, attr = "base")
  datasets <- xml2::xml_child(thredd, search = "dataset") |>
    xml2::xml_find_all(xpath = "dataset")
  fnames <- xml2::xml_attr(datasets, "name")
  iagg <- grepl("aggregation", fnames)
  fnames <- fnames[!iagg]
  datasets <- datasets[!iagg]
  opendap_urls <- xml2::xml_find_all(datasets, "access")
  opendap_urls <- opendap_urls[
    xml2::xml_attr(opendap_urls, "serviceName") == "OpenDAPServer"
  ] |>
    xml2::xml_attr(attr = "urlPath")
  opendap_urls <- paste0("http://", data_node, base, opendap_urls)
  return(opendap_urls)
}


load_data_from_opendap <- function (
    opendap_urls,
    varname,
    shape
){
  df <- data.frame()  
  for(i in seq_along(opendap_urls)){
    nc <- nc_open(opendap_urls[i])
    if(i == 1){
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      coords <- data.frame(lon = c(lon), lat = c(lat))
      sp::coordinates(coords) <- ~ lon + lat
      iinside <- sp::over(coords, shape)
      iinside <- which(!is.na(iinside))
      icoords <- arrayInd(iinside, dim(lon))
      cal <- attr(ncdf4.helpers::nc.get.time.series(nc), "cal")
    }
    datapoints <- apply(
        icoords,
        1,
        function(coord) {
            ncdf4::ncvar_get(
                nc,
                varid = varname,
                start = c(coord[1], coord[2], 1),
                count = c(1, 1, -1)
            )
        }
    )
    colnames(datapoints) <- rep(varname, ncol(datapoints))
    time <- ncdf4.helpers::nc.get.time.series(nc)
    print(str(time))
    nc_close(nc)
    df <- rbind(df, cbind(time = time, datapoints))
    # print(dim(df))
    print(str(df))
  }
  attr(df, "lon") <- lon[iinside]
  attr(df, "lat") <- lat[iinside]
  attr(df, "cal") <- cal
  return(df[order(df$time),])
}

load_data_from_doc <- function (
    doc,
    shape
){
  metadata <- get_metadata(doc)
  opendap_urls <- get_opendap_url(doc)
  df <- load_data_from_opendap(opendap_urls, metadata["variable"], shape = shape)
  attr(df, "metadata") <- metadata
  return(df)
}

load_data_from_opendap_in_xml <- function (
    esgf_search,
    shape = sp::SpatialPolygons(
	list(sp::Polygon(
		matrix(
                    c(0, 90,
                      360, 90,
                      360, -90,
                      0, -90),
                    byrow = TRUE
                )
        ))
   )
){
    docs <- xml2::xml_child(esgf_search, "result") |>
    	xml2::xml_children()  
    return(lapply(docs, load_data_from_doc, shape = shape))
}
