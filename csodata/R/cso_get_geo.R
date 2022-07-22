#' Return geographic data as a sf data frame
#'
#' Retrieves an ESRI shapefile of vector data for Ireland from the cso website
#' \href{https://www.cso.ie/en/census/census2011boundaryfiles/}{cso.ie} and
#' returns it as an sf data frame. The data is returned as a zip file, which is
#' downloaded to and unzipped in a temporary directory.
#'
#' The map data is from the 2011 census, and is 20m generalised, which offers a
#' good balance of fidelity and low file size. More datasets, as well as
#' 50m generalised, 100m generalised and ungeneralised versions of the map
#' files can also be found on the OSi (Ordnance Survey Ireland) website at
#' \url{https://data-osi.opendata.arcgis.com/search?tags=boundaries}.
#'
#' The NUTS2 and NUTS3 map files are the updated versions for 2016, including
#' three NUTS2 regions and the movement of Louth and South Tipperary into new
#' NUTS3 regions. These files are downloaded directly from the OSi website, as
#' they are not available on the CSO website, and do not contain the population
#' and housing data contained in the map files from the CSO website.
#'
#' @param map_data string. Indicates which shapefile to download. Options are:
#' \itemize{
#'   \item "Provinces" OR "p",
#'   \item "NUTS2",
#'   \item "NUTS3",
#'   \item "NUTS2_2011",
#'   \item "NUTS3_2011",
#'   \item "Administrative Counties" OR "admin_counties" OR "ac",
#'   \item "Electoral Divisions" OR "elec_div" OR "ed",
#'   \item "Small Areas" OR "sa" and
#'   \item "Gaeltacht" OR "g".
#' }
#' Until v0.1.5 "NUTS2" and "NUTS3" gave access to the 2011 dataset.
#' @param cache logical. Indicates whether to cache the result using R.cache.
#' TRUE by default.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for old, unused
#' files. Any files which have not been accessed in the last month will be deleted
#' @return data frame of the requested CSO table.
#' @export
#' @examples
#' \dontrun{
#' shp <- cso_get_geo("NUTS2")
#' }

cso_get_geo <- function(map_data, cache = TRUE, flush_cache = TRUE) {
  dl_path <- "https://census.cso.ie/censusasp/saps/boundaries/"
  # Set shapefile name ------------------
  fname <- dplyr::case_when(
    map_data == "Provinces" || map_data == "p" ~
      "Census2011_Province_generalised20m",
    map_data == "NUTS2" ~ "NUTS2_Boundaries_Generalised_20m__OSi_National_Statistical_Boundaries__2015",
    map_data == "NUTS3" ~ "NUTS3_Boundaries_Generalised_20m__OSi_National_Statistical_Boundaries__2015",
    map_data == "NUTS2_2011" ~ "Census2011_NUTS2_generalised20m",
    map_data == "NUTS3_2011" ~ "Census2011_NUTS3_generalised20m",
    map_data == "Administrative Counties" ||
      map_data == "admin_counties" || map_data == "ac" ~
      "Census2011_Admin_Counties_generalised20m",
    map_data == "Electoral Divisions" ||
      map_data == "elec_div" || map_data == "ed" ~
      "Census2011_Electoral_Divisions_generalised20m",
    map_data == "Small Areas" || map_data == "sa" ~
      "Census2011_Small_Areas_generalised20m",
    map_data == "Gaeltacht" || map_data == "g" ~
      "Census2011_Gaeltacht",
    TRUE ~ NA_character_
  )
  
  # Need to separate error check. Including it in case_when causes error ---
  if (is.na(fname)) {
    stop("Not one of the available map files.")
  }
  
  if (map_data == "NUTS2") {
    url <- "https://opendata.arcgis.com/datasets/62e0cf326bab442897944e4dc4999c16_2.zip"
  } else if (map_data == "NUTS3") {
    url <- "https://opendata.arcgis.com/datasets/1a5d91a11ad9454d865ad426bbf5bc37_2.zip"
  } else {
    url <- paste0(dl_path, fname, ".zip")
  }
  
  # Retreive cached data ----------------
  if (cache) {
    data <- R.cache::loadCache(list(fname), dirs = "csodata/geodata")
    if (!is.null(data)) {
      message("Loaded cached data\n")
      return(data)
    }
  }
  
  #Empty out the cache of unused files if a new file is being downloaded
  if(flush_cache){
    file.remove(
      rownames(
        fileSnapshot(paste0(R.cache::getCacheRootPath(),"/csodata"), full.names = T, recursive = T)$info[!lubridate::`%within%`(
          fileSnapshot(paste0(R.cache::getCacheRootPath(),"/csodata"), full.names = T, recursive = T)$info[,"mtime"],
          lubridate::interval(start = Sys.Date()- lubridate::days(2) , end = Sys.Date() + lubridate::days(1))) , ]
      )
    )
  }
  
  # No caching, or cache empty ----------
  tmpdir <- tempdir()
  filepath <- paste0(tmpdir, "/", fname, ".zip")
  
  
  # Error Messaging --------
  error_message =  paste0("Failed retrieving map data. Please check internet",
                          " connection and that cso.ie and opendata.arcgis.com are online")
  endfunc <- FALSE
  tryCatch({
    utils::download.file(url, filepath)
  }, warning = function(w) {
    message(paste0("Warning: ", error_message))
    endfunc <<- TRUE
    return(NULL)
  }, error = function(e) {
    message(paste0("Connection Error: ", error_message))
    endfunc <<- TRUE
    return(NULL)
  })
  
  if(!endfunc){
    tryCatch({
      utils::unzip(filepath, exdir = tmpdir)
    }, warning = function(w) {
      message(paste0("Warning: ", error_message))
      endfunc <<- TRUE
      return(NULL)
    }, error = function(e) {
      message(paste0("Connection Error: ", error_message))
      endfunc <<- TRUE
      return(NULL)
    })
  }
  
  if(!endfunc){
    
    
    '
  tryCatch({
    utils::unzip(filepath, exdir = tmpdir)
  }, warning = function(w) {
    message(paste0("Warning: ", error_message))
    return(NULL)
  }, error = function(e) {
    message(paste0("Connection Error: ", error_message))
    return(NULL)
  })
  '
    utils::unzip(filepath, exdir = tmpdir)
    if (map_data == "NUTS2") {
      shape_file <- paste0(tmpdir, "/", "c2f2dbb3-289e-45cc-ae79-791cbc9339632020330-1-1uh3380.g89t.shp")
    } else if ( map_data == "NUTS3"){
      shape_file <- paste0(tmpdir, "/", "527c3332-32cc-44cd-baa3-267a0e917b5a2020328-1-1cpklcw.flb0h.shp")
    } else{
      shape_file <- paste0(tmpdir, "/", fname, ".shp")
    }
    '
  url <- "https://census.xcso.ie/censusasp/saps/boundaries/Census2011_NUTS3_generalised20m.zip"
  filepath <- paste0(tmpdir, "\\", "zoo")
  response <- tryCatch({
    httr::GET(url)
  }, warning = function(w) {
    print(paste0("Warning: ", error_message))
    return(NULL)
  }, error = function(e) {
    message(paste0("Connection Error: ", error_message))
    return(NULL)
  })
  '
    shp <- sf::st_read(shape_file, stringsAsFactors = F)
    
    
    if (map_data == "NUTS2" | map_data == "NUTS3") {
      # Transform OSi maps to use Irish grid projection, like CSO maps
      ire_proj = "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs"
      shp <- sf::st_transform(shp, ire_proj)
    }
    
    if (cache) {
      R.cache::saveCache(shp, key = list(fname), dirs = "csodata/geodata")
    }
    return(shp)
  }
}


#' Returns a data frame with the metadata of a vector shapefile
#'
#' Takes the output from \code{\link{cso_get_geo}} or otherwise and returns
#' information about it in a data frame.
#'
#' @param shp sf data.frame. Geographic data stored as an sf object.
#' @return list with eight elements:
#' \itemize{
#'   \item The coordinate reference system, itself a list with two elements,
#'   the EPSG code (if any, NA value if none), and the proj4string
#'   \item The number of polygons in the data
#'   \item If all the polygons are simple (not self-intersecting)
#'   \item If any polygons are empty
#'   \item If all of the polygons are valid
#'   \item The average area of the polygons, including units
#' }
#' @export
#' @examples
#' \dontrun{
#' shp_meta <- cso_get_geo_meta(shp)
#' }
cso_get_geo_meta <- function(shp) {
  crs <- sf::st_crs(shp)
  poly <- length(sf::st_dimension(shp))

  simp <- all(sf::st_is_simple(shp))
  empt <- any(sf::st_is_empty(shp))
  valid <- all(sf::st_is_valid(shp))
  avg_area <- mean(sf::st_area(shp))

  list(
    coordinate_reference_system = crs, polygons = poly, all_simple = simp,
    any_empty = empt, all_valid = valid, average_area = avg_area
  )
}


#' Prints metadata from an ESRI shapefile to console
#'
#' Takes the output from \code{\link{cso_get_geo}} or otherwise and prints
#' information about it to the console as formatted text.
#'
#' @param shp sf data.frame. Geographic data stored as an sf object.
#' @return Does not return any values, rather the function prints the shapefile metadata to console.
#' @export
#' @examples
#' \dontrun{
#' cso_disp_geo_meta(shp)
#' }
cso_disp_geo_meta <- function(shp) {
  contents <- setdiff(names(shp), "geometry")
  shp_meta <- cso_get_geo_meta(shp)
  #date_range <- range(as.POSIXlt(shp$CREATEDATE, format = "%d-%m-%Y", tz = "GMT"))

  message("*** GEOGRAPHIC METADATA ***")
  print(sf::st_crs(shp))
  message("\nPolygons = ", shp_meta$polygons, "\n")
  message("All polygons are simple? = ", shp_meta$all_simple, "\n")
  message("Any polygons is empty? = ", shp_meta$any_empty, "\n")
  message("All polygons are valid? = ", shp_meta$all_valid, "\n")
  #message("Creation date range = ", as.character(date_range[[1]]), "--", as.character(date_range[[2]]), "\n")
  message("Average polygon area = ")
  print(shp_meta$average_area)
  message("\n*** ADDITIONAL DATA INCLUDED ***")

  geoid <- c("NUTS1", "NUTS1NAME", "NUTS2", "NUTS2NAME", "NUTS3", "NUTS3NAME",
             "COUNTY", "COUNTYNAME", "GEOGID", "CSOED", "OSIED", "EDNAME",
             "GAELTACHT", "NAME", "PROVINCE", "PROVNAME", "SMALL_AREA")

  geoident <- contents[contents %in% geoid]

  message("\nGeographic identifiers:")
  print(geoident)

  pop <- contents[contents %in% c("MALE2011", "FEMALE2011", "TOTAL2011",
                                  "Male2011", "Female2011", "Total2011")]
  message("\nPopulation (Male, Female, Total):")
  print(pop)

  hs <- c("PPOCC2011", "UNOCC2011", "VACANT2011", "HS2011", "PCVAC20111",
          "PCVAC2011", "PPOcc2011", "Unocc2011", "Vacant2011", "PCVac2011")

  house <- contents[contents %in% hs]
  message("\nHousing figures:")
  print(house)

  area <- contents[contents %in% c("LAND_AREA", "TOTAL_AREA")]
  message("\nArea figures:")
  print(area)

  message("\nCreation/modification date:")
  print("CREATEDATE")

  other <- contents[!contents %in% c(geoident, pop, house, area, "CREATEDATE")]
  if (length(other) > 0) {
    message("Other data: \n")
    print(other)
  }
}
