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
#'   \item "Local Authorities", "County Councils", "la" OR "cc"
#'   \item "Local Authorities 2016", "County Councils 2016", "la2016" OR "cc2016"
#'   \item "Constituencies" OR "Constituencies (2017)"  OR "con"
#'   \item "Constituencies_2013" OR "Constituencies (2013)"
#'   \item "Electoral Divisions" OR "elec_div" OR "ed"
#'   \item "Gaeltacht" OR "g"
#'   \item "Language Planning Areas" OR "lpa"
#'   \item "Local Electoral Areas (2019)" , "lea_2019" , "lea (2019)" , "Local Electoral Areas" OR "lea"
#'   \item "Local Electoral Areas (2014)" OR "lea_2014" OR "lea (2014)"
#'   \item "NUTS3" OR "nuts3"
#'   \item "Provinces" OR "p"
#'   \item "Settlements" OR "s"
#'   \item "Small Areas" OR "sa"
#'   
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
  
  # Set shapefile name ------------------
  fname <- dplyr::case_when(
    map_data == "Local Authorities 2016" || map_data == "County Councils 2016" ||
      map_data == "la2016"|| map_data == "cc2016" ~
      "Census2016_Local_Authorities",
    map_data == "Constituencies"|| map_data == "Constituencies (2017)" 
    || map_data == "con" ~
      "2017_Constituencies",
    map_data == "Constituencies_2013"|| map_data == "Constituencies (2013)" ~
      "2013_Constituencies",
    map_data == "Electoral Divisions" ||
      map_data == "elec_div" || map_data == "ed" ~
      "Census2016_Electoral_Divisions_generalised20m",
    map_data == "Gaeltacht" || map_data == "g" ~
      "Census2016_Gaeltacht",
    map_data == "Language Planning Areas" || map_data == "lpa" ~
      "Census2016_LPA",
    map_data == "Local Electoral Areas (2014)" ||
      map_data == "lea_2014" || map_data == "lea (2014)" ~
      "2014_Electoral_Areas",
    map_data == "Local Electoral Areas (2019)" ||
      map_data == "lea_2019" || map_data == "lea (2019)" ||
      map_data == "Local Electoral Areas" || map_data == "lea"  ~
      "2019_Electoral_Areas",
    map_data == "NUTS3" || map_data == "nuts3" ~ 
      "Census2016_NUTS3_generalised20m",
    map_data == "Provinces" || map_data == "p" ~
      "Census2016_Province_generalised20m",
    map_data == "Settlements" || map_data == "s" ~
      "Census2016_Settlements_generalised20m",
    map_data == "Small Areas" || map_data == "sa" ~
      "Census2016_Small_Areas_generalised20m",
    map_data == "Local Authorities" || map_data == "County Councils" ||
      map_data == "la"|| map_data == "cc" ~
      "2019_Local_Authorities",
    TRUE ~ NA_character_
  )
  url <- dplyr::case_when(
    fname == "Census2016_Local_Authorities" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/e4d3585d979c15653c7317a18d73b511",
    fname == "2017_Constituencies" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/2b6b493d675f17c75de3d2a76e69ef34",
    fname == "2013_Constituencies" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/91d1bb9ad7b0af2b8ca4361f944c3f57",
    fname == "Census2016_Electoral_Divisions_generalised20m" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/ea4d7bf2683f1bbcafc8428c715235b6",
    fname == "Census2016_Gaeltacht" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/feba4375fbb00dc945abab0e4477141f",
    fname == "Census2016_LPA" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/9b504eb50b10e0087c2b4913ade4d10d",
    fname == "2014_Electoral_Areas" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/e34a94319c050ca52766e193036eecaa",
    fname == "2019_Electoral_Areas" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/f381d63507530cfc61df96fa5f766e31",
    fname == "Census2016_NUTS3_generalised20m" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/57bf25130c4f5e8d086f314bbb98ef72",
    fname == "Census2016_Province_generalised20m" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/9f352336de5e2a0d42455237888478b7",
    fname == "Census2016_Settlements_generalised20m" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/1781a48b462bacb8eb860c716e24f609",
    fname == "Census2016_Small_Areas_generalised20m" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/a9c563c15cc611817af939f70d1d1f04",
    fname == "2019_Local_Authorities" ~
      "https://ws.cso.ie/public/api.static/PxStat.Data.GeoMap_API.Read/440c36d3b86e067e97ffb2fabf55900e",
    TRUE ~ NA_character_
  )  
  # Need to separate error check. Including it in case_when causes error ---
  if (is.na(fname)) {
    stop("Not one of the available map files.")
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
  filepath <- paste0(tmpdir, "/", fname, ".shp")
  
  
  # Error Messaging --------
  error_message =  paste0("Failed retrieving map data. Please check internet",
                          " connection and that cso.ie and opendata.arcgis.com are online")
  endfunc <- FALSE
  tryCatch({
    utils::download.file(url, filepath)
  }, warning = function(w) {
    message(paste0("Warning: ", error_message))
    endfunc <<- TRUE
  }, error = function(e) {
    message(paste0("Connection Error: ", error_message))
    endfunc <<- TRUE
  })
  if(endfunc){
    return(NULL)
  }
  if(!endfunc){
    
    
    shp <- sf::st_read(filepath, stringsAsFactors = F)
    
    
    'if (map_data == "NUTS2" | map_data == "NUTS3") {
      # Transform OSi maps to use Irish grid projection, like CSO maps
      ire_proj = "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs"
      shp <- sf::st_transform(shp, ire_proj)
    }'
    
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
  avg_area <- avg_area <- mean(sf::st_area(shp[sf::st_is_valid(shp),]))

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

