#' Return a CSO table as a data frame
#'
#' Returns a CSO table from the CSO PxStat Application Programming Interface
#' (API) as a data frame, with the option to give it in wide format (default)
#' very wide or long format.
#'
#' The data is pulled from the ResponseInstance service on the CSO API in
#' JSON-Stat format, using the GET method from the httr package.
#'
#' @param table_code string. If the table_code is a filename or a path to a
#' file, e.g. "QNQ22.json", it is imported from that file. Otherwise if it is
#' only a table code e.g. "QNQ22", the file is downloaded from data.cso and
#' checked to see if it is a valid table.
#' @param pivot_format string, one of "wide", "very_wide", "tall" or "tidy. If "wide"
#' (default) the table is returned in wide (human readable) format, with
#' statistic as a column (if it exists). If "very_wide" the table is returned
#' wide format and spreads the statistic column to rows. If "tall" the table is
#' returned in tall (statistic and value) format.If "tidy" will be returned in a tidy-like format.
#' @param include_ids logical. The JSON-stat format stores variables as ids
#' i.e. IE11 and labels i.e. Border. While the label is generally preferred,
#' sometimes it is useful to have the ids to match on. If \code{include_ids}
#' is TRUE (default) then ids are retrieved and appended to the table to the
#' right of the original column with the name <columnName>.id.
#' @param id_list either NULL (default) or a character vector of columns that
#' should have ids appended if include_ids is TRUE.
#' if NULL then every column that is not included in the vector
#' \code{remove_id} will be used.
#' @param use_factors logical. If TRUE (default) factors will be used in
#' strings.
#' @param use_dates logical. If True dates will be returned as date-time competent.
#' Default is FALSE.
#' @param cache logical. if TRUE (default) csodata will cache the result using
#' R.cache. The raw data downloaded from the data.csi.ie is cached, which means
#' that calling \code{cso_get_data} with the same table_code but different
#' parameterswill result in cached data being used.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for 
#' old, unused files. Any files which have not been accessed in the last month
#'  will be deleted.
#' @param wide_format string. Deprecated argument as of 1.4.0. Please use pivot_format instead.
#' @export
#' @examples
#' \dontrun{
#' tbl1 <- cso_get_data("QNQ22")
#' tbl2 <- cso_get_data("QLF07.json")
#' }
cso_get_data <- function(table_code,pivot_format = "wide", wide_format = lifecycle::deprecated(), include_ids = FALSE,
                         id_list = NULL, use_factors = TRUE,use_dates = FALSE, cache = TRUE, flush_cache = TRUE) {
  
  table_code <- toupper(table_code)
  
  #check if user supplied "wide_format" instead of "pivot_format"
  if (lifecycle::is_present(wide_format)) {
    
    # Signal the deprecation to the user
    lifecycle::deprecate_warn("1.3.1", "csodata::cso_get_data(wide_format = )", "csodata::cso_get_data(pivot_format = )")
    
    # Deal with the deprecated argument for compatibility
    pivot_format <- wide_format
  }
  
  #Setting a default for value and statistic to avoid notes during check
  Statistic=value=STATISTIC=Value=NULL
  
  # Set path to or download data --------
  if (substr(
    table_code, nchar(table_code) - 4, nchar(table_code)
  ) == ".json") {
    if (file.exists(table_code)) {
      json_data <- table_code
    } else {
      stop("Not a valid path to a .json file")
    }
  } else {
    json_data <- cso_download_tbl(table_code, cache = cache, flush_cache = flush_cache)
    # Error Checking ----------------------
    if (is.null(json_data)) {
      return(NULL)
    }
  }
  
  # Load data ---------------------------
  data <- rjstat::fromJSONstat(json_data,
                               naming = "label", use_factors = use_factors
  )
  
  names(data) <- make.names(names(data), unique = TRUE)
  
  # Append ids as new column ------------
  if (include_ids) {
    data_id <- rjstat::fromJSONstat(parse(json_data)$result,
                                    naming = "id", use_factors = use_factors
    )
    names(data_id)[substr(names(data_id),0,1) == "C"] <-paste0(names(data)[substr(names(data_id),0,1) == "C"],".id")
    
    #Composing the list of id columns to be appended
    if (is.null(id_list)) {
      
      concat_id <- names(data_id)[grepl(".*id$",names(data_id))]
    } else if (all(id_list %in% names(data))){
      concat_id <- paste0(id_list,".id")
    } else {
      #There was an issue with the id_list
      stop("One or more columns supplied to id_list not found")
    }
    
    for (ID in concat_id) {
      id_list <- list(as.vector(as.matrix(data_id[ID])))
      names(id_list) <- ID
      data <- data.frame(append(data, id_list, after = match(substr(ID,0,nchar(ID)-3), names(data)) ),
                         stringsAsFactors = TRUE
      )
    }
  }
  
  #Changing Month from String to Date format
  if (use_dates == TRUE){
    if("Month" %in% names(data)){
      data$Month <- lubridate::ym(data$Month)
    }
    if("Quarter" %in% names(data)){
      data$Year <- lubridate::yq(data$Quarter)
    }
  }
  
  
  #Pivot to Tidy Data
  if (pivot_format == "tidy"){
    
    if ("STATISTIC" %in% names(data)){
      data <- dplyr::rename(data, Statistic = STATISTIC)
    }
    
    if ("value" %in% names(data)){
      data <- dplyr::rename(data, Value = value)
    }
    
    data <- tidyr::pivot_wider(data, names_from = Statistic ,values_from = value)
    
  }
  
  # Pivot to wide table -----------------
  if (pivot_format == "wide" || pivot_format == "very_wide") {
    string <- names(data)
    remove <- c(
      "Year", "Quarter", "Month", "value", "CensusYear",
      "Census.Year", "HalfYear", "Intercensal.Period"
    )
    
    if (pivot_format == "very_wide") {
      remove <- append(remove, "Statistic")
    }
    
    row_vars <- string [!string %in% remove]
    
    wide_data <- reshape2::dcast(data, formula = paste(
      paste(row_vars, collapse = " + "), " ~ ... "
    ))
    
    if (!use_factors) {
      wide_data <- dplyr::mutate_if(wide_data, is.factor, as.character)
    }
    
    return(wide_data)
  } else {
    return(data)
  }
}

#' Download a CSO table as a data frame
#'
#' Internal function to return a CSO table from the CSO PxStat Application
#' Programming Interface (API) as a JSON-stat dataset.
#'
#' The data is pulled from the ReadDataset service on the CSO API in
#' JSON-Stat format, using the GET method from the httr package.
#'
#' To improve performance, the result is cached by default.
#'
#' @param table_code string. The code uniquely identifying one table.
#' @param cache logical. Indicates whether to cache the result using R.cache.
#' @param suppress_messages logical. If FALSE (default) a message is printed
#' when loading a previously cached data table.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for 
#' old, unused files. Any files which have not been accessed in the last month 
#' will be deleted
#' @return a character object, containing the data in JSON-stat format.
#' @noRd
#' @importFrom utils fileSnapshot


cso_download_tbl <- function(table_code, cache = TRUE,
                             suppress_messages = FALSE, flush_cache = TRUE) {
  table_code <- toupper(table_code)
  url <- paste0("https://ws.cso.ie/public/api.jsonrpc?data=%7B%22jsonrpc%22:%222.0%22,%22method%22:%22PxStat.Data.Cube_API.ReadDataset%22,%22params%22:%7B%22class%22:%22query%22,%22id%22:%5B%5D,%22dimension%22:%7B%7D,%22extension%22:%7B%22pivot%22:null,%22codes%22:false,%22language%22:%7B%22code%22:%22en%22%7D,%22format%22:%7B%22type%22:%22JSON-stat%22,%22version%22:%222.0%22%7D,%22matrix%22:%22",
                        table_code,"%22%7D,%22version%22:%222.0%22%7D%7D"
  )
  
  # Attempt to retrieve cached data -----
  if (cache) {
    toc <- cso_get_toc(suppress_messages = TRUE, cache = FALSE, from_date = NULL,flush_cache = FALSE)
    last_update <- toc[toc$id == table_code, 1]
    data <- R.cache::loadCache(list(table_code, last_update), dirs = "csodata")
    if (!is.null(data)) {
      if (!suppress_messages) {
        message("Loaded cached data\n")
      }
      return(data)
    }
  }
  
  #Empty out the cache of unused files if a new file is being downloaded
  #checks if csodata directory in cache before attempting to flush it
  if(flush_cache & dir.exists(paste0(R.cache::getCacheRootPath(),"\\csodata"))){
    file.remove(
      rownames(
        fileSnapshot(paste0(R.cache::getCacheRootPath(),"\\csodata"), full.names = T, recursive = T)$info[!lubridate::`%within%`(
          fileSnapshot(paste0(R.cache::getCacheRootPath(),"\\csodata"), full.names = T, recursive = T)$info[,"mtime"],
          lubridate::interval(start = Sys.Date() - lubridate::days(2) , end = Sys.Date() + lubridate::days(1))) , ]
      )
      )
  } 
  
  
  # No caching, or cache empty ----------
  
  # Check for errors using trycatch since PxStat API does not support
  # html head requests.
  error_message =  paste0("Failed retrieving table. Please check internet ",
                          "connection and that data .cso.ie is online")
  
  response <- tryCatch({
    httr::GET(url)
  }, warning = function(w) {
    print(paste0("Warning: ", error_message))
    return(NULL)
  }, error = function(e) {
    message(paste0("Connection Error: ", error_message))
    return(NULL)
  })
  
  #Cut off if theres issues                   
  if(is.null(response)){return(NULL)}
  
  # Check if data valid -------------
  if (httr::status_code(response) == 200 &&
      !all(response[["content"]][1:32] ==
           charToRaw("invalid Maintable format entered"))) {
    json_data <- rawToChar(response[["content"]])
    temp_json <- jsonlite::fromJSON(json_data)
    json_data <- jsonlite::toJSON(temp_json$result, auto_unbox = TRUE )
    if (cache) {
      last_update <- toc[toc$id == table_code, 1]
      R.cache::saveCache(json_data,
                         key = list(table_code, last_update), dirs = "csodata"
      )
    }
    return(json_data)
  } else {
    stop("Not a valid table code. See cso_get_toc() for all valid tables.")
  }
}
