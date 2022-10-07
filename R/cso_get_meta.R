#' Returns a data frame with the metadata of a CSO data table
#'
#' Checks the CSO PxStat API for a metadata on a dataset and returns it as a
#' list of metadata and contained statistics.
#'
#'
#' @param table_code string. A valid code for a table on data.cso.ie .
#' @param cache logical. Whether to use cached data, if available.
#' Default value is TRUE.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for old, unused
#' files. Any files which have not been accessed in the last month will be deleted
#' @return list with nine elements:
#' \itemize{
#'   \item The title of the table.
#'   \item The units used (the R class of the value column)
#'   \item The Copyright on the data.
#'   \item The time interval used in the data. (Census year, Quarter, Month)
#'   \item The date the table was last modified.
#'   \item The names of the variables included in the table, returned as a
#'   character vector with one element for each variable.
#'   \item The names of the statistics included in the table, returned as a
#'   character vector with one element for each statistic.
#'   \item An indicator if the statistics are experimental
#'   \item Returns if the data is geographic
#' }
#' @export
#' @examples
#' meta1 <- cso_get_meta("HS014", cache = FALSE)
#' 
cso_get_meta <- function(table_code, cache = TRUE, flush_cache = TRUE) {
  # Use fromJSON in order to preserve metadata ---
  tbl <- cso_download_tbl(table_code, cache, flush_cache)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }
  
  response_fj <- jsonlite::fromJSON(tbl)
  title <- response_fj$label
  
  time_period <- response_fj$dimension[[2]]$label 
  units <- response_fj$dimension$STATISTIC$category$unit[[1]]$label
  date_modified <- response_fj$updated
  vars <- setdiff(names(rjstat::fromJSONstat(tbl,naming = "label", use_factors = TRUE)), c("Statistic","value"))
  stat <- as.character(response_fj$dimension$STATISTIC$category$label)
  
  copyright <- response_fj$extension$copyright$name
  experimental <- response_fj$extension$experimental
  
  if (is.null(response_fj$dimension[[3]]$link$enclosure)){
    geo <- FALSE
  } else{
    geo <- response_fj$dimension[[3]]$label
  }
  
  
  list(
    Title = title, Time = time_period,
    Units = units, Date_last_modified = date_modified,
    Variables = vars, Statistics = stat,
    Copyright = copyright,
    Is_Experimental = experimental,
    Geographic = geo
  )
}

#' Returns a character vector listing the contents of a CSO data table
#'
#' Reads the metadata of a table to return a character vector of the
#' included variables in the table.
#'
#' @param table_code string. A valid code for a table on data.cso.ie .
#' @param cache logical. Whether to use cached data, if available.
#' Default value is TRUE. Strongly recommended to use caching, as otherwise
#' the entire table could be downloaded only to access a small part of its
#' metadata.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for old, unused
#' files. Any files which have not been accessed in the last month will be deleted.
#' @return character vector. The names of the statistics included in the
#' table.
#' @export
#' @examples
#' \dontrun{
#' cso_get_vars("IPA03")
#' }
cso_get_vars <- function(table_code, cache = TRUE, flush_cache = TRUE) {
  tbl <- cso_download_tbl(table_code, cache, flush_cache)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }
  
  setdiff(names(rjstat::fromJSONstat(tbl,naming = "label", use_factors = TRUE)), c("Statistic","value"))
}


#' Returns a list of the values of variables of a CSO data table
#'
#' Reads the table to determine all the unique values taken by the
#' variables in the table and returns them as a list.
#'
#' @param table_code string. A valid code for a table on data.cso.ie .
#' @param cache logical. Whether to use cached data, if available.
#' Default value is TRUE.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for old, unused
#' files. Any files which have not been accessed in the last month will be deleted
#' @return list. It has length equal to the number of variables in the table,
#' and each element is a character vector which has all the values taken by
#' one variable.
#' @export
#' @examples
#' \dontrun{
#' var_val <- cso_get_var_values("IPA03")
#' }
#' 
cso_get_var_values <- function(table_code, cache = TRUE, flush_cache = TRUE) {
  tbl <- cso_download_tbl(table_code, cache, flush_cache)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }
  
  response_fj <- jsonlite::fromJSON(tbl)
  vars <- setdiff(names(rjstat::fromJSONstat(tbl,naming = "id", use_factors = TRUE)), "value")
  len <- length(vars)
  var_vec <- vector(mode = "list", length = len)
  names(var_vec) <- setdiff(names(rjstat::fromJSONstat(tbl,naming = "label", use_factors = TRUE)), "value")
  for (i in 1:len) {
    response_fj$dimension
    string <- paste0("response_fj$dimension$`",
                     vars[i], "`$category$label")
    var_vec[i] <-  list(as.vector(unlist(eval(parse(text = string)))))
  }
  
  return(var_vec)
}


#' Returns a the time interval used to record data in a CSO table
#'
#' Reads the metadata of a table to return an atomic character vector
#' displaying the intervals at which the data included in the table was
#' gathered/calculated.
#'
#' @param table_code string. A valid code for a table on data.cso.ie .
#' @param cache logical. Whether to use cached data, if available.
#' Default value is TRUE. Strongly recommended to use caching, as otherwise
#' the entire table could be downloaded only to access a small part of its
#' metadata.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for old, unused
#' files. Any files which have not been accessed in the last month will be deleted
#' @return character vector. The names of the statistics included in the
#' table, with one element for each statistic.
#' @export
#' @examples
#' \dontrun{
#' interval <- cso_get_interval("C0636")
#' }
cso_get_interval <- function(table_code, cache = TRUE, flush_cache = TRUE) {
  tbl <- cso_download_tbl(table_code, cache, flush_cache)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }
  
  response_fj <- jsonlite::fromJSON(tbl)
  
  if (!is.null(response_fj$dimension[[2]]$category$index)) {
    out <- response_fj$dimension[[2]]$category$index 
  } else {
    out <- "There is no time interval information for this PxStat table"
  }
  return(out)
}


#' Returns a character vector listing the statistics in a CSO data table
#'
#'
#' @param table_code string. A valid code for a table on data.cso.ie .
#' @param cache logical. Whether to use cached data, if available.
#' Default value is TRUE.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for old, unused
#' files. Any files which have not been accessed in the last month will be deleted
#' @return character vector. The names of the statistics included in the
#' table, with one element for each statistic.
#' @export
#' @examples
#' \dontrun{
#' var_cont <- cso_get_content("EP008")
#' }
cso_get_content <- function(table_code, cache = TRUE, flush_cache = TRUE) {
  # Pull the list from API and keep only useful column -----
  tbl <- cso_download_tbl(table_code, cache, flush_cache)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }
  
  response_fj <- jsonlite::fromJSON(tbl)
  
  as.character(response_fj$dimension$STATISTIC$category$label)
}


#' Prints metadata from a PxStat table to the console
#'
#' Takes the output from \code{\link{cso_get_meta}} and prints it to the
#' console as formatted text.
#'
#' @param table_code string. A valid code for a table on data.cso.ie .
#' @return Does not return any values, rather the function prints the tables metadata to console.
#' @export
#' @examples
#' \dontrun{
#' cso_disp_meta("EP001")
#' }
cso_disp_meta <- function(table_code) {
  meta <- cso_get_meta(table_code, flush_cache = FALSE)
  
  # Error Checking ----------------------
  if (is.null(meta)) {
    return(NULL)
  }
  
  message("*** METADATA ***\n")
  message("CSO Table = ", meta$Title, "\n")
  message("Units = ", meta$Units, "\n")
  message("Copyright = ", meta$Copyright, "\n")
  message("Time interval in data = ", meta$Time, "\n")
  message("Are these statistics experimental? -", meta$Is_Experimental, "\n")
  message("Date last modified = ", meta$Date_last_modified, "\n")
  message("Variables:")
  print(meta$Variables)
  message("\nStatistics:")
  print(meta$Statistics)
  message("\nGeographic Data:")
  print(meta$Geographic)
}
