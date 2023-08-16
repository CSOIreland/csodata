#' csodata: A package for downloading CSO data.
#'
#' The csodata package allows for easily downloading CSO (Central Statistics
#' Office, the National Statistics Institute of Ireland) PxStat data into R.
#'
#' A specific table can be downloaded using \code{\link{cso_get_data}},
#' while a list of all tables currently available and their titles can
#' be found using \code{\link{cso_get_toc}} and \code{\link{cso_search_toc}}
#' is used to search their descriptions.
#' Metadata for a specified table can be retrieved with
#' \code{\link{cso_get_meta}}, or printed on the console using
#' \code{\link{cso_disp_meta}}.
#'
#' \code{\link{cso_get_vars}}, \code{\link{cso_get_interval}}, and
#' \code{\link{cso_get_content}} all return a subset of the full metadata of
#' a table. \code{\link{cso_get_var_values}} returns all the variables in the
#' tables.
#'
#' These functions provide the option to cache the returned data using the
#' R.cache package. The cache can be deleted using
#' \code{\link{cso_clear_cache}}.
#'
#' ESRI shapefiles covering the country in varying degrees of granularity can
#' be downloaded from
#' \href{https://www.cso.ie/en/census/census2011boundaryfiles/}{cso.ie} and
#' imported as an sf data frame using the \code{\link{cso_get_geo}} function.
#' Metadata about the map data can be retrieved with
#' \code{\link{cso_get_geo_meta}}, and displayed on the console with
#' \code{\link{cso_disp_geo_meta}}.
#'
#' @docType package
#' @name csodata
NULL


#' Clear csodata cache
#'
#' Deletes all data cached by the csodata package. The cached data from the
#' csodata package is stored in a subdirectory of the default R.cache cache
#' at R.cache::getCachePath(). This function provides a quick way to delete
#' those files along with the directory to free up space.
#'
#'
#' @return Does not return a value, deletes the csodata cache.
#' @export
#' @examples
#' \dontrun{
#' cso_clear_cache()
#' }
cso_clear_cache <- function() {
  R.cache::clearCache(dirs = "csodata", recursive = TRUE, prompt = FALSE)
  unlink(paste0(R.cache::getCacheRootPath(),"/csodata"), recursive = TRUE, force = TRUE)
}


