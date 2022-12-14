#' Returns a data frame with all valid CSO PxStat tables listed sequentially
#' by id number, e.g. A0101, A0102, A0103, etc.
#'
#' Checks the CSO PxStat API for a list of all the table codes (e.g. A0101,
#' A0102, A0103, etc.), which also includes date last modified and title for
#' each table, and returns this list as an R data frame.
#'
#' The data is pulled from the ReadCollection on the CSO API. See
#' \url{https://github.com/CSOIreland/PxStat/wiki/API-Cube-RESTful}
#' for more information on this.
#'
#' @param cache logical. If TRUE (default) the table of contents is cached
#' with the system date as a key.
#' @param suppress_messages logical. If FALSE (default) a message is printed
#' when loading a previously cached table of contents.
#' @param get_frequency logical. If TRUE the frequency of each 
#' table(yearly, monthly etc...) will be returned as an additional column in 
#' the table of contents.
#' @param list_vars logical. If TRUE an additional column will be added
#' to the table of contents which lists each tables variables.
#' @param flush_cache logical. If TRUE (default) the cache will be checked for 
#' old, unused files. Any files which have not been accessed in the last month 
#' will be deleted.
#' @param from_date date in the format YYYY-MM-DD or Null. Will only return tables last modified after date provided. Default is 2 years from current date.
#' @return data frame of three character columns:
#' \itemize{
#'   \item id. Contains all of the table codes currently
#' available on the CSO API.
#'   \item LastModified. The date the table was last modified in POSIXct
#'   format.
#'   \item title. The title of the table.
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' head(cso_get_toc())
#' }
  cso_get_toc <- function(cache = TRUE, suppress_messages = FALSE, get_frequency = FALSE, list_vars = FALSE, flush_cache = TRUE,
                          from_date = lubridate::date(lubridate::today() - lubridate::years(2))){
    
    #changing Null to string of null
    if (is.null(from_date)){
      from_date <- "null"
    }
    url <- paste0(
      
      "https://ws.cso.ie/public/api.jsonrpc?data=%7B%0A%09%22jsonrpc%22:%20%222.0%22,%0A%09%22method%22:%20%22PxStat.Data.Cube_API.ReadCollection%22,%0A%09%22params%22:%20%7B%0A%09%09%22language%22:%20%22en%22,%0A%09%09%22datefrom%22:%20%22",from_date,"%22%0A%09%7D%0A%7D"
    )
    
    # cache
    if (cache) {
      data <- R.cache::loadCache(list("cso_toc", Sys.Date(),list_vars,get_frequency), dirs = "csodata")
      if (!is.null(data)) {
        if (!suppress_messages) {
          message("Loaded cached toc\n")
        }
        return(data)
      } }
    
    #Empty out the cache of unused files if a new file is being downloaded
    if(flush_cache){
      file.remove(
        rownames(
          fileSnapshot(paste0(R.cache::getCacheRootPath(),"/csodata"), full.names = T, recursive = T)$info[!lubridate::`%within%`(
            fileSnapshot(paste0(R.cache::getCacheRootPath(),"/csodata"), full.names = T, recursive = T)$info[,"mtime"],
            lubridate::interval(start = Sys.Date() - lubridate::days(2) , end = Sys.Date() + lubridate::days(1) )) , ]
        ) #lubridate::`%m+%`(Sys.Date(),months(-1)) 
      )
    }
    
    # Check for errors using trycatch since PxStat API does not support
    # html head requests.
    error_message =  paste0("Failed retrieving table of contents. Please ",
                            "check internet connection and that data.cso.ie is online")
    
    tbl <- tryCatch({
      data.frame(jsonlite::fromJSON(url)$result)
      
    }, warning = function(w) {
      print(paste0("Warning: ", error_message))
      return(NULL)
    }, error = function(e) {
      print(paste0("Error: ", error_message))
      return(NULL)
    })
    
    if(is.null(tbl)){return(NULL)}
    
    tbl2 <- cbind(tbl[c("link.item.updated","link.item.label")],data.frame(tbl$link.item.extension$matrix))
    tbl3 <- dplyr::mutate_if(tbl2, is.factor, as.character)
    
    names(tbl3)[1] <- "LastModified"
    names(tbl3)[2] <- "title"
    names(tbl3)[3] <- "id"
    
    tbl3$LastModified <- as.POSIXct(tbl3$LastModified,
                                    format = "%Y-%m-%dT%H:%M:%SZ")
    
    if (get_frequency){
      tbl3 <- cbind(tbl3,
                    tbl$link.item.dimension$`TLIST(A1)`$label,
                    tbl$link.item.dimension$`TLIST(M1)`$label,
                    tbl$link.item.dimension$`TLIST(Q1)`$label,
                    tbl$link.item.dimension$`TLIST(W1)`$label,
                    tbl$link.item.dimension$`TLIST(H1)`$label,
                    tbl$link.item.dimension$`TLIST(D1)`$label
      )
      
      colnames(tbl3)[4:9] <- c("Tlist_A","Tlist_M","Tlist_Q","Tlist_H","Tlist_D","Tlist_W")
      
      #tbl3 <- data.frame(lapply(tbl3, as.character), stringsAsFactors=FALSE) # need make DF char for case_when function
      
      tbl3 <- dplyr::mutate(tbl3, ReleaseFrequency = dplyr::case_when(
        !is.na (Tlist_M) ~ Tlist_M,
        !is.na (Tlist_A) ~ Tlist_A,
        !is.na(Tlist_Q) ~ Tlist_Q,
        !is.na (Tlist_H) ~ Tlist_H,
        !is.na(Tlist_D) ~ Tlist_D,
        !is.na(Tlist_W) ~ Tlist_W,
        TRUE ~ NA_character_
      ))
      
      tbl3 <- tbl3[, c("LastModified","title","id","ReleaseFrequency")] # keep columns
    }
    
    if (list_vars){
      catlist <- lapply(tbl$link.item.id,setdiff, c("STATISTIC","TLIST(Q1)","TLIST(A1)","TLIST(M1)"))
      l2 <- c()
      for (i in 1:length(catlist)) {
        l <- c()
        for (j in 1:length(catlist[[i]])) {
          l <- append(l,tbl$link.item.dimension[catlist[[i]][j]][i,]$label)
        }
        l2[[i]] <- as.list(l)
      }
      tbl3$vars <- l2
    }
    
    
    
    if (cache){
      R.cache::saveCache(tbl3,
                         key = list("cso_toc", Sys.Date(), list_vars,get_frequency), dirs = "csodata"
      )
    }
    
    return(tbl3)
  }

#' Search list of all table descriptions for given string
#'
#' Searches the list of all table descriptions returned by cso_get_toc() for a
#' given substring.
#'
#' @param string string. The text to search for. Case insensitive.
#' @param toc data.frame. The table of contents as returned by cso_get_toc. If
#' not given, will be re-downloaded (or retrieved from cache) using
#' cso_get_toc().
#' @param flush_cache logical. If TRUE  the cache will be checked for old, unused
#' files. Any files wich have not been accessed in the last month will be deleted
#' strings.
#' @return data frame of three character columns, with layout identical to
#' that of cso_get_toc. A subset of the results of cso_get_toc, with only rows
#' where the description field contains the entered string.
#'
#' @export
#' @examples
#' \dontrun{
#' trv <- cso_search_toc("travel")
#' }
cso_search_toc <- function(string, toc = cso_get_toc(suppress_messages = TRUE, flush_cache = FALSE)) {
  # Error Checking ----------------------
  if (is.null(toc)) {
    return(NULL)
  }

  # Search string -----------------------
  pattern <- toupper(string)
  x <- toupper(toc$title)

  # Use grep to search ------------------
  toc[grep(pattern, x), ]
}
