
#' Returns source of msd
#'
#' @param file_path path where msd lives
#'
#' @return object with msd source
#' @export
#'
file_source <- function(file_path){
  msd_source <<- gophr::source_info(file_path)
  return(msd_source)
}

#' Returns period information from file path of msd
#'
#' @param file_path path where msd lives
#'
#' @return current period used as a filter
#' @export
#'
#' @examples
file_pd <- function(file_path){
  curr_pd <<- gophr::source_info(file_path, return = "period")
  return(curr_pd)
}

#' Returns fiscal year information for file path of msd
#'
#' @param file_path path where msd lives
#'
#' @return current fiscal year used as a filter
#' @export
#'
#' @examples
file_fy <- function(file_path){
  curr_fy <<- gophr::source_info(file_path, return = "fiscal_year")
  return(curr_fy)
}

#' Returns ggplot caption information for file path of msd
#'
#' @param file_path path where msd lives
#'
#' @return caption to be used in footnote
#' @export
#'
footer_meta <- function(file_path){
  data_source <<- gophr::source_info(file_path) %>%
    glue::glue(., 'Created by: USAID OHA SI Team')
  return(data_source)
}


#' Stores all metadata objects to Global Environment
#'
#' @param file_path path where msd lives
#'
#' @return global objects
#' @export
#'
#' @examples
#' \dontrun{
#'merdata <- file.path(glamr::si_path("path_msd"))
#'file_path <- return_latest(folderpath = merdata, pattern = "Zambia-Daily-2022-08-15")
#'get_file_metadata(file_path)}
#'
get_file_metadata <- function(file_path){
  file_source(file_path)
  file_pd(file_path)
  file_fy(file_path)
  footer_meta(file_path)
}






