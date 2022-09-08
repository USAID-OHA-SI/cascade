
#' Title
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

#' Title
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

#' Title
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

#' Title
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


#' Title
#'
#' @param file_path path where msd lives
#'
#' @return global objects
#' @export
#'
#' @examples
get_file_metadata <- function(file_path){
  file_source(file_path)
  file_pd(file_path)
  file_fy(file_path)
  footer_meta(file_path)
}






