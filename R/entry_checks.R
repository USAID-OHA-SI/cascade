

#' Check plot entry value
#' Validate plot entry from user
#'
#' @param cscd_val number entered by user, from 1:13 for valid entries
#' @return a message for invalid plot entries
#'
#' @examples
#' \dontrun{
#' check_plot_entry()
#' }
#'
#'
check_plot_entry <- function(cscd_val){
  cscd_num <- as.numeric(cscd_val)
  if (!cscd_val %in% seq(1:length(plot_name))) {
    stop(glue::glue_col("{cyan Please enter a valid selection between 1 and {length(plot_name)}}"))
  } else {
    message(glue::glue_col("{yellow You have selected the {plot_name[cscd_num]} Cascade.}"))
  }
}

#' Check for metadata list
#'
#' @return message if list does not exist
#'
#' @examples
#' \dontrun{
#' check_metadata()
#' }
check_metadata <- function(){
  if(!exists('metadata')){
    stop(
      usethis::ui_code_block("gophr::get_metadata(file_path)"),
      usethis::ui_todo("Run the code chunk above with the appropriate data source file path")
    )
  }
}
