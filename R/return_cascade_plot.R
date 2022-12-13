#' Create plot of selected cascades
#'
#' `return_cascade_plot` is a wrapper function that lets a user select between
#' 13 different cascade plots. Valid entries must fall between 1 and 10. The
#' function will plot the selected cascade and return a list of data underlying
#' the plot.
#'
#' @param msd_df MSD data frame from either a Genie or Panorama MSD
#' @param export exports the plot
#' @param path the path to where the plot will be saved (Images by default)
#'
#' @return returns a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' return_cascade_plot(df_sub, export = T, path = "Images")}
#'
return_cascade_plot <- function(msd_df, export = TRUE, path = "Images") {
  print(glue::glue_col("{yellow Please enter the cascade you would like to create.}"))
  print(glue::glue_col("{yellow {1:length(plot_name)}:{plot_name}}"))

  cscd_val <- readline(prompt = "Enter selection: ")
  cscd_num <- as.numeric(cscd_val)

  # Check the value entered is valid, if not return a useful error
  check_plot_entry(cscd_val)
  check_metadata()

  # Fetch the plot title
  p_title <- plot_title(cscd_num)

  # create cascade dataframe
  cscd_df <- return_cascade(msd_df, cscd_num)

  # create visualization dataframe
  df_viz <- cscd_df %>%
    assign_cscd_colors()

  # create the annotation dataframe (need the factor levels from df_viz)
  df_annot <- create_annotations(cscd_df, df_viz)

  # Plot the cascade
  suppressWarnings(p <- plot_cascade(df_viz, df_annot, cscd_num, p_title))

  # Export the plot as a standardized SI png
  if(export == TRUE){
    p_title <- gsub(" ", "_", p_title)
    glitr::si_save(glue::glue("{path}/{p_title}_cascade.png") %>% stringr::str_to_lower(.), p, scale = 1.25)
    return(p)
  } else {
    return(p)
  }
}
