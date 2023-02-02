#' Returns a munged data frame of cascade data.
#'
#' Fetches the correct version of the data from each cascade option.
#' Returns a data frame of cascade indicators for a given population
#'
#' @param df MER structured data set for a given population and geography
#' @param cscd_num cascade selection from user
#'
#' @return a data frame of cascade indicators
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # returns standard cascade
#' return_cascade(df, 1)
#'
#' # returns KP cascade
#' return_cascade(df, 13)
#' }
#'
return_cascade <- function(df, cscd_num) {

  # For Total Numerator all, female, male
  df_nonkp <- df %>%
    youth_wrapper()

  if (cscd_num %in% c(1, 2, 3)) {
    df_cscd <-
      df_nonkp %>%
      {
        if (cscd_num == 2) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 3) fltr_sex(., m_or_f = "Male") else .
      } %>%
      sum_reshape()
  }

  # Pediatric cascades all, female, male
  if (cscd_num %in% c(4, 5, 6)) {
    df_cscd <-
      df_nonkp %>%
      {
        if (cscd_num == 5) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 6) fltr_sex(., m_or_f = "Male") else .
      } %>%
      sum_reshape(trendscoarse) %>%
      dplyr::filter(trendscoarse == "<15")
  }

  # AYP cascades all, female, male
  if (cscd_num %in% c(7, 8, 9)) {
    df_cscd <-
      df_nonkp %>%
      {
        if (cscd_num == 8) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 9) fltr_sex(., m_or_f = "Male") else .
      } %>%
      fltr_ayp() %>%
      sum_reshape(trendscoarse) %>%
      dplyr::filter(trendscoarse == "AYP")
  }

  # Pediatric cascades all, female, male
  if (cscd_num %in% c(10, 11, 12)) {
    df_cscd <-
      df_nonkp %>%
      {
        if (cscd_num == 11) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 12) fltr_sex(., m_or_f = "Male") else .
      } %>%
      sum_reshape(trendscoarse) %>%
      dplyr::filter(trendscoarse == "15+")
  }

  # KP cascade (13 option)
  if (cscd_num == 13) {
    df_cscd <-
      df %>%
      fltr_cascade() %>%
      fltr_disag(pop_fltr = disag_kp) %>%
      sum_reshape()

  }

  tx_df <- lag_tx_curr(df_cscd)
  df <- bind_tx_curr_lag(df_cscd, tx_df)

  return(df)
}











