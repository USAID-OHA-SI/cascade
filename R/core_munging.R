#' Cascade filter
#' Filters the cascade variables to current fiscal year
#'
#' @param .data MER structured data set
#'
#' @return passed through a filtered cascade data frame
#' @export
#'
#'
fltr_cascade <- function(.data) {
  df <- .data %>%
    dplyr::filter(
      indicator %in% gophr::cascade_ind
    )
  return(df)
}


#' Filter disaggregate
#'
#' Filters the MSD to the appropriate population disaggregate
#'
#' @param .data  MER structured data set
#' @param pop_fltr population filter to be applied to disaggregate
#'
#' @return passes through a filtered cascade data frame
#' @export
#'
#'
fltr_disag <- function(.data, pop_fltr) {
  .data %>%
    dplyr::filter(standardizeddisaggregate %in% pop_fltr)
}




#' Filter for Adults and Young People
#'
#' Adds a category to trendscoarse if indicating AYP or Non AYP
#'
#' @param .data MER structured data set whittled down
#'
#' @return passes through a filtered ayp dataframe
#'
#
fltr_ayp <- function(.data) {
  .data %>%
    dplyr::mutate(trendscoarse = ifelse(ageasentered %in% c("15-19", "20-24"), "AYP", "Non AYP"))
}


#' Filter sex
#'
#' Filters a whittled down MSD into sex categories, male or female.
#'
#' @param .data whittled down MSD
#' @param m_or_f Male or Female filter
#'
#' @return passes through a filtered sex data frame
#'
#'
fltr_sex <- function(.data, m_or_f) {
  .data %>%
    dplyr::filter(sex %in% m_or_f)
}


#' Wrapper for youth cascade
#'
#' Combines the cascade filter with the peds disag
#'
#' @param .data MSD data set
#'
#' @return passes through a filtered youth data frame
#'
#'
youth_wrapper <- function(.data) {
  .data %>%
    fltr_cascade() %>%
    fltr_disag(pop_fltr = disag_peds)
}



#' Aggregate targets and results
#'
#' Takes a whittled down MSD and cleans the indicators,
#' summarizes the targets and results, and reshapes everything
#' into a long data set with quarters and cumulative results.
#'
#' @param .data MSD data set
#' @param ... additional parameters to pass to `group_by`
#'
#' @return passes through a reshaped data frame
#'
#'
sum_reshape <- function(.data, ...) {
  .data %>%
    gophr::clean_indicator() %>%
    dplyr::group_by(indicator, fiscal_year, ...) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("targets|qtr"), sum, na.rm = T)) %>%
    gophr::reshape_msd(direction = "quarters") %>%
    dplyr::ungroup()
}


#' Create TX_CURR_Lag2
#' Filters the cascade variables to current fiscal year
#'
#' @param .data MER structured data set
#'
#' @return passed through a filtered cascade data frame
#' @export
#'
#'
lag_tx_curr <- function(df) {
  df %>%
    dplyr::filter(indicator == "TX_CURR") %>%
    dplyr::arrange(period) %>%
    dplyr::mutate(TX_CURR_Lag2 = dplyr::lag(results, n = 2),
                  indicator = "TX_CURR_Lag2",
                  targets = 0,
                  results = TX_CURR_Lag2,
                  results_cumulative = TX_CURR_Lag2) %>%
    dplyr::select(-TX_CURR_Lag2)
}



# Bind and filter to curr FY
#' Title
#'
#' @param df base data frame to bind to
#' @param df2 tx_curr_lag2 data frame to bind on
#'
#' @return a single bound data frame
#' @export
#'
bind_tx_curr_lag <- function(df, df2){
  dplyr::bind_rows(df, df2) %>%
    dplyr::arrange(indicator) %>%
    dplyr::filter(fiscal_year == metadata$curr_fy)
}

