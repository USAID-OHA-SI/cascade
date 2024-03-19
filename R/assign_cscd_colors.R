
#' Assign cascade colors
#'
#' @param .data data frame resulting from `return_cascade`
#'
#' @return a data frame with assigned colors
#' @export
#'
assign_cscd_colors <- function(.data) {

  # Use the color names to set the facet order
  colors <- c(
    HTS_TST = "#B98ABF",
    HTS_TST_POS = "#E14BA1",
    TX_NEW = "#F36428",
    TX_NET_NEW = "#F9C555",
    TX_CURR = "#BC3414",
    TX_PVLS_D = "#5BB5D5",
    TX_PVLS = "#7EBCCB"
  )

  cascade_levels <- c(
    "Testing", "Linkage & Net New", "Treatment & VLS | VLC"
  )

  df_cscd_viz <-
    .data %>%
    dplyr::filter(indicator != "TX_CURR_Lag2") %>%
    dplyr::mutate(
      achv = ifelse(targets > 0, results_cumulative / targets, NA_real_),
      cascade = dplyr::case_when(
        stringr::str_detect(indicator, "HTS") ~ "Testing",
        stringr::str_detect(indicator, "TX_NE") ~ "Linkage & Net New",
        TRUE ~ "Treatment & VLS | VLC"
      )
    )

  df_cscd_viz <-
    df_cscd_viz %>%
    dplyr::bind_rows(df_cscd_viz %>%
                       dplyr::filter(indicator == "HTS_TST_POS") %>%
                       dplyr::mutate(cascade = "Linkage & Net New")) %>%
    dplyr::mutate(
      indicator = forcats::fct_relevel(factor(indicator), rev(names(colors))),
      cascade = forcats::fct_relevel(factor(cascade), cascade_levels)
    )

  return(df_cscd_viz)
}
