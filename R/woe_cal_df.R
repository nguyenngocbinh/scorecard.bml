# Compute WOE for data.frame
#' @title woe_cal_df
#' @description Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @import purrr
#' @export woe_cal_df
#' @param data The data is to be computed.
#' @param woe_table The table is calculated woe
#' @return The woe data
#' \itemize{
#'   \item woe_data. The data is computed woe.
#' }
#' @examples
#' data('SimData')
#' woe_cal_df(data=SimData, woe_table=woe_table)
woe_cal_df <- function(data, woe_table) {
    name_list <- list(names(woe_table), paste0(names(woe_table), "_woe")) %>% transpose()
    woe_table2 <- woe_table %>% map(select, c("group", "woe")) %>% map(mutate_if, is.character, as.factor) %>% map2(name_list, 
        set_names)
    woe_table2[["data"]] <- data
    woe_data <- reduce(woe_table2, inner_join, .dir = "backward")
    return(woe_data)
}

