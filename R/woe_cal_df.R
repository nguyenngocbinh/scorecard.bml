# Compute WOE Table
#' @title woe_cal
#' @description Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export woe_cal
#' @param x The categorical variable stored as factor for which WOE Table is to be computed.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param valueOfBad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
#' @return The WOE table with the respective weights of evidence for each group and the IV's.
#' \itemize{
#'   \item group The groups (levels) of the categorical X variable for which WOE is to be calculated.
#'   \item total. The total number of observations in respective group.
#'   \item good. The total number of 'Goods' or 'Events' in respective group.
#'   \item bad. The total number of 'Bads' or 'Non-Events' in respective group.
#'   \item pert. The Percentage of observations accounted for by respective group.
#'   \item pertg. The Percentage of 'Goods' or 'Events' accounted for by respective group.
#'   \item pertb. The Percentage of 'Bads' or 'Non-Events' accounted for by respective group.
#'   \item br. The Percentage of 'Bads' or 'Non-Events' in respective group.
#'   \item woe. The computed weights of evidence(WOE) for respective group. The WOE values can be used in place of the actual group itself, thereby producing a 'continuous' alternative.
#'   \item iv. The information value contributed by each group in the X. The sum of IVs is the total information value of the categorical X variable.
#' }
#' @examples
#' data('hmeq')
#' woe_cal(x = hmeq$reason, y = hmeq$bad)
woe_cal <- function(x, y, valueOfBad = 1) {
    # check format of y
    if (is.numeric(y) == FALSE) {
        stop("y should be a numeric variable (0,1)", "\n")
    }
    # check length of y
    if (length(unique(y)) != 2) {
        stop("y is not a binary variable", "\n")
    } else {
        y <- ifelse(y == valueOfBad, 1, 0)
    }

    # check type variable store of x
    if(is.numeric(x)){
        warning("x should be a factor variable")
    }

    if(length(unique(x)) > 20){
        stop("too many unique value of x")
    }

    x <- as.factor(x)
    if (sum(is.na(x)) > 0) {
        levels(x) <- c(levels(x), "missing")
        x[is.na(x)] <- "missing"
        warning("x contains missing value")
    }
    x <- droplevels(x)
    woe_table <- data.frame(matrix(nrow = nlevels(x), ncol = 10))
    names(woe_table) <- c("group", "total", "good", "bad", "pert", "pertg", "pertb", "br", "woe", "iv")
    woe_table$group <- levels(x)
    for (i in levels(x)) {
        woe_table$total[woe_table$group == i] <- sum(x == i, na.rm = TRUE)
        woe_table$good[woe_table$group == i] <- sum(x == i & y == 0, na.rm = TRUE)
        woe_table$bad[woe_table$group == i] <- sum(x == i & y == 1, na.rm = TRUE)
        if (woe_table$good[woe_table$group == i] == 0 | woe_table$bad[woe_table$group == i] == 0) {
            cat("Group:", i, "having 0 obs good/bad", "\n")
        }
    }
    woe_table$pert <- woe_table$total/sum(woe_table$total, na.rm = TRUE)
    woe_table$pertg <- woe_table$good/sum(woe_table$good, na.rm = TRUE)
    woe_table$pertb <- woe_table$bad/sum(woe_table$bad, na.rm = TRUE)
    woe_table$br <- woe_table$bad/woe_table$total
    woe_table$woe <- log(woe_table$pertg/woe_table$pertb)
    woe_table$woe[is.finite(woe_table$woe) == FALSE] <- NA
    woe_table$iv <- (woe_table$pertg - woe_table$pertb) * woe_table$woe
    return(woe_table)
}

# Compute WOE Table
#' @title woe_cal_quantiles
#' @description Compute the woe_cal_quantiles that shows the Weights Of Evidence (WOE) for each group and respective Information Values (IVs). The group X is devided by quantile
#' @details For a given actual for a Binary Y variable and a numeric X variable, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom stats quantile
#' @export woe_cal_quantiles
#' @param x The categorical variable stored as factor for which WOE Table is to be computed.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param valueOfBad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
#' @param nbr_bin default = 20
#' @return The WOE table with the respective weights of evidence for each group and the IV's.
#' \itemize{
#'   \item group The groups (levels) of the categorical X variable for which WOE is to be calculated.
#'   \item total. The total number of observations in respective group.
#'   \item good. The total number of 'Goods' or 'Events' in respective group.
#'   \item bad. The total number of 'Bads' or 'Non-Events' in respective group.
#'   \item pert. The Percentage of observations accounted for by respective group.
#'   \item pertg. The Percentage of 'Goods' or 'Events' accounted for by respective group.
#'   \item pertb. The Percentage of 'Bads' or 'Non-Events' accounted for by respective group.
#'   \item br. The Percentage of 'Bads' or 'Non-Events' in respective group.
#'   \item woe. The computed weights of evidence(WOE) for respective group. The WOE values can be used in place of the actual group itself, thereby producing a 'continuous' alternative.
#'   \item iv. The information value contributed by each group in the X. The sum of IVs is the total information value of the categorical X variable.
#' }
#' @examples
#' data('hmeq')
#' woe_cal_quantiles(x = hmeq$loan, y = hmeq$bad)

woe_cal_quantiles <- function(x, y, valueOfBad = 1, nbr_bin = 20) {
    if (is.numeric(y) == FALSE) {
        stop("y should be a numeric variable (0,1)", "\n")
    }
    if (length(unique(y)) != 2) {
        stop("y is not a binary variable", "\n")
    } else {
        y <- ifelse(y == valueOfBad, 1, 0)
    }

    break_x = quantile(x, probs = seq(0, 1, length.out = nbr_bin + 1), na.rm = TRUE)
    break1 = break_x[2:(length(break_x) - 1)]
    break_x = c(-Inf, break1, +Inf)
    break_x = unique(break_x)
    bin_x = cut(x, breaks = break_x, include.lowest = TRUE, right = FALSE, dig.lab = 10)

    woe_cal(bin_x, y)
}

# Compute WOE Table
#' @title woe_cal_table
#' @description Compute the woe_cal_table that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a data.frame, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export woe_cal_table
#' @importFrom dplyr select mutate_if
#' @importFrom purrr map
#' @param df data.frame.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param valueOfBad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
#' @return The WOE table with the respective weights of evidence for each group and the IV's.
#' \itemize{
#'   \item group The groups (levels) of the categorical X variable for which WOE is to be calculated.
#'   \item total. The total number of observations in respective group.
#'   \item good. The total number of 'Goods' or 'Events' in respective group.
#'   \item bad. The total number of 'Bads' or 'Non-Events' in respective group.
#'   \item pert. The Percentage of observations accounted for by respective group.
#'   \item pertg. The Percentage of 'Goods' or 'Events' accounted for by respective group.
#'   \item pertb. The Percentage of 'Bads' or 'Non-Events' accounted for by respective group.
#'   \item br. The Percentage of 'Bads' or 'Non-Events' in respective group.
#'   \item woe. The computed weights of evidence(WOE) for respective group. The WOE values can be used in place of the actual group itself, thereby producing a 'continuous' alternative.
#'   \item iv. The information value contributed by each group in the X. The sum of IVs is the total information value of the categorical X variable.
#' }
#' @examples
#' data('hmeq')
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' woe_table <- woe_cal_table(df = hmeq1, y = "bad")
woe_cal_table <- function(df, y, valueOfBad = 1){
    explanatory <- df[[y]]

    if (is.numeric(explanatory) == FALSE) {
        stop("y should be a numeric variable (0,1)", "\n")
    }
    if (length(unique(explanatory)) != 2) {
        stop("y is not a binary variable", "\n")
    } else {
        explanatory <- ifelse(explanatory == valueOfBad, 1, 0)
    }

    woe_table <- df %>% select(-y) %>%
        mutate_if(is.character, as.factor) %>%
        map(woe_cal, y = explanatory)

    return(woe_table)

}


# Compute WOE for data.frame
#' @title woe_cal_df
#' @description Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom purrr map map2 reduce transpose
#' @importFrom magrittr %>% set_names
#' @importFrom dplyr select inner_join mutate_if
#' @export woe_cal_df
#' @param data The data is to be computed.
#' @param woe_table The table is calculated woe
#' @return The woe data
#' \itemize{
#'   \item woe_data. The data is computed woe.
#' }
#' @examples
#' data('hmeq')
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' woe_table <- woe_cal_table(df = hmeq1, y = "bad")
#' df_woe <- woe_cal_df(data = hmeq, woe_table = woe_table)
woe_cal_df <- function(data, woe_table) {
    name_list <- list(names(woe_table), paste0(names(woe_table), "_woe")) %>% transpose()
    woe_table2 <- woe_table %>%
        map(select, c("group", "woe")) %>%
        map(mutate_if, is.character, as.factor) %>%
        map2(name_list, set_names)
    woe_table2[["data"]] <- data
    woe_data <- reduce(woe_table2, inner_join, .dir = "backward")
    return(woe_data)
}

