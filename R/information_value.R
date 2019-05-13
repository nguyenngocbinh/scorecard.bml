# Compute WOE Table
#' @title woe_cal
#' @description Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export woe_cal
#' @param x The categorical variable stored as factor for which WOE Table is to be computed.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param bad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
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
#' data('SimData')
#' woe_cal(X=SimData$X.Cat, Y=SimData$Y.Binary)
woe_cal <- function(x, y, bad = 1) {
    if (is.numeric(y) == FALSE) {
        stop("y should be a numeric variable (0,1)", "\n")
    }
    if (length(unique(y)) != 2) {
        stop("y is not a binary variable", "\n")
    } else {
        y <- ifelse(y == bad, 1, 0)
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


# Sample run: woe_cal(X = SimData$X.Cat, Y = SimData$Y.Binary, bad = 1)

