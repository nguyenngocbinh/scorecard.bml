# table
#' @title tab
#' @description Quickly show frequency table
#' @details Quickly show frequency table
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom knitr kable
#' @param vec is input vector
#' @export tab
#' @return frequency table
#' @examples
#' tab(iris$Species)
tab <- function(vec){
    vec %>% table() %>% prop.table() %>% kable() %>% return()

}

# Show encode vietnamese
#' @title vietnamese_code
#' @description Show encode vietnamese
#' @details Show encode vietnamese
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom  ggplot2 aes ggplot geom_point  geom_label element_blank scale_x_continuous theme_classic theme labs
#' @export vietnamese_code
#' @param show is logic option
#' @return vietnamese hex text
#' @examples
#' vietnamese_code()

## plot(1:4,rep(1,4),pch=c('\u0111','\u01B0','\u01A1','\u0103'),cex=4)
vietnamese_code <- function(show = TRUE) {
    x = 2:5
    y = rep(c(0, 3), 2)
    lbl = c("d", "u", "o", "a")
    val = c("u0111", "u01B0", "u01A1", "u0103")

    df <- data.frame(x = x, y = y, lbl = lbl, val = val)
    p <- df %>% ggplot(aes(x = x, y = y, label = lbl)) + geom_point(color = "white") + geom_label(aes(y = 1,
        label = lbl)) + geom_label(aes(y = 2, label = val)) + theme_classic() + theme(axis.line = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + scale_x_continuous(limits = c(1,
        6))
    labs(x = NULL, y = NULL)

    print(p)

}
