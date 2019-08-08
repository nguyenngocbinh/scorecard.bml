# Compute AUC
#' @title get_auc
#' @description Compute the auc
#' @details With a Binary Y variable and a categorical X variable stored as factor. AUC value will be calculated
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom pROC auc
#' @importFrom magrittr %>%
#' @export get_auc
#' @param y binary variable
#' @param x categorical variable
#' @return AUC value
#' @examples
#' data('hmeq')
#' auc1 <- get_auc(x = hmeq$reason, y = hmeq$bad)

get_auc <- function(x, y) {

  if (length(x) != length(y)) {
    stop("length(x) != length(y)")
  }

  if (length(unique(y)) != 2) {
    stop("y is not a binary variable", "\n")
  }

  logit_fit <- glm(y ~ x, family = binomial(link = "logit"))
  logit_auc <- auc(logit_fit$y, logit_fit$fitted.values, levels = c(0, 1), direction = "<") %>% as.numeric()
  return(logit_auc)

}


# Compute information value
#' @title get_iv
#' @description Compute the iv
#' @details For a given actual for a Binary Y variable and a category or continous X variable. Information value will be calculated
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom magrittr %>%
#' @export get_auc
#' @param y binary variable
#' @param x categorical variable
#' @return AUC value
#' @examples
#' data('hmeq')
#' iv1 <- get_iv(x = hmeq$reason, y = hmeq$bad)
#' iv2 <- get_iv(x = hmeq$loan, y = hmeq$bad)

get_iv <- function(x, y) {
  if (is.numeric(x)) {
    iv <- sum(woe_cal_num(x, y)$iv, na.rm = TRUE)
  } else if (is.factor(x)) {
    iv <- sum(woe_cal(x, y)$iv, na.rm = TRUE)
  } else {
    stop("x is not a factor or numeric variable")
  }
  return(iv)
}


# Compute missing rate
#' @title Compute missing rate
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export get_missing
#' @examples
#' data('hmeq')
#' get_missing(hmeq$loan)

get_missing <- function(x) {
  rate <- sum(is.na(x)) / length(x)
  return(rate)
}

# Compute identical rate
#' @title Compute identical rate
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export get_identical
#' @examples
#' data('hmeq')
#' get_identical(hmeq$loan)

get_identical <- function(x) {
  uniqx <- unique(x)
  cnt_mode <- max(tabulate(match(x, uniqx)))
  rate <- cnt_mode / length(x)
  return(rate)
}

# Compute Kolmogorov Smirnov statistic
#' @title Compute Kolmogorov Smirnov statistic
#' @description Compute the ks
#' @details With a Binary Y variable and a categorical X variable stored as factor. KS value will be calculated
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom ROCR prediction performance
#' @export get_ks
#' @param y binary variable
#' @param x categorical variable
#' @return AUC value
#' @examples
#' data('hmeq')
#' ks1 <- get_ks(x = hmeq$reason, y = hmeq$bad)
get_ks <- function(x, y){
  if (length(x) != length(y)) {
    stop("length(x) != length(y)")
  }

  if (length(unique(y)) != 2) {
    stop("y is not a binary variable", "\n")
  }
  logit_fit <- glm(y ~ x, family = binomial(link = "logit"))
  logit_scores <- prediction(predictions = logit_fit$fitted.values, labels = logit_fit$y)
  logit_perf <- performance(logit_scores, "tpr", "fpr")
  logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
  logit_ks
}


# Single factor analysis
#' @title Filter variable
#' @description This function filter variables base on specified conditions, such as information value, missing rate, identical value rate, Kolmogorov Smirnov statistic.
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr select select_if mutate mutate_if group_by setdiff case_when
#' @importFrom tidyr gather nest
#' @importFrom tibble rownames_to_column
#' @export filter_sfa
#' @param y binary variable
#' @param x_list list predictor variables
#' @return data.frame include pass variables
#' @examples
#' data('hmeq')
#' x <- var_filter(hmeq,
#'            y = "bad",
#'            missing_limit = 0.95,
#'            iv_limit = 0.3,
#'            auc_limit = 0.5,
#'            ks_limit = 0.4,
#'            identical_limit = 0.95,
#'            var_rm = NULL,
#'            var_skip = "loan",
#'            return_rm_reason = TRUE )

filter_sfa <- function(df,
                       y,
                       x_list = NULL,
                       missing_limit = 0.95,
                       iv_limit = 0.01,
                       auc_limit = 0.5,
                       ks_limit = 0.1,
                       identical_limit = 0.95,
                       var_skip = NULL,
                       return_rm_reason = TRUE) {
  # Check x_list status
  if (is.null(x_list)){
    predictor <- names(df) %>% setdiff(c(y, var_skip))
  } else {
    predictor <- x_list %>% setdiff(c(y, var_skip))
  }

  dat <- select(df, predictor)
  explanatory <- df[[y]]

  tbl_missing <- dat %>% map_df(get_missing)
  tbl_identical <- dat %>% map_df(get_identical)
  tbl_auc <- dat %>% map_df(get_auc, y = explanatory)
  tbl_iv <- dat %>% map_df(get_iv, y = explanatory)
  tbl_ks <- dat %>% map_df(get_ks, y = explanatory)

  tbl_sfa <- tbl_missing %>%
    rbind(tbl_identical) %>%
    rbind(tbl_auc) %>%
    rbind(tbl_iv) %>%
    rbind(tbl_ks) %>%
    t() %>%
    as.data.frame() %>%
    set_names(c("missing_rate", "identical_rate", "auc", "iv", "ks")) %>%
    rownames_to_column() %>%
    mutate(rm_reason = case_when(missing_rate > missing_limit ~ paste("missing >", missing_limit),
                                 identical_rate > identical_limit ~ paste("identical >", identical_limit),
                                 auc < auc_limit ~ paste("auc <", auc_limit),
                                 iv < iv_limit ~ paste("iv <", iv_limit),
                                 ks < ks_limit ~ paste("ks <", ks_limit),
                                 TRUE ~ "pass"))

  df_pass <- dat %>% select(tbl_sfa[tbl_sfa$rm_reason == "pass",]$rowname)

  return(list(tbl_sfa = tbl_sfa, df_pass = df_pass))
}

