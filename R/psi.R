#' Population stability index (PSI) or system stability index (SSI)
#' @title psi
#' @description Compute the psi
#' @details Check whether the population on which the model is currently being used is similar to the population that was used to develop it.
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr mutate pull full_join
#' @importFrom purrr map reduce
#' @importFrom magrittr %>%
#' @export psi
#' @param x_train expected or training variable, should be a factor
#' @param x_test observed or actual variable, should be a factor
#' @return psi value
#' @references P395, Credit risk analytics - Measurement technique, applications and examples
#' Rules of thumb when using psi:
#' \tabular{rr}{
#' PSI \tab shift \cr
#' ----------------- \tab ---------------- \cr
#'      < 0.1 \tab no significant shift (green traffic light) \cr
#' 0.1 to 0.25 \tab moderate shift (yellow traffic light) \cr
#'       > 0.25 \tab significant shift (red traffic light)
#' }
#'
#' @examples
#' data('hmeq')
#' library(rsample)
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' spl <- initial_split(hmeq1)
#' train_df <- training(spl)
#' test_df <- testing(spl)
#' psi1 <- psi(train_df$job, test_df$job)
#'

psi <- function(x_train, x_test){
  #
  if(any(length(unique(x_train)), length(unique(x_test))) > 20){
    stop("too many unique value of x_train or x_test")
  }

  psi <- list(x_train = x_train, x_test = x_test) %>%
    # map(factor, levels = lbl, labels = lbl) %>%
    map(table) %>%
    map(prop.table) %>%
    map(as.data.frame) %>%
    reduce(full_join, by = "Var1") %>%
    mutate(psi = (Freq.x-Freq.y)*(log(Freq.x)-log(Freq.y))) %>%
    pull(psi) %>%
    sum()

  return(psi)
}


# Compute psi for all dataset
#' @title psi_df
#' @description Compute the psi for all dataset
#' @details Check whether the population on which the model is currently being used is similar to the population that was used to develop it.
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr mutate pull full_join case_when
#' @importFrom purrr map_dfr reduce
#' @importFrom magrittr %>% set_names
#' @export psi_df
#' @param df_train expected or training variable, should be a factor
#' @param df_test observed or actual variable, should be a factor
#' @param var_skip Name of force kept variables, Defaults to NULL
#' @return psi value
#' @references P395, Credit risk analytics - Measurement technique, applications and examples
#' Rules of thumb when using psi:
#' \tabular{rr}{
#' PSI \tab shift \cr
#' ----------------- \tab ---------------- \cr
#'      < 0.1 \tab no significant shift (green traffic light) \cr
#' 0.1 to 0.25 \tab moderate shift (yellow traffic light) \cr
#'       > 0.25 \tab significant shift (red traffic light)
#' }
#' @examples
#' data('hmeq')
#' library(rsample)
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' spl <- rsample::initial_split(hmeq1)
#' train_df <- training(spl)
#' test_df <- testing(spl)
#' psi_df1 <- psi_df(train_df, test_df)

psi_df <- function(dat_train, dat_test, var_skip = NULL){
  same_name <- names(dat_test) %>%
    intersect(names(dat_train)) %>%
    setdiff(var_skip)

  sl <- function(x){
    psi(dat_train[[x]], dat_test[[x]]) %>% as.data.frame() %>% set_names("psi") %>% return()
  }

  same_name %>%
    set_names(same_name) %>%
    map_dfr(sl, .id = "variables") %>%
    mutate(tag = case_when(psi < 0.1 ~ "green", psi < 0.25 ~ "yellow", TRUE ~ "red")) %>%
    return()

}

