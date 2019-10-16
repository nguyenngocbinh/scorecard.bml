
#==============================================================================
# Hàm lọc các mô hình đa biến
# First date: 12/04/2019
# Hàm này dùng để lọc mô hình đa biến cuối cùng
# Đầu vào là file data đã được tính woe
# Đầu ra là coef, pvalue, auc, ks, vif, trọng số
#=============================================================================

#=============================================================================
mfa_metrics <- function(fit) {
  # library(ROCR)
  # library(magrittr)
  # KS
  logit_scores <- prediction(fit$fitted.values, fit$y)
  logit_perf <- performance(logit_scores, "tpr", "fpr")
  logit_ks <-
    max(logit_perf@y.values[[1]] - logit_perf@x.values[[1]]) %>% set_names("ks")
  # AUC
  logit_perf_auc <- performance(logit_scores, "auc")
  logit_auc <- logit_perf_auc@y.values[[1]] %>% set_names("auc")
  # Acurracy
  pred <-
    ifelse(fit$fitted.values > 0.5, "1", "0") %>% factor(., levels = c("0", "1"), labels = c("0", "1"))
  truth <-
    fit$y %>% factor(., levels = c("0", "1"), labels = c("0", "1"))
  conf <- caret::confusionMatrix(pred, truth, mode = "everything")
  # Output
  metric <- c(logit_auc, logit_ks, conf$overall, conf$byClass) %>%
    t() %>%
    as.data.frame()
  
  return(metric)
}

#=============================================================================
# Các thước đo về mô hình
mfa_turn_cutoffs <- function(fit){
  #library(ROCR)
  logit_scores <- prediction(fit$fitted.values, fit$y)
  logit_perf_roc <- performance(logit_scores, "tpr", "fpr")
  logit_perf_prec_rec <- performance(logit_scores, "prec", "rec")
  logit_perf_sens_spec <- performance(logit_scores, "sens", "spec")
  logit_perf_lift <- performance(logit_scores, "lift", "rpp")
  
  logit_measure <- c("acc", "err", "fpr", "tpr", "fnr", "tnr", 
                     "ppv", "npv", "phi", "lift", "f", "rch", 
                     "auc", "prbe", "cal", "rmse")
  
  # ppv Positive predictive value = Precision
  # tpr True positive rate = Recall
  # mat Matthews correlation coefficient
  perf_all <- purrr::map(logit_measure, function(x) {
    pref <- performance(logit_scores, x)
    value <- pref@y.values %>% unlist() %>%  as.numeric()
  })
  
  names(perf_all) <- logit_measure
  list(
    logit_perf_roc = logit_perf_roc,
    logit_perf_prec_rec = logit_perf_prec_rec,
    logit_perf_sens_spec = logit_perf_sens_spec,
    logit_perf_lift = logit_perf_lift,
    perf_all = perf_all
  ) %>% return()
  
}

#=============================================================================
# Hàm chuẩn hóa hệ số beta
# mfa_sd_beta <- function(fit){
#   b <- summary(fit)$coef[-1, 1]
#   sx <- sapply(fit$model[-1], sd)
#   sy <- sd(fit$y)
#   beta <- b * sx/sy
#   return(beta)
# }

#=============================================================================
# Hàm tính toán các đầu ra liên quan đến hệ số beta
mfa_estimate <- function(fit){
  # VIF
  mvif <- car::vif(fit) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    set_colnames(c("term","vif"))
  # Coef
  standard_estimate <- mfa_sd_beta(fit) %>% 
    as.data.frame() %>% 
    rownames_to_column()%>% 
    set_colnames(c("term","standard_estimate"))    
  
  coef_summary <- fit %>% 
    broom::tidy() %>% 
    left_join(standard_estimate, by = "term") %>% 
    left_join(mvif, by = "term") %>% 
    mutate(weight_estimate = abs(standard_estimate)/ sum(abs(standard_estimate), na.rm = TRUE))
  
  coef_summary %>% return()
  
}

#=============================================================================
mfa_combn_predictors <-
  function(predictors,
           y = "good_bad",
           min = NULL,
           max = NULL) {
    # Dạng mô  hình
    f_model_form <-
      function(x) {
        paste(y, paste(x, collapse = '+'), sep = "~")
      }
    # Kết hợp biến
    f_combn <- function(i) {
      combn(predictors, i, f_model_form)
    }
    # Tất cả các kết hợp
    formulas <- map(min:max, f_combn) %>%
      unlist()
    # Output
    return(formulas)
  }

#=============================================================================
# f_perf <- function(fit) {
#   logit_performance <-
#     list(fitted.values = fit$fitted.values, y = fit$y)
#   return(logit_performance)
# }

# ============================================================================
# This function is using to check require conditions of model like 
# pvalue, beta, vif, weight
# Inputs are tidy estimate

f_check_est <- function(estimate,
                        limit_pvalue = 0.05,
                        limit_vif = 10,
                        beta_weight_min = 0.05,
                        beta_weight_max = 0.3) {
  
  est <- estimate[-1,]
  
  check_est <- data.frame(
    check_beta = if_else(any(est$estimate > 0), "any beta > 0", "pass"),
    check_pvalue = if_else(
      any(est$p.value > limit_pvalue),
      paste("any pvalue >", limit_pvalue),
      "pass"
    ),
    check_vif = if_else(any(est$vif > limit_vif), paste("any vif >", limit_vif), "pass"),
    check_weight = case_when(
      any(est$weight_estimate > beta_weight_max) ~ paste0("any weight > ", beta_weight_max * 100, "%"),
      any(est$weight_estimate < beta_weight_min) ~ paste0("any weight < ", beta_weight_min * 100, "%"),
      TRUE ~ "pass"
    ),
    stringsAsFactors = FALSE
    
  )
  return(check_est)
}


#=============================================================================
filter_mfa <-
  function(df,
           y = "good_bad",
           nbr_predictors_min = 5,
           nbr_predictors_max = 12,
           limit_pvalue = 0.05,
           limit_vif = 10,
           beta_weight_min = 0.05,
           beta_weight_max = 0.3,
           limit_auc = 0.65,
           limit_ks = 0.6,
           var_skip = NULL
  ) {
    # Danh sách biến
    predictors <- df %>% names() %>% setdiff(c(y, var_skip))
    # Check: nbr_predictors_max vs nbr of predictors 
    nbr_predictors_max <- min_na_rm(length(predictors), nbr_predictors_max)
    # Kết hợp biến
    combine_predictor <-
      mfa_combn_predictors(predictors, y, nbr_predictors_min, nbr_predictors_max)
    # regression

    f_fit <- partial(glm, family = binomial(link = "logit"), data = df)
    f_mfa_metrics <- compose(mfa_metrics, f_fit, as.formula)
    f_mfa_estimate <- compose(mfa_estimate, f_fit, as.formula)
    
    fit_all_model <- tibble(formula_name = combine_predictor) %>%
      mutate(
        #formulas = map(formula_name, as.formula),
        #fit = map(formulas, f_fit),
        metrics = map(formula_name, f_mfa_metrics),
        estimate = map(formula_name, f_mfa_estimate),
        check = map(estimate, f_check_est)
      ) %>%
      mutate(model_name = paste0("mod_", row_number())) %>%
      select(model_name, everything()) %>%
      unnest(metrics) %>%
      unnest(check) %>%
      mutate(check_auc = case_when(auc < limit_auc ~ paste("auc <", limit_auc), TRUE ~ "pass")) %>%
      mutate(check_ks = case_when(ks < limit_ks ~ paste("ks <", limit_ks), TRUE ~ "pass")) %>%
      mutate(
        check_all = case_when(
          check_beta == "pass" & check_pvalue == "pass" &
            check_vif == "pass" &
            check_weight == "pass" &
            check_auc == "pass" &
            check_ks == "pass" ~ "pass",
          TRUE ~ "not pass"
        )
      )
    
    # Tue Jun 25 14:59:14 2019 ------------------------------
    tbl_models <-
      fit_all_model %>%
      select(
        model_name,
        formula_name,
        auc,
        ks,
        Accuracy,
        Kappa,
        Sensitivity,
        Specificity,
        F1,
        check_beta,
        check_pvalue,
        check_vif,
        check_weight,
        check_auc,
        check_ks,
        check_all
      )
    # Tue Jun 25 14:59:29 2019 ------------------------------
    tbl_estimate <- fit_all_model %>%
      select(model_name,
             formula_name,
             estimate) %>%
      unnest(estimate)
    
    return(list(tbl_models = tbl_models, tbl_estimate = tbl_estimate))
    
  }

#=============================================================================
filter_mfa_stepwise <- function(df,
                            y = "good_bad",
                            nbr_predictors_min = 5,
                            nbr_predictors_max = 12,
                            limit_pvalue = 0.05,
                            limit_vif = 10,
                            beta_weight_min = 0.05,
                            beta_weight_max = 0.3,
                            limit_auc = 0.65,
                            limit_ks = 0.6,
                            var_skip = NULL) {

  # Danh sách biến
  predictors <- df %>% names() %>% setdiff(c(y, var_skip))
  df2 <- df %>% select(c(y, predictors))
  # stepwise

  m1 <- glm(good_bad ~ ., family = binomial(), data = df2)
  
  m_step <- step(m1, direction="both", trace=FALSE)
  m2 <- eval(m_step$call)
  
}

#=============================================================================

  
