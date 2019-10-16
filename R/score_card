
#==============================================================================
# Hàm tính score card
# First date: 18/04/2019
#==============================================================================

# Hàm này dùng để tính điểm đối với từng đặc trưng của biến độc lập
# Đầu vào object: bins, model hồi quy đa biến cuối cùng
# Đầu ra là điểm theo từng đặc trưng

# library(tidyverse)
# library(magrittr)
# library(broom)

f_ab <- function(point0 = 200, odd0 = 1/10, pdo = 20){
  # z = beta0+beta1*x1+...+betar*xr = beta*x
  ##==> z = log(p/(1-p)),
  # odds = p/(1-p) # bad/good <==>
  # p = odds/1+odds
  ##==> z = log(odds)
  ##==> score = a - b*log(odds)
  
  # two hypothesis
  # point0 = a + b*log(odds0)
  # point0 + pdo = a + b*log(odds0/2)
  
  gamma1 = -pdo/log(2)
  gamma0 = point0 - gamma1*log(odd0)
  
  return(list(gamma0=gamma0, gamma1=gamma1))
}


f_score_card <- function(bins, model = fit_sel_model, point0 = 200, odd0 = 1/10, pdo = 20){

  # coefficients
  aabb = f_ab(point0, odd0, pdo)
  
  # Bảng hệ số hồi quy đa biến
  coef <- broom::tidy(model) %>% 
    mutate(variable = term %>% str_replace_all('_woe', '')) %>% 
    select(variable, estimate) %>% 
    filter(variable != "(Intercept)")

  # Number of independent vars
  k <- length(coef$variable)
  
  # Intercept
  intercept <- as.numeric(model$coefficients[1])
  
  # Thêm hệ số và WOE 
  table_score <- map_dfr(bins, bind_rows) %>% 
    inner_join(coef, by = 'variable') %>% 
  # Tính điểm
    mutate(intercept = intercept,
           k = k,
           point0 = point0,
           odd0 = odd0,
           pdo = pdo,
           gamma0 = aabb$gamma0,
           gamma1 = aabb$gamma1,
           base_point = gamma0 + gamma1*intercept,
           factor_points = woe * estimate * gamma1,
           score = base_point/k + factor_points,
           score = round(score, 0)
           ) 
  table_score %>% 
    select(- is_special_values) %>% 
    return()
}


#==============================================================================
# Hàm tính score data
# First date: 19/04/2019
# Sử dụng đầu vào là scorecard đã tính ở trên
#==============================================================================

f_score_data <- function(df_woe, score_card, class = "good_bad"){
  # Biến của mô hình cuối cùng
  final_vars <- score_card$variable %>% unique()
  # Chọn data.frame
  df_woe_sel <- df_woe %>% select(class, paste0(final_vars, "_woe"))
  # Hàm phụ đặt tên object
  f_set_names <- function(df, df_name){
    names(df) <- paste(df_name, names(df), sep = "_")
    df
  }
  # Tách điểm số theo từng thẻ để join
  split_sc <- score_card %>% 
    select(variable, woe, score) %>% 
    split(.$variable) %>% 
    map(~select_if(.,is.numeric)) %>% 
    map2(., names(.), f_set_names)
  # Thêm data vào list 
  split_sc[["df_woe_sel"]] <- df_woe_sel
  # vlookup data từ phải sang trái (backward)
  score_data <- reduce(split_sc, inner_join, .dir = "backward") %>% 
    mutate(total_score = rowSums(select(., contains("_score")))) %>% 
    select(class, total_score, contains("_score"), contains("_woe"))
  
}


