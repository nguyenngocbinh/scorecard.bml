# PSI function
# Reference: P395, Credit risk analytics - Measurement technique, applications and examples
# Rules of thumb
# PSI < 0.10: no significant shift (green traffic light)
# 0.10 ≤ PSI < 0.25: moderate shift (yellow traffic light)
# PSI ≥ 0.25: significant shift (red traffic light)
library(tidyverse)

# Hàm tính PSI cho từng biến

psi <- function(actual, expected){
  
  tbl <- list(actual = actual, expected = expected) %>% 
    # map(factor, levels = lbl, labels = lbl) %>% 
    map(table) %>% 
    map(prop.table) %>% 
    map(as.data.frame, stringsAsFactors=FALSE) %>% 
    reduce(full_join, by = "Var1") %>% 
    mutate(sub_psi = (Freq.x-Freq.y)*(log(Freq.x)-log(Freq.y))) %>% 
    mutate(psi = sum(sub_psi)) %>% 
    select(Range = Var1, 
           Trainning = Freq.y, 
           Actual = Freq.x, 
           psi) %>% 
    mutate(Trainning = paste0(round(Trainning * 100, 1), "%"),
           Actual = paste0(round(Actual * 100, 1), "%"),
           psi = round(psi, 3),
           tag = case_when(psi < 0.1 ~ "green", 
                           psi < 0.25 ~ "yellow", 
                           TRUE ~ "red"))
  
  
  return(tbl)
}

# Hàm tính PSI cho cả bộ dữ liệu
psi_df <- function(dat_train, dat_test, var_skip = NULL){
  same_name <- names(dat_test) %>%
    intersect(names(dat_train)) %>%
    setdiff(var_skip)
  
  sl <- function(x){
    psi(expected = dat_train[[x]], actual = dat_test[[x]]) %>% return()
  }
  
  same_name %>% 
    set_names(same_name) %>%  
    map_dfr(sl, .id = "Variables") %>%  
    return()
  
}
