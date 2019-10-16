# PSI function
# Reference: P395, Credit risk analytics - Measurement technique, applications and examples
# Rules of thumb
# PSI < 0.10: no significant shift (green traffic light)
# 0.10 ≤ PSI < 0.25: moderate shift (yellow traffic light)
# PSI ≥ 0.25: significant shift (red traffic light)
library(tidyverse)

# Hàm tính PSI cho từng biến

psi <- function(actual, expected){
  
  list(actual = actual, expected = expected) %>% 
    # map(factor, levels = lbl, labels = lbl) %>% 
    map(table) %>% 
    map(prop.table) %>% 
    map(as.data.frame) %>% 
    reduce(full_join, by = "Var1") %>% 
    mutate(psi = (Freq.x-Freq.y)*(log(Freq.x)-log(Freq.y))) %>% 
    pull(psi) %>% 
    sum() -> psi

  return(psi)
}


# Hàm tính PSI cho cả bộ dữ liệu
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

