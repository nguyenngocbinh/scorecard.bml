filter_cor <- function(dt,
                       y,
                       x = NULL,
                       method = "pearson",
                       cutoff = 0.6,
                       var_skip = NULL) {
  # x variable names
  x = x_variable(dt,y,x)
  
  dt = setDT(copy(dt)) # copy(setDT(dt))
  
  # force removed variables
  if (!is.null(var_skip))  x = setdiff(x, var_skip)

  # check force kept variables
  if (!is.null(var_skip)) {
    var_skip2 = intersect(var_skip, x)
    len_diff = length(var_skip) - length(var_skip2)
    if (len_diff > 0) {
      warning("Incorrect inputs; there are ", len_diff, " var_kp variables are not exist in input data, which are removed from var_kp. \n", setdiff(var_kp, var_kp2))
    }
    var_skip = var_skip2
  }
  
  # Search variables high cor
  vars_high_cor <- dt[,x, with = FALSE] %>% 
    cor(method = method) %>% 
    caret::findCorrelation(cutoff = cutoff, names = TRUE)
  
  dt_pass <- dt[, c(setdiff(x, vars_high_cor), c(y, var_skip)), with = FALSE]

  return(dt_pass)
}
