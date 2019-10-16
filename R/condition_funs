# x variable
x_variable = function(dt, y, x, var_skip=NULL) {
  x_all = setdiff(names(dt), c(y, var_skip))
  
  if (is.null(x)) x = x_all
  
  if ( length(setdiff(x,x_all)) > 0 ) {
    warning(sprintf('Incorrect inputs; there are %s variables are not exist in the input data frame, which are removed from x. \n%s', length(setdiff(x, x_all)), paste(setdiff(x, x_all), collapse = ', ')) )
    x = intersect(x, x_all)
  }
  
  return(x)
}

