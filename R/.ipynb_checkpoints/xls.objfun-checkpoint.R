#' Preparing eXtreme Least Squares Nonlinear Objective Function
#'
#' Automatically used in `xls.fit()`
#' No need to use if the objective function is not specifically desired to be achieved.
#'
#' @param data A data.frame object which is returned by `xls.prep`. Tip: `xls.prep`'s `.$data` sub object returns the data.frame
#' @param error_column_name Symbolic error column's name. By default, it is named "error_symbolic" by `xls.prep()`
#' @param error_weights A numeric vector including error weights by order.
#' @param error_ahead_level An integer which represents how many steps further the parameters will be optimized for each data point.
#' @export

xls.objfun <- function(data,error_column_name,error_weights,error_ahead_level){
    
    df <- data
    
    sample_size <- nrow(df)
    
    df$ahead_error_symbolic <- base::sapply(
      1:sample_size,
      function(x) paste(df[[error_column_name]][x:min(x+error_ahead_level-1, sample_size)], collapse = " add ")
    )
    
    df$ahead_num <- sapply(strsplit(df$ahead_error_symbolic,split = ' add '),length)
    
    df <- df[df[['ahead_num']] == error_ahead_level,]
    
    df$ahead_error_symbolic <- strsplit(df$ahead_error_symbolic,' add ')
    
    new_sample_size <- nrow(df)
    
    df$error_weights <- rep(list(error_weights),new_sample_size)
    
    new_error_column <- paste0(error_column_name,'_weighted')
    
    df[[new_error_column]] <- mapply(
      paste,
      df$error_weights,
      df$ahead_error_symbolic,
      sep = "*",
      collapse = " + "
    )

    sum_of_errors <- base::paste(df[[new_error_column]],collapse = ' + ')
    
    multivariate_sse <- base::gsub("x\\[(\\d)\\]", "x\\1", sum_of_errors)
    
    base::suppressMessages({polynomial_sse <-  mpoly::mp(multivariate_sse, stars = TRUE)})
    
    base::suppressMessages({objfun <-  base::as.function(polynomial_sse, varorder = base::sort(mpoly::vars(polynomial_sse)))})
    
    base::return(objfun)
}