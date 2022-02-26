#' Preparing eXtreme Least Squares Data
#'
#' Automatically used in `xls.fit()`
#' No need to use if the raw data is not specifically desired to be achieved.
#'
#' @param formula An object of class "formula": a symbolic description of the model to be fitted.
#' @param data A data.frame object.
#' @param dependent_var A character which is the same as left hand side variable in specified formula.
#' @export

xls.prep <- function(formula,data,dependent_var){
    
    matrix.baked <- stats::model.matrix(formula,data)
    
    df.baked <- base::as.data.frame(matrix.baked)
    
    base::rm(matrix.baked)
    
    varnum <- base::ncol(df.baked)
    
    varnames <- base::colnames(df.baked)
    
    coefnames <- base::paste0('x[',1:varnum,']')
    
    regformat <- base::paste(base::rep('%s*%s',varnum),collapse = ' + ')
    
    coef_var_match <- NULL
    
    for(i in 1:varnum){
        coef_var_match[i] <- base::sprintf('"%s",df.baked[["%s"]]',coefnames[i],varnames[i])
    }
    
    coef_var_match <- base::paste(coef_var_match,collapse = ',')
    
    symbolic_error_calculation <- base::sprintf('base::sprintf("%s",%s)',regformat,coef_var_match)
    
    base::eval(base::parse(text = base::sprintf('df.baked[["error_symbolic"]] <- %s',symbolic_error_calculation)))
    
    df.baked[["error_symbolic"]] <- base::sprintf('(%s - %s)^2',df.baked[["error_symbolic"]],data[[dependent_var]])

    base::list(data=df.baked,independent_var=varnames)

}