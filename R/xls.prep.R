#' Preparing eXtreme Least Squares Data
#'
#' Automatically used in `xls.fit()`
#' No need to use if the raw data is not specifically desired to be achieved.
#'
#' @param formula An object of class "formula": a symbolic description of the model to be fitted.
#' @param data A data.frame object.
#' @param dependent_var A character which is the same as left hand side variable in specified formula.
#'
#' @return A `list` object which contains a `data.frame` object to be modeled and character vector of independent variables.
#'
#' @export

xls.prep <- function(formula,data,dependent_var){
    
    matrix.baked <- stats::model.matrix.lm(formula,data,na.action = 'na.pass')
    
    df.baked <- base::as.data.frame(matrix.baked)
    
    base::rm(matrix.baked)
    
    df.baked[[dependent_var]] <- data[[dependent_var]]
    
    if(base::anyNA(df.baked)){

        count_na <- base::nrow(df.baked[!stats::complete.cases(df.baked),])
        msg_format <- '%s rows with missing data were dropped.'
        base::message(base::sprintf(msg_format,count_na))

        df.baked <- df.baked[stats::complete.cases(df.baked),]

    }

    dependent_var_vector <- df.baked[[dependent_var]]

    df.baked[[dependent_var]] <- NULL
        
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
    
    df.baked[["error_symbolic"]] <- base::sprintf('(%s - %s)^2',df.baked[["error_symbolic"]],dependent_var_vector)

    base::list(data=df.baked,independent_var=varnames)

}