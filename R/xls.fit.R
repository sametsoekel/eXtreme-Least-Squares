#' Fitting an eXtreme Least Squares Model
#'
#' Almost the same interface as `stats::lm`.
#' Just includes two parameters more, error_weights and error_ahead_level
#'
#' @param formula An object of class "formula": a symbolic description of the model to be fitted.
#' @param data A "data.frame" (with no missing values) object containing the variables in the model.
#' @param error_weights A numeric vector including error weights by order. If NULL, it is created automatically by error_ahead_level amount, decreasing at equal intervals. 
#' @param error_ahead_level An integer which represents how many steps further the parameters will be optimized for each data point.
#'
#' @return A `lm` object whose coefficients are optimized by the mentioned method.
#'
#' @examples
#' df <- datasets::airquality
#' 
#' ordered_df <- df[with(df,order(Month,Day)),]
#'
#' model <- xls.fit(Ozone ~ Solar.R + Wind + Temp,ordered_df,
#' error_weights = c(0.4,0.3,0.2,0.1),error_ahead_level = 4)
#' @export


xls.fit <- function(formula,
                    data,
                    error_weights = NULL,
                    error_ahead_level=4){
    
    if(base::nrow(data) < error_ahead_level){
        
        base::stop('The number of observations must be greater than error ahead level.')
        
    }
    
    dependent_var <- base::all.vars(formula)[1]
    
    if(base::is.null(error_weights)){
        
        dummy_weights <- base::seq(from = 0,to = 1,length.out = error_ahead_level + 1)
        error_weights <- dummy_weights[-1]/base::sum(dummy_weights[-1])
        error_weights <- base::rev(error_weights)
        base::rm(dummy_weights)
        
    }else if(base::length(error_weights) != error_ahead_level){
        
        base::stop('Error weights should have same length with ahead level.')
        
    }else if(!base::isTRUE(base::all.equal(1, base::sum(error_weights), tolerance = .Machine$double.eps^0.25))){
        
        base::stop('The sum of the error weights must be 1.')
        
    }
    
    

    prepared_obj <- xls.prep(formula,data,dependent_var)
    
    df <- prepared_obj$data
    
    independent_var <- prepared_obj$independent_var
    
    initial_solution <- base::rep(0,base::length(independent_var))

    objfun <- xls.objfun(data = df,error_column_name = 'error_symbolic',error_weights = error_weights, error_ahead_level = error_ahead_level)
    
    optimizing_parameters <- stats::optim(par = initial_solution,fn = objfun)
    
    coefficients <- base::as.data.frame(optimizing_parameters$par)
    
    base::colnames(coefficients) <- 'coef'
    
    base::rownames(coefficients) <- independent_var

    dummy_model <- stats::lm(formula,data)
    
    coefficients_vec <- coefficients$coef
    
    names(coefficients_vec) <- base::rownames(coefficients)
    
    dummy_model <- stats::lm(formula = formula,data = data)

    # Updating Coefficien

    dummy_model$coefficients <- coefficients_vec

    # Updating Residuals

    preds <- stats::predict(dummy_model,data)

    obs <- data[[dependent_var]]

    residuals <- obs - preds

    dummy_model$residuals <- residuals

    # Updating Call
    
    dataname <- base::substitute(data)
    
    dummy_model$call <- base::call('xls.fit',formula = formula,data = dataname,error_weights = error_weights,error_ahead_level = error_ahead_level)
    
    base::return(dummy_model)

}