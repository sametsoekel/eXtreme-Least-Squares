xls.objfun <- function(data,error_column_name,args,error_weights,error_ahead_level){

  df <- data

  sample_size <- base::nrow(df)

  df$ahead_error_symbolic <- base::sapply(
    1:sample_size,
    function(x) base::paste(df[[error_column_name]][x:min(x+error_ahead_level-1, sample_size)], collapse = " add ")
  )

  df$ahead_num <- base::sapply(base::strsplit(df$ahead_error_symbolic,split = ' add '),base::length)

  df <- base::subset(df,ahead_num == error_ahead_level)

  df$ahead_error_symbolic <- base::strsplit(df$ahead_error_symbolic,' add ')

  new_sample_size <- base::nrow(df)

  df$error_weights <- base::rep(base::list(error_weights),new_sample_size)

  new_error_column <- base::paste0(error_column_name,'_weighted')

  df[[new_error_column]] <- base::mapply(
    base::paste,
    df$error_weights,
    df$ahead_error_symbolic,
    sep = "*",
    collapse = " + "
  )

  sum_of_errors <- base::paste(df[[new_error_column]],collapse = ' + ')

  base::eval(base::parse(text = base::paste('objfun <- function(', args, ') { return(' , sum_of_errors , ')}', sep='')))

  base::list(objective = objfun)
}
