rmDuplicated <- function(df, groupingVariables){

  return(df[!duplicated(df[, groupingVariables]),])

}
