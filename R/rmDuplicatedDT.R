rmDuplicated <- function(df, groupingVariables){

  return(dsubset(unique(setkeyv(data.table(df), c('vin','ymdhm')))))

}
