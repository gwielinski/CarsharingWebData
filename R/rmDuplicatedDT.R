rmDuplicatedDT <- function(df, groupingVariables){

  return(subset(unique(setkeyv(data.table(df), c('vin','ymdhm')))))

}
