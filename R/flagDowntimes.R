flagDowntimes <- function(odDataFrame, observationsDataFrame){

  # Détecter les paires od où il y a eu une anomalie de détecté
  ## Voici le vecteur des temps où il y a une perte de données
  downtime <- observationsDataFrame %>% ungroup() %>% filter(N <= 0) %>%  select(datetime)

  ## Rouler la fonction intervalInSequence pour connaître les paires OD à flagger
  odDataFrame$downtime <- apply(
    as.data.frame(
    odDataFrame %>% mutate(lowlimit=as.numeric(ymdhm), uplimit=as.numeric(nextymdhm)) %>% select(lowlimit, uplimit)),  # l'objet sur lequel appliquer le apply
    1,                                      # Apply functions by row
    intervalInSequence,                     # Apply intervalInSequence function
    seq = as.numeric(downtime$datetime)     # Add additionnal arguments to the function
  )

  return(odDataFrame)


}
