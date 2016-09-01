plotSuccObs <- function(observationdf){

  # Regarder la différence du nombre d'observations entre une valeur et la prochaine
  return(
   plot(
    table(
      observationdf %>% arrange(datetime) %>%
                       mutate(id = 1) %>% group_by(id) %>%
                       mutate(prevN = lag(N)) %>%
                       mutate(Ndiff = prevN - N) %>%
                       ungroup() %>%
                       select(Ndiff)
         ),
        xlab = "Differences between two successive obs", ylab = "Number of occurences"
      )
  )

}
