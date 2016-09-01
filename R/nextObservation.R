nextObservation <- function(df, addfeatures = TRUE){

  # Créer des key figures additionnelles au niveau de la prochaine observation du même véhicule
    df <- df %>% arrange(vin, desc(datetime)) %>%
                 mutate(nextobs = lag(id)) %>% arrange(vin, datetime) %>% ungroup()

  if(addfeatures == TRUE){

    # Rajouter des key figures à la table provenant des id liés à la next obs
    df <- df %>%
          left_join(df %>%
                    select(id, lat, lon, energy_level, ymdhm) %>%
                    rename(nextlat = lat, nextlon = lon, nextenergy_level = energy_level, nextymdhm = ymdhm),
                    by = c("nextobs" = "id")) %>%
          mutate(timediff = nextymdhm - ymdhm, energydiff = nextenergy_level - energy_level) %>% ungroup()
  }

   return(df)
}
