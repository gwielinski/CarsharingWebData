createODDf <- function(df, interval = 300, component = "Destination"){

  # Si on choisit de garder les données sur la destination de la transacton:
 if(component=="Destination"){
   od <- df %>%
          select(vin, model_name, id, nextobs, lat, lon, nextlat, nextlon, ymdhm, nextymdhm, energy_level, nextenergy_level,
                 timediff, energydiff, nextyear, nextmonth, nextday, nextwday, nexthour) %>%
          filter(timediff > interval)

  return(od)
 }
  # Si on choisit de garder les données sur l'origine de la transacton:
  if(component=="Origine"){
    od <- df %>%
      select(vin, model_name, id, nextobs, lat, lon, nextlat, nextlon, ymdhm, nextymdhm, energy_level, nextenergy_level,
             timediff, energydiff, year, month, day, wday, hour) %>%
      filter(timediff > interval)

    return(od)
  }else{
    return("The component parameter is not set properly.")
  }

}

