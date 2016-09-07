createODDf <- function(df, interval = 300){

  od <- df %>%
          select(vin, model_name, id, nextobs, lat, lon, nextlat, nextlon, ymdhm, nextymdhm, energy_level, nextenergy_level,
                 timediff, energydiff, nextyear, nextmonth, nextday, nextwday) %>%
          filter(timediff > interval)

  return(od)
}
