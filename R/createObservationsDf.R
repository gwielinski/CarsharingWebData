createObservationsDf <- function(df, interval = 300, precision = "minute"){

  # Double-check if precision argument is whitin selection

  if(!(precision %in% c("month", "day", "hour", "minute"))){
    return(print("precision argument is not within the following selection : [month, day, hour, minute]"))
  }

  # switch loop on the precision argument

  switch(precision,

         "month" = {return(
           observations <- data.frame(datetime = seq(ISOdate(year(min(df$ym)), month(min(df$ym)), 1),
                                                     ISOdate(year(max(df$ym)), month(max(df$ym)), 1),
                                                     by=(interval * 1 * 60 * 60 * 24 * 30))) %>%   # Spécifique l'intervalle de temps en seconde
             left_join(df %>% group_by(ym) %>% summarise(N = length(ym)),
                       by = c("datetime"="ym")) %>%  #Joindre au df le nombre d'observations par 5 minutes
             mutate(N = ifelse(is.na(N), -100, N)) %>% ungroup()
         )
         },

         "day" = {return(
           observations <- data.frame(datetime = seq(ISOdatetime(year(min(df$ymd)), month(min(df$ymd)), day(min(df$ymd)), 0,0,0),
                                                     ISOdatetime(year(max(df$ymd)), month(max(df$ymd)), day(max(df$ymd)), 0,0,0),
                                                     by=(interval * 1 * 60 * 60 * 24))) %>%   # Spécifique l'intervalle de temps en seconde
             left_join(df %>% group_by(ymd) %>% summarise(N = length(ymd)),
                       by = c("datetime"="ymd")) %>%  #Joindre au df le nombre d'observations par 5 minutes
             mutate(N = ifelse(is.na(N), -100, N)) %>% ungroup()
         )
         },

         "hour" = {return(
           observations <- data.frame(datetime = seq(ISOdatetime(year(min(df$ymdh)), month(min(df$ymdh)), day(min(df$ymdh)), hour(min(df$ymdh)),0,0),
                                                     ISOdatetime(year(max(df$ymdh)), month(max(df$ymdh)), day(max(df$ymdh)), hour(max(df$ymdh)),0,0),
                                                     by=(interval * 1 * 60 * 60))) %>%   # Spécifique l'intervalle de temps en seconde
             left_join(df %>% group_by(ymdh) %>% summarise(N = length(ymdh)),
                       by = c("datetime"="ymdh")) %>%  #Joindre au df le nombre d'observations par 5 minutes
             mutate(N = ifelse(is.na(N), -100, N)) %>% ungroup()
         )
         },

         "minute" = {return(
           observations <- data.frame(datetime = seq(ISOdatetime(year(min(df$ymdhm)), month(min(df$ymdhm)), day(min(df$ymdhm)), hour(min(df$ymdhm)), minute(min(df$ymdhm)), 0),
                                                     ISOdatetime(year(max(df$ymdhm)), month(max(df$ymdhm)), day(max(df$ymdhm)), hour(max(df$ymdhm)), minute(min(df$ymdhm)), 0),
                                                     by=(interval * 1 * 60))) %>%   # Spécifique l'intervalle de temps en seconde
             left_join(df %>% group_by(ymdhm) %>% summarise(N = length(ymdhm)),
                       by = c("datetime"="ymdhm")) %>%  #Joindre au df le nombre d'observations par 5 minutes
             mutate(N = ifelse(is.na(N), -100, N)) %>% ungroup()
         )
         }
  )

}

# En premier lieu, cette function crée une plage de temps équivalente entre la plus grande et la plus petite date du df,
# puis ajoute à chaque date le nombre d'observations aggrégée

# df data frame containig the original scrapped data with datetime observations
# interval numerical vector that will act as a "step" between each observation. interval is mesured in the same units as the precision variable
# precision character vector that will specify the unit of the interval variable, but will also determine which variable to use within the df

# Cette table permet d'aggréger le nombre d'observations par unité de temps
