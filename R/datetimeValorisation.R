#' datetimeValorisation
#'
#' Function to add key figures generated from a date/time field
#' @param df data frame containig the date/time field to be improve
#' @param transormTimestamp boolean value; TRUE if the timestamp field is in character format and need to be transformed
#' @examples df <- datetimeValorisation(df, TRUE)

datetimeValorisation <- function(df, transformTimestamp = TRUE){

  if(transformTimestamp == TRUE){
  # Convert timestamp as a POSxilt date format
  df$datetime <- as.POSIXct(strptime(substr(df$timestamp,1,19), tz = Sys.timezone(location = TRUE), "%Y-%m-%d %H:%M:%S"))
  df$timestamp <- NULL   # Remove timestamp variable
  } else {
    df$datetime <- df$timestamp
    df$timestamp <- NULL   # Remove timestamp variable
  }

  # Create new key figures based on the new datetime variable
  df <- df %>%
    mutate(year = year(datetime),
           month = month(datetime),
           day = day(datetime),
           week = week(datetime),
           hour = hour(datetime),
           minute = minute(datetime),
           ym = ISOdate(year, month, 1),
           ymd = ISOdate(year, month, day),
           ymdh = ISOdatetime(year, month, day, hour,0,0),
           ymdhm = ISOdatetime(year, month, day, hour, minute - (minute %% 5),0)
    )
  return(df)
}
