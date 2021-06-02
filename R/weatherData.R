weatherData <- function(ID, latitude, longitude, altitude, date, time, measurement, value, height_above_ground_m){
  #Create siteData for weather station
  Site <- list("ID"=ID,
                   "latitude"=latitude,
                   "longitude"=longitude,
                   "altitude"=altitude)

  weather <- data.frame("date"=date, "time"=time, "measurement"=measurement, "value"=value, "height_above_ground_m"=height_above_ground_m)


  #Create output list.
  list("Site"=Site,
       "Weather"=weather
  )


}
