#' Tegnhammar SIS SIHjust adjustment 1992
#'
#' @param latitude decimal degrees.
#' @param longitude decimal degrees.
#' @param altitude metres above sea level
#' @param vegetation
#' @param ground_layer
#' @param aspect_main
#' @param incline_percent
#' @param soil_moisture
#' @param soil_depth
#' @param soil_texture
#' @param ditched
#' @param lateral_water
#' @param peat_humification "Low", if organic remnants clearly visible: squeezing sample gives somewhat cloudy water; "Medium", if with some difficulty organic remnants can be distinguished, if sample is squeezed the water is cloudy; or "High", if no organic remnants can be distinguished. Water and humus cannot be separated by squeezing. Humus is porridge-like.
#'
#' @return
#' @export
#'
#' @examples
Tegnhammar_SIS_Hjust_1992 <- function(species,latitude, longitude, altitude,vegetation, ground_layer,aspect_main,incline_percent,soil_moisture,soil_depth,soil_texture,ditched,lateral_water,peat_humification="Medium"){
  if(latitude>60){ #Replacement for Limes Norrlandicus
    LatN <- TRUE
    LatS <- FALSE
  } else {
    LatN <- FALSE
    LatS <- TRUE
  }

  if(LatN==TRUE){
    Altitude_S_Sweden <- FALSE
    Altitude_N_Sweden <- altitude
  } else {
    Altitude_N_Sweden <- FALSE
    Altitude_S_Sweden <- altitude
  }

  #humidity <- fun(lat,long)
  humidity_south_sweden_dry <- if(LatS==TRUE && soil_moisture%in%c(1,2)){
    humidity
  } else {
    0
  }

  humidity_north_sweden <- if(LatN==TRUE){
    humidity
  } else {
    0
  }

  humidity_north_sweden_LatN <- humidity_north_sweden*LatN

  shallow <- if(soil_depth!=1){1} else {0}

  dry <- if(soil_moisture==1){1} else {0}

  moist <- if(soil_moisture==4){1} else {0}

  peat <- if(soil_texture==9){1} else {0}

  if(ditched==1 && peat==0 && moist==0){
    message("Ditched only defined for peat or moist soils! Setting ditched to 0.")
    ditched <- 0
  }



  lateral_water_longer_periods <- if(lateral_water==4){1} else {0}

  lateral_water_shorter_periods <- if(lateral_water==3){1} else {0}

  coarse <- if(soil_texture%in%c(1,2,3)){1} else {0}

  fine <- if(soil_texture%in%c(7,8)){1} else {0}



  if(peat==0){
    peat_humification_low <- 0
    peat_humification_medium <- 0
    peat_humification_high <- 0
  } else if(peat==1){
    if(peat_humification=="Low"){
      peat_humification_low <- 1
      peat_humification_medium <- 0
      peat_humification_high <- 0
    } else if(peat_humification=="Medium"){
      peat_humification_low <- 0
      peat_humification_medium <- 1
      peat_humification_high <- 0
    } else if(peat_humification=="High"){
      peat_humification_low <- 0
      peat_humification_medium <- 0
      peat_humification_high <- 1
    }
  }


  #incline, if above 1:20 == 5% AND aspect east or southeast.
  ## NOTE ONLY MAIN ASPECTS AVAILABLE - INCLUDE SOUTH?
  aspect_east_south_east_incline <- if(aspect=="East" && incline_percent>5){1} else {0}


  #Extremely cold site
    if((altitude + (130*latitude) - 8900)>0){
      extremely_cold <- (altitude + (130*latitude) - 8900)
    } else {
      extremely_cold <- 0
    }

  #distance to closest SWEDISH coast in km
  distance_to_swedish_coast

  distance_to_swedish_coast_2 <- if(soil_moisture%in%c(1,2)){
    exp((-distance_to_swedish_coast/5))
  } else {
    0
  }

  #Vegetation type
  rich_herb_no_shrub <- if(vegetation==1){1} else {0}
  low_herb_no_shrub <- if(vegetation==3){4} else {0}
  herb_with_shrub <- if(vegetation%in%c(2,3,5,6)){1} else {0}
  herb <- if(vegetation%in%c(1:6)){1} else {0}
  no_field_layer <- if(vegetation==7){1} else {0}
  broadleaved_grass <- if(vegetation==8){1} else {0}
  thinleaved_grass <- if(vegetation==9){1} else {0}
  Carex_or_Equisetum <- if(vegetation==c(10:12)){1} else {0}
  bilberry <- if(vegetation==13){1} else {0}
  lingonberry <- if(vegetation==14){1} else {0}
  crowberry_or_worse <- if(vegetation>=15){1} else {0}

  #Ground layer
  sphagnum <- if(ground_layer==4){1} else {0}
  polytrichum <- if(ground_layer==5){1} else {0}

  #Calculation
  SIHjust <- 1210.813605+
    -14.969124*LatN+
    -15.444848*LatS+
    -1.308729*Altitude_N_Sweden+
    -0.000062709*(Altitude_N_Sweden^2)+
    -0.048787*Altitude_N_Sweden*moist+
    +0.000099984*(Altitude_N_Sweden^2)*moist+
    -2.396754*Altitude_S_Sweden+
    -0.000326*(Altitude_S_Sweden^2)+
    +0.019672*LatN*Altitude_N_Sweden+
    +0.042405*LatS*Altitude_S_Sweden+
    +4.084904*humidity_north_sweden+
    -0.064359*humidity_north_sweden_LatN+
    -0.085390*humidity_north_sweden*moist+
    +0.026421*humidity_south_sweden_dry+
    -22.492215*distance_to_swedish_coast_2+
    -6.494468*dry+
    +3.284251*moist+
    +8.004164*lateral_water_shorter_periods+
    +12.557257*lateral_water_longer_periods+
    +7.162013*ditched+
    -2.241642*fine+
    -3.944443*coarse+
    -22.450707*peat_humification_low+
    -10.735284*peat_humification_medium+
    -2.872332*peat_humification_high+
    -12.206813*shallow+
    +5.792854*shallow*moist+
    +36.421502*rich_herb_no_shrub+
    +31.110677*low_herb_no_shrub+
    +20.906555*herb_with_shrub+
    +22.879371*no_field_layer+
    +11.623366*broadleaved_grass+
    +13.542164*thinleaved_grass+
    -6.635013*Carex_or_Equisetum+
    -10.954260*lingonberry+
    -19.853882*crowberry_or_worse+
    -15.615067*sphagnum+
    -5.950684*polytrichum+
    -0.062692*extremely_cold+
    +5.299670*aspect_east_south_east_incline

  return(SIHjust)


}
