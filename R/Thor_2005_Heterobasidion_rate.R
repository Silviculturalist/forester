#' Probability of incidence of Heterobasidion Root Rot in individual trees of Norway Spruce.
#'
#' @source Thor, M. 2005. Heterobasidion Root Rot in Norway Spruce: Modelling Incidence, Control Efficacy and Economic Consequences in Swedish Forestry. Diss. Swedish University of Agricultural Sciences. Uppsala. p. 22. URL: https://pub.epsilon.slu.se/729/1/Avh.nr.5_.pdf
#'
#' @param Latitude Decimal Degrees WGS84.
#' @param Longitude Decimal Degrees WGS84.
#' @param Altitude Meters above sea level
#' @param TotalAge Total age, years.
#' @param DBHcm Diameter at Breast height, 1.3 meters.
#' @param SIS Site Index Prediction according to [forester::Hagglund_Lundmark_1979_Site_Index_Stand_Factors()]
#' @param soilMoisture Integer 1-5 [forester::Sweden_soil_types('moisture')]
#' @param soilTexture Integer 1-9 [forester::Sweden_soil_types('texture')]
#' @param BA_percent_NorwaySpruce Percent of basal area which is Norway Spruce in stand.
#' @param TsumCorrection Apply continental/maritime correction to Tsum? See [forester::Moren_Perttu_1994_Sweden_temperature_sum_5C()]
#' @param TmaxMonthly Required for TsumCorrection. Else NULL.
#' @param TminMonthly Required for TsumCorrection. Else NULL.
#' @param Region One of 'auto' for automatic detection from latitude, or 'national'
#' @param AtStumpHeight Boolean. If True, gives probability at stump. If FALSE (Default), provides probability of incidence at breast height 1.3 m.
#'
#' @return Numeric. Probability of incidence.
#' @export

Thor_2005_Heterobasidion_Decay_Probability_Individual_trees <- function(
  Latitude,
  Longitude,
  Altitude,
  TotalAge,
  DBHcm,
  SIS,
  soilMoisture,
  soilTexture,
  BA_percent_NorwaySpruce,
  TsumCorrection=FALSE,
  TmaxMonthly=NULL,
  TminMonthly=NULL,
  Region='auto', #or national
  AtStumpHeight=FALSE
){
  #Convert latitude to RT90 from WGS84
  Latitude = geom(terra::project(terra::vect(x = matrix(c(Longitude,Latitude),ncol=2),crs='epsg:4326'),'epsg:3021'))[[4]]
  Longitude = geom(terra::project(terra::vect(x = matrix(c(Longitude,Latitude),ncol=2),crs='epsg:4326'),'epsg:3021'))[[3]]
  if(Region=='auto'){
    Region = ifelse(Latitude>7e6,'North','South')
  }

  Tsum = Moren_Perttu_1994_Sweden_temperature_sum_5C(Latitude,Altitude,TsumCorrection,TmaxMonthly = TmaxMonthly,TminMonthly = TminMonthly)

  if(Region=='North'){
    RootRotProb =
      -4.93 +
      -1.4949E-3*Longitude+
      7.4552E-3*TotalAge+
      9.5968E-2*SIS+
      4.2212E-1*ifelse(Tsum<900 | Tsum>=1000)+
      4.8165E-3*DBHcm*10+
      6.3746E-2*BA_percent_NorwaySpruce/100

    Correction = 1.354
  }

  if(Region=='South'){

    RootRotProb =
      -1.9303E1+
      6.5569E-4*Longitude+
      -3.6854E-2*TotalAge+
      2.6239*log(TotalAge)+
      4.4097E-5*TotalAge*DBHcm*10+
      -1.0206*ifelse(SIS>15,1,0)+
      2.2457E-1*ifelse(Tsum<800 | Tsum>=1100,1,0)+
      2.4790E-1*ifelse(Altitude>100,1,0)+
      -5.9550E-1*Altitude/Tsum+
      -8.6392E-3*DBHcm*10+
      1.8944*log(DBHcm*10)+
      -3.1527E-1*ifelse(soilMoisture==4)+
      1.3148E-1*ifelse(soilTexture!=4)+
      1.4648E-1*log((BA_percent_NorwaySpruce/100)+0.1)

    Correction = 2.107


  }

  if(Region=='National'|Region=='national'){

    RootRotProb =
      -3.18388E1+
      -3.7414E-1*TotalAge+
      5.7919*log(TotalAge)+
      5.7248E-2*TotalAge*log(TotalAge)+
      1.5533E-2*SIS+
      3.3479E-1*ifelse(Tsum<800 | Tsum>=1100,1,0)+
      -3.0993E-1*ifelse(Altitude>100,1,0)+
      -6.1511E-2*DBHcm*10
      3.3909*log(DBHcm*10)+
      7.6693E-3*(DBHcm*10)*log(DBHcm*10)+
      -2.6827E-1*ifelse(soilMoisture==4)+
      1.5098E-1*ifelse(soilTexture!=4)+
      1.3704E-1*log((BA_percent_NorwaySpruce/100)+0.1)

      Correction = 2.037


  }

  if(AtStumpHeight) return(Correction*(exp(RootRotProb)/(1+exp(RootRotProb))))
  if(!AtStumpHeight) return(exp(RootRotProb)/(1+exp(RootRotProb)))


}
