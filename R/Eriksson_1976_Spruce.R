#' Bark procent of basal area at breast height
#'
#' @source From p. 82-83 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @param age_bh_100_largest_trees_per_ha_years Age of 100 largest trees at breast height per hectare in years.
#' @param diameter_of_mean_basal_area_under_bark_cm Diameter corresponding to the mean basal area under bark, cm.
#' @param basal_area_under_bark_ha_m2 Basal area under bark per hectare, m2.
#' @param SI SI H100 Spruce, m.
#'
#' @return \%
#' @export
Eriksson_1976_bark_procent <- function(age_bh_100_largest_trees_per_ha_years, SI, diameter_of_mean_basal_area_under_bark_cm, basal_area_under_bark_ha_m2){

  if(SI<=17.9){
    #G16
    b1 <- -0.224

  } else if(SI<=21.9){
    #G20
    b1 <- -0.247

  } else if(SI<=25.9){
    #G24
    b1 <- -0.249

  } else if(SI<=29.9){
    #G28
    b1 <- -0.266

  } else if(SI<=33.9){
    #G32
    b1 <- -0.255

  } else if(SI>=34){
    #G36
    b1 <- -0.280
  }

  return(
    45.08*(diameter_of_mean_basal_area_under_bark_cm^b1)*(basal_area_under_bark_ha_m2^-0.281)*(age_bh_100_largest_trees_per_ha_years^0.125)
  )
}


#' Bark subtraction procent
#'
#' @source From p. 82-83 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @param diameter_of_mean_basal_area_over_bark_cm Diameter corresponding to the mean basal area over bark, cm.
#' @param basal_area_over_bark_ha_m2 Basal area over bark per hectare, m2.
#' @param age_bh_100_largest_trees_per_ha_years Age of 100 largest trees at breast height per hectare in years.
#' @param SI Site index H100 m.
#'
#' @return \%
#' @export
Eriksson_1976_bark_subtraction_procent <- function(diameter_of_mean_basal_area_over_bark_cm,SI, basal_area_over_bark_ha_m2,age_bh_100_largest_trees_per_ha_years){

  if(SI<=17.9){
    #G16
    b1 <- -0.138

  } else if(SI<=21.9){
    #G20
    b1 <- -0.183

  } else if(SI<=25.9){
    #G24
    b1 <- -0.206

  } else if(SI<=29.9){
    #G28
    b1 <- -0.236

  } else if(SI<=33.9){
    #G32
    b1 <- -0.243

  } else if(SI>=34){
    #G36
    b1 <- -0.243
  }

  return(
    9.50*(diameter_of_mean_basal_area_over_bark_cm^b1)*(basal_area_over_bark_ha_m2^0.135)*(age_bh_100_largest_trees_per_ha_years^0.112)
  )
}

#' Basal area m2 per hectare before first thinning
#' @source From p. 99 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @description Author does not reccomend the use of this function if dominant height exceeds 16 meters, since self thinning increases rapidly if the number of stems then also is high.
#' In ranges between 1'100 and 10'000 stems per ha and a dominant height between 7 and 16 metres the function should provide acceptable results. Results may be less accurate at lower site indexes due to no underlying data.
#' @param dominant_height_m Dominant height, m.
#' @param number_of_trees_per_ha Number of trees per hectare.
#' @param planted If site was planted, TRUE. If the site was founded by pre-commercial thinning to a certain plant spacing, FALSE.
#'
#' @return Basal Area, m2 / ha
#' @export

Eriksson_1976_basal_area_before_first_thinning <- function(dominant_height_m, number_of_trees_per_ha,planted){

  b <- ifelse(planted,0.355,0.319)

  return(
    1.0111*((dominant_height_m-1.3)^1.230)*((number_of_trees_per_ha/1000)^b)
  )
}

#' Annual Increment in Basal Area in Spruce Stand in Eriksson 1976
#' @source From p. 63;64. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @details Coefficient of variation for basal area  on a 5 plot has been calculated as per p.112.
#'
#' @param basal_area_ha_before_thinning_m2 Basal area under bark per hectare before thinning, m2.
#' @param site_index SI H100 m Spruce
#' @param thinning_percent_ba_under_bark Thinning as percentage of basal area under bark before thinning.
#' @param diameter_of_mean_basal_area_of_thinned_trees_cm Diameter corresponding to mean basal area under bark of the thinned trees, cm.
#' @param diameter_of_mean_basal_area_before_thinning_cm Diameter corresponding to the mean basal area of the stand before thinning, cm.
#' @param number_thinned_trees_per_ha Number of thinned trees per hectare.
#' @param number_trees_per_ha_before_thinning Number of trees per hectare in the stand before thinning.
#' @param increment_period_years Length of increment period, years.
#' @param dominant_height_dm Dominant height, dm.
#' @param age_bh_100_largest_trees_per_ha_years Age at breast height of the 100 largest trees per hectare, years.
#'
#'
#'
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
Eriksson_1976_basal_increment_under_bark <- function(basal_area_ha_before_thinning.m2,
                                                     site_index,
                                                     thinning_percent_ba_under_bark,
                                                     diameter_of_mean_basal_area_of_thinned_trees_cm,
                                                     diameter_of_mean_basal_area_before_thinning_cm,
                                                     number_thinned_trees_per_ha,
                                                     number_trees_per_ha_before_thinning,
                                                     increment_period_years,
                                                     dominant_height_dm,
                                                     age_bh_100_largest_trees_per_ha_years
){

  if(site_index<=17.9){
    #G16
    b1 <- 0.250
    b2 <- -0.046
    b3 <- 0.049
    b4 <- 0.033
    b5 <- -0.748
    b6 <- -0.308
    c <- 0.447
  } else if(site_index<=21.9){
    #G20
    b1 <- 0.213
    b2 <- -0.055
    b3 <- 0.085
    b4 <- 0.460
    b5 <- -0.875
    b6 <- -0.313
    c <- 0.488
  } else if(site_index<=25.9){
    #G24
    b1 <- 0.304
    b2 <- -0.056
    b3 <- 0.126
    b4 <- 0.035
    b5 <- -0.571
    b6 <- -0.154
    c <- 0.498
  } else if(site_index<=29.9){
    #G28
    b1 <- 0.260
    b2 <- -0.059
    b3 <- 0.034
    b4 <- 0.112
    b5 <- -0.667
    b6 <- -0.066
    c <- 0.568
  } else if(site_index>=30){
    #G32
    b1 <-  0.337
    b2 <- -0.046
    b3 <- 0.056
    b4 <- 0.120
    b5 <- -0.789
    b6 <- -0.013
    c <- 0.763
  }

  return(
    2.635*basal_area_ha_before_thinning.m2^b1*
      (thinning_percent_ba_under_bark+0.01)^b2*
      ((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm+0.1)*(100*number_thinned_trees_per_ha/number_trees_per_ha_before_thinning+0.01))^0.024*
      increment_period_years^b3*
      dominant_height_dm^b4*
      age_bh_100_largest_trees_per_ha_years^b5*
      (13.778*((age_bh_100_largest_trees_per_ha_years/10)^c)*((number_trees_per_ha_before_thinning/1000)^-0.052))^b6
  )

}

#' Annual increment in dry weight of stem wood, tons per hectare.
#' @source From p. 93;94. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @param basal_area_ha_before_thinning_m2 Basal area under bark per hectare before thinning, m2.
#' @param site_index Spruce
#' @param thinning_percent_ba_under_bark Thinning as percentage of basal area under bark before thinning.
#' @param diameter_of_mean_basal_area_of_thinned_trees_cm Diameter corresponding to mean basal area under bark of the thinned trees, cm.
#' @param diameter_of_mean_basal_area_before_thinning_cm Diameter corresponding to the mean basal area of the stand before thinning, cm.
#' @param number_thinned_trees_per_ha Number of thinned trees per hectare.
#' @param number_trees_per_ha_before_thinning Number of trees per hectare in the stand before thinning.
#' @param increment_period_years Length of increment period, years.
#' @param dominant_height_dm Dominant height, dm.
#' @param age_bh_100_largest_trees_per_ha_years Age at breast height of the 100 largest trees per hectare, years.
#' @param basal_area_5_m_coef_of_variation Groupstructure index (Coefficient of variation for the basal area under bark on 5-metre circular plots inside the sample plot.) \% .
#'
#'
#'
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
Eriksson_1976_dry_weight_increment_tons_per_hectare <- function(basal_area_ha_before_thinning.m2,
                                                                site_index,
                                                                thinning_percent_ba_under_bark,
                                                                diameter_of_mean_basal_area_of_thinned_trees_cm,
                                                                diameter_of_mean_basal_area_before_thinning_cm,
                                                                number_thinned_trees_per_ha,
                                                                number_trees_per_ha_before_thinning,
                                                                increment_period_years,
                                                                dominant_height_dm,
                                                                age_bh_100_largest_trees_per_ha_years,
                                                                basal_area_5_m_coef_of_variation){
  if(site_index<=17.9){
    #G16
    b1 <- 0.320
    b2 <- -0.131
    b3 <- 0.065
    b4 <- 0.991
    b5 <- -0.842
    b6 <- -0.295
  } else if(site_index<=21.9){
    #G20
    b1 <- 0.455
    b2 <- -0.137
    b3 <- 0.064
    b4 <- 0.900
    b5 <- -0.866
    b6 <- -0.255
  } else if(site_index<=25.9){
    #G24
    b1 <- 0.523
    b2 <- -0.138
    b3 <- 0.092
    b4 <- 0.765
    b5 <- -0.702
    b6 <- -0.194
  } else if(site_index<=29.9){
    #G28
    b1 <- 0.399
    b2 <- -0.136
    b3 <- 0.012
    b4 <- 0.821
    b5 <- -0.786
    b6 <- -0.131
  } else if(site_index>=30){
    #G32
    b1 <-  0.442
    b2 <- -0.125
    b3 <- 0.021
    b4 <- 0.770
    b5 <- -0.814
    b6 <- -0.065
  }

  return(
    0.072*((basal_area_ha_before_thinning.m2)^b1)*((thinning_percent_ba_under_bark+0.01)^b2)*((((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm)+0.1)*(((100*number_thinned_trees_per_ha)/number_trees_per_ha_before_thinning)+0.01))^0.081)*((increment_period_years/10)^b3)*(dominant_height_dm^b4)*((age_bh_100_largest_trees_per_ha_years/10)^b5)*(basal_area_5_m_coef_of_variation^b6)
  )
}

#' Form height for spruce stands in Eriksson 1976
#'
#' @source From p. 78 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @param dominant_height_dm Dominant height, dm.
#' @param SI Site index, metres.
#' @param diameter_of_mean_basal_area_over_bark_cm Diameter corresponding to mean basal area over bark, cm.
#'
#' @return Form Height
#' @export
Eriksson_1976_form_height <- function(dominant_height_dm, SI, diameter_of_mean_basal_area_over_bark_cm){

  if(SI<=17.9){
    #G16
    b1 <- 0.835

  } else if(SI<=21.9){
    #G20
    b1 <- 0.833

  } else if(SI<=25.9){
    #G24
    b1 <- 0.838

  } else if(SI<=29.9){
    #G28
    b1 <- 0.838

  } else if(SI<=33.9){
    #G32
    b1 <-  0.839

  } else if(SI>=34){
    #G36
    b1 <- 0.844
  }

  return(
    (10^(-1.141))*(dominant_height_dm^b1)*(diameter_of_mean_basal_area_over_bark_cm^0.123)
  )



}

#' Calculate the number of self-thinned stems during a period as by Eriksson 1976.
#'
#' @source Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm. p. 107.
#'
#' @param dominant_height_m Dominant height of stand, meters.
#' @param number_trees_per_ha_period_start Number of trees per hectare at start of period.
#' @param basal_area_over_bark_m2_ha_before_thinning Basal area above bark m2/ha before thinning.
#'
#' @return numeric.
#' @export
Eriksson_1976_number_self_thinned_stems <- function(dominant_height_m,
                                                    number_trees_per_ha_period_start,
                                                    basal_area_over_bark_m2_ha_before_thinning,
                                                    SI
){

  basal_area_self_thinned <- Eriksson_1976_self_thinning(number_trees_per_ha_period_start = number_trees_per_ha_period_start,
                                                         dominant_height_m = dominant_height_m,
                                                         SI=SI)

  basal_area_weighted_mean_diameter <- basal_area_weighted_mean_diameter_cm(basal_area_m2_ha = basal_area_over_bark_m2_ha_before_thinning,
                                                                            stem_count = number_trees_per_ha_period_start)

  diameter_quotient <- Eriksson_1976_self_thinning_mean_diameter_quotient(dominant_height_m = dominant_height_m)

  basal_area_weighted_mean_diameter_self_thinned_stems <- basal_area_weighted_mean_diameter*diameter_quotient

  self_thinned_stems_per_ha <- ((basal_area_self_thinned*40000)/(basal_area_weighted_mean_diameter_self_thinned_stems^2*pi))

  return(self_thinned_stems_per_ha)
}

#' Quotient D4/D1 from Eriksson 1976.
#'
#' @description Gives the quotient between the basal area weighted mean stem diameter for self-thinned trees (D4) and the basal area weighted mean stem diameter for all trees on the plot (D1)
#'
#' @details R = 0.405
#'
#'
#'
#' @source Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm. p.107.
#'
#' @param dominant_height_m Dominant height of stand, in meters.
#'
#' @return Quotient D4/D1.
#' @export
Eriksson_1976_self_thinning_mean_diameter_quotient <- function(dominant_height_m){
  return(
    0.039*(dominant_height_m*10)^0.517
  )
}

#' Eriksson 1976 self thinning
#' @source From p. 86-88 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @description Author recommends that the function not be used when stems per ha exceed 4500 and dominant height at the same time is more than 15 m. Author still sees this as the best available information when it was written.
#' @param number_trees_per_ha_period_start Number of trees per hectare at the start of the period.
#' @param dominant_height_m Dominant height, m.
#' @param SI100 Site Index H100 Spruce from Hägglund 1972/1973, e.g. [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()], [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#'
#' @return Annual self thinning in basal area over bark per hectare, m2.
#' @export
Eriksson_1976_self_thinning <- function(number_trees_per_ha_period_start, dominant_height_m, SI){

  dominant_height_dm <- dominant_height_m*10

  if(number_trees_per_ha_period_start>4500 && dominant_height_dm>150){
    warning("Correction needed to Ekö PM self-thinning function. Using Eriksson 1976 correction from p. 107.")
    number_trees_per_ha_period_start <- 4500 + 0.1*(number_trees_per_ha_period_start-4500)
  }


  if(SI<=17.9){
    #G16
    b1 <- 0.524
    b2 <- 3.505

  } else if(SI<=21.9){
    #G20
    b1 <- 1.858
    b2 <- 3.376

  } else if(SI<=25.9){
    #G24
    b1 <- 2.613
    b2 <- 3.356

  } else if(SI<=29.9){
    #G28
    b1 <- 2.872
    b2 <- 3.347

  } else if(SI<=33.9){
    #G32
    b1 <- 3.229
    b2 <- 3.458

  } else if(SI>=34){
    #G36
    b1 <- 2.646
    b2 <- 3.479
  }

  return(
    3.25*(10^-10)*((number_trees_per_ha_period_start / 1000)^b1)*(dominant_height_dm^b2)
  )
}

#' Volume and Stem Frequencies for even-aged Norway Spruce stands from the Great Yield
#' Investigation (Näslund 1971).
#'
#' @source Eriksson, H. 1976. Yield of Norway spruce in Sweden. Research Notes.
#' Dept. of. Forest Yield Research. Nr. 41. Royal College of Forestry.
#' Stockholm. p. 161-165; 279-284.
#'
#' @description This is an attempt to follow the instructions given by Eriksson.
#' From functions relating easily gathered information to a fitted stem and
#' volume distribution respectively, a skewed normal distribution is estimated.
#' From an expanded series, the frequency of each class is then calculated.
#'
#' @details TESTED ONLY FOR 2 CM class width!
#'
#' @param age Stand age at breast height (1.3 m)
#' @param dominant_height Dominant height, in metres.
#' @param QMD Quadratic Mean Diameter (cm).
#' @param volume Volume of stand, cu.m.
#' @param stems Stems per hectare.
#' @param class_width_cm Default 2 cm. Range between upper and lower class limit.
#' @param class_middle Arithmetic mean of the sum of the upper and lower limit.
#'
#' @return Frequency, in percent.
#' @export
#' @name Eriksson_frequency
#'
Eriksson_1976_volume_frequency <- function(
    age,
    dominant_height,
    QMD,
    volume,
    stems,
    class_width_cm=2,
    class_middle=1.5
){

  dominant_height <- dominant_height*10 #m to dm.

  #Standard deviation of the volume distribution
  volume_sd <- 0.028*(dominant_height^1.043)*(QMD^-0.260)*(age^0.076)

  #Coefficient of asymmetry for the volume distribution
  volume_asymmetry <- 9.757*(dominant_height^-0.289)*(QMD^0.048)*(age^-0.035)*((volume/100)^0.024)*(volume_sd^0.205)-3

  #Coefficient of excess for the volume distribution
  volume_excess <- 4.851*((volume_asymmetry+3)^-0.387)-3

  #Volume mean
  volumeMean= QMD + (0.000022*(dominant_height^3.238)*(QMD^-2.118)*(age^0.239)*((volume/100)^-0.274))


  #Cramér 1961 moment estimation of PDF of normal distribution.
  CramerNormalPDF <- function(x, mean, sd,skew,kurtosis,total){
    X <- (x-mean)/sd #X is number of sd from mean.

    phi0 <- function(X) (1/sqrt(2*pi))*exp(-((X^2)/2))
    phi3 <- function(X) (-X^3+3*X)*phi0(X)
    phi4 <- function(X) (X^4 - 6*X^2 +3)*phi0(X)
    phi6 <- function(X) (X^6 - 15*X^4 + 45*X^2 -15)*phi0(X)

    return(
      ((total)/sd) *(phi0(X)-(skew/factorial(3))*phi3(X)+(kurtosis/factorial(4))*phi4(X)+ ((10*skew^2)/factorial(6))*phi6(X))
    )
  }

  volume_frequency <- CramerNormalPDF(
    x=class_middle,
    mean=volumeMean,
    sd = volume_sd,
    skew = volume_asymmetry,
    kurtosis = volume_excess,
    total = 100*class_width_cm #? Works for 2 cm classes. Don't know why.
  )



  return(
    volume_frequency
  )





}


#' @rdname Eriksson_frequency
#' @export
Eriksson_1976_stem_frequency <- function(
    age,
    dominant_height,
    QMD,
    stems,
    class_width_cm=2,
    class_middle=1.5
){

  dominant_height <- dominant_height*10 #m to dm.

  #Standard deviation of the diameter distribution.
  diameter_sd <- 0.01*(dominant_height^1.671)*(QMD^-0.855)*(stems/1000)^-0.182

  #Coefficient of asymmetry for the diameter distribution
  diameter_asymmetry <- 1.528*(dominant_height^0.405)*(QMD^-0.530)*((stems/1000)^-0.017)-3

  #Coefficient of excess for the diameter distribution
  diameter_excess <- 1.844*(dominant_height^0.276)*(QMD^0.014)*((stems/1000)^-0.163)*(age^-0.050)*(diameter_sd^-0.477)-3

  #Get arithmetic mean diameter (Cajanus theorem)
  diameterArithmetic <- sqrt(QMD^2 - diameter_sd^2)


  CramerNormalPDF <- function(x, mean, sd,skew,kurtosis,total){
    X <- (x-mean)/sd #X is number of sd from mean.

    phi0 <- function(X) (1/sqrt(2*pi))*exp(-((X^2)/2))
    phi3 <- function(X) (-X^3+3*X)*phi0(X)
    phi4 <- function(X) (X^4 - 6*X^2 +3)*phi0(X)
    phi6 <- function(X) (X^6 - 15*X^4 + 45*X^2 -15)*phi0(X)

    return(
      ((total)/sd) *(phi0(X)-(skew/factorial(3))*phi3(X)+(kurtosis/factorial(4))*phi4(X)+ ((10*skew^2)/factorial(6))*phi6(X))
    )
  }

  stem_frequency <- CramerNormalPDF(
    x=class_middle,
    mean=diameterArithmetic,
    sd = diameter_sd,
    skew = diameter_asymmetry,
    kurtosis = diameter_excess,
    total = 100*class_width_cm #? Works for 2 cm classes. Don't know why.
  )

  return(
    stem_frequency
  )


}

#'Annual Volume Increment under bark per hectare, m3
#' @source From p. 64;65. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @param basal_area_ha_before_thinning_m2 Basal area under bark per hectare before thinning, m2.
#' @param site_index Spruce
#' @param thinning_percent_ba_under_bark Thinning as percentage of basal area under bark before thinning.
#' @param diameter_of_mean_basal_area_of_thinned_trees_cm Diameter corresponding to mean basal area under bark of the thinned trees, cm.
#' @param diameter_of_mean_basal_area_before_thinning_cm Diameter corresponding to the mean basal area of the stand before thinning, cm.
#' @param number_thinned_trees_per_ha Number of thinned trees per hectare.
#' @param number_trees_per_ha_before_thinning Number of trees per hectare in the stand before thinning.
#' @param increment_period_years Length of increment period, years.
#' @param dominant_height_dm Dominant height, dm.
#' @param age_bh_100_largest_trees_per_ha_years Age at breast height of the 100 largest trees per hectare, years.
#'
#'
#'
#' @return Annual increment in basal area under bark per hectare, m2.
#' @export
Eriksson_1976_volume_increment_under_bark_Spruce <- function(basal_area_ha_before_thinning.m2,
                                                             site_index,
                                                             thinning_percent_ba_under_bark,
                                                             diameter_of_mean_basal_area_of_thinned_trees_cm,
                                                             diameter_of_mean_basal_area_before_thinning_cm,
                                                             number_thinned_trees_per_ha,
                                                             number_trees_per_ha_before_thinning,
                                                             increment_period_years,
                                                             dominant_height_dm,
                                                             age_bh_100_largest_trees_per_ha_years
){

  if(site_index<=17.9){
    #G16
    b1 <- 0.253
    b2 <- -0.100
    b3 <- 0.067
    b4 <- 1.103
    b5 <- -0.906
    b6 <- -0.296
    c <- 0.447
  } else if(site_index<=21.9){
    #G20
    b1 <- 0.370
    b2 <- -0.107
    b3 <- 0.080
    b4 <- 1.015
    b5 <- -0.939
    b6 <- -0.233
    c <- 0.488
  } else if(site_index<=25.9){
    #G24
    b1 <- 0.435
    b2 <- -0.109
    b3 <- 0.109
    b4 <- 0.831
    b5 <- -0.730
    b6 <- -0.125
    c <- 0.498
  } else if(site_index<=29.9){
    #G28
    b1 <- 0.322
    b2 <- -0.108
    b3 <- 0.042
    b4 <- 0.907
    b5 <- -0.795
    b6 <- -0.109
    c <- 0.568
  } else if(site_index>=30){
    #G32
    b1 <-  0.358
    b2 <- -0.097
    b3 <- 0.035
    b4 <- 0.868
    b5 <- -0.861
    b6 <- -0.042
    c <- 0.763
  }

  return(
    0.141*basal_area_ha_before_thinning.m2^b1*
      (thinning_percent_ba_under_bark-0.01)^b2*
      ((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm+0.1)*(100*number_thinned_trees_per_ha/number_trees_per_ha_before_thinning+0.01))^0.062*
      (increment_period_years/10)^b3*
      dominant_height_dm^b4*
      (age_bh_100_largest_trees_per_ha_years/10)^b5*
      (13.778*((age_bh_100_largest_trees_per_ha_years/10)^c)*((number_trees_per_ha_before_thinning/1000)^-0.052))^b6
  )

}

