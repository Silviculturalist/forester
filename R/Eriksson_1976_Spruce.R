#' Yield of Norway spruce in Sweden
#'
#' @source Eriksson, Harry. 1976. Granens Produktion i Sverige: Yield of Norway spruce in Sweden. Research Notes Nr. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @param age Age at breast height of 100 largest trees per hectare in years.
#' @param QMD_UB QMD under bark, cm.
#' @param QMD QMD over bark, cm.
#' @param QMD1 QMD (over bark?) of stand before thinning, cm.
#' @param QMDThinned_UB QMD under bark of thinned stems
#' @param BA_UB Basal area under bark per hectare, m2.
#' @param BA Basal area over bark per hectare, m2.
#' @param BA1 Basal area (under bark) per hectare before thinning.
#' @param SI SI H100 Spruce, meters. e.g. [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()][forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#' @param incrementPeriod Number of years to increment.
#' @param thinningPercent Thinning grade in percent of BA1.
#' @param stemsThinned Number of thinned stems per hectare.
#' @param dominant_height Dominant height, m.
#' @param stems Number of trees per hectare.
#' @param stems1 Number of trees per hectare before thinning.
#' @param planted If site was planted, TRUE. If the site was founded by pre-commercial thinning to a certain plant spacing, FALSE.
#' @param BA5CV Group structure index (Coefficient of variation for the basal area under bark on 5-meter circular plots inside the sample plot.) (\%).
#' @name Eriksson1976

#' @title Bark Percent addition
#' @rdname Eriksson1976
#' @export
#' @return (\%)
Eriksson_1976_bark_procent <- function(age, SI, QMD_UB, BA_UB){

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
    45.08*(QMD_UB^b1)*(BA_UB^-0.281)*(age^0.125)
  )
}


#' @title Bark subtraction procent
#' @return \%
#' @rdname Eriksson1976
#' @export
Eriksson_1976_bark_subtraction_procent <- function(QMD,SI, BA,age){

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
    9.50*(QMD^b1)*(BA^0.135)*(age^0.112)
  )
}

#' @title Basal area m2 per hectare before first thinning
#' @details Author does not recommend the use of this function if dominant height exceeds 16 meters, since self thinning increases rapidly if the number of stems then also is high.
#' In ranges between 1'100 and 10'000 stems per ha and a dominant height between 7 and 16 metres the function should provide acceptable results. Results may be less accurate at lower site indexes due to no underlying data.
#' @return Basal Area, m2 / ha
#' @rdname Eriksson1976
#' @export

Eriksson_1976_basal_area_before_first_thinning <- function(dominant_height, stems,planted){

  b <- ifelse(planted,0.355,0.319)

  return(
    1.0111*(((dominant_height)-1.3)^1.230)*((stems/1000)^b)
  )
}

#' @title Annual Increment in Basal Area in Spruce Stand
#' @details Coefficient of variation for basal area on a 5 plot has been calculated as per p.112.
#' @rdname Eriksson1976
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
Eriksson_1976_basal_increment_under_bark <- function(BA1,
                                                     SI,
                                                     thinningPercent,
                                                     QMDThinned_UB,
                                                     QMD1,
                                                     stemsThinned,
                                                     stems1,
                                                     incrementPeriod,
                                                     dominant_height,
                                                     age
){

  if(SI<=17.9){
    #G16
    b1 <- 0.250
    b2 <- -0.046
    b3 <- 0.049
    b4 <- 0.033
    b5 <- -0.748
    b6 <- -0.308
    c <- 0.447
  } else if(SI<=21.9){
    #G20
    b1 <- 0.213
    b2 <- -0.055
    b3 <- 0.085
    b4 <- 0.460
    b5 <- -0.875
    b6 <- -0.313
    c <- 0.488
  } else if(SI<=25.9){
    #G24
    b1 <- 0.304
    b2 <- -0.056
    b3 <- 0.126
    b4 <- 0.035
    b5 <- -0.571
    b6 <- -0.154
    c <- 0.498
  } else if(SI<=29.9){
    #G28
    b1 <- 0.260
    b2 <- -0.059
    b3 <- 0.034
    b4 <- 0.112
    b5 <- -0.667
    b6 <- -0.066
    c <- 0.568
  } else if(SI>=30){
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
    2.635*BA1^b1*
      (thinningPercent+0.01)^b2*
      ((QMDThinned_UB/QMD1+0.1)*(100*stemsThinned/stems1+0.01))^0.024*
      incrementPeriod^b3*
      (dominant_height*10)^b4*
      age^b5*
      (13.778*((age/10)^c)*((stems1/1000)^-0.052))^b6
  )

}

#' @title Annual increment in dry weight of stem wood, tons per hectare.
#' @rdname Eriksson1976
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
Eriksson_1976_dry_weight_increment_tons_per_hectare <- function(BA1,
                                                                SI,
                                                                thinningPercent,
                                                                QMDThinned_UB,
                                                                QMD1,
                                                                stemsThinned,
                                                                stems1,
                                                                incrementPeriod,
                                                                dominant_height,
                                                                age,
                                                                BA5CV){
  if(SI<=17.9){
    #G16
    b1 <- 0.320
    b2 <- -0.131
    b3 <- 0.065
    b4 <- 0.991
    b5 <- -0.842
    b6 <- -0.295
  } else if(SI<=21.9){
    #G20
    b1 <- 0.455
    b2 <- -0.137
    b3 <- 0.064
    b4 <- 0.900
    b5 <- -0.866
    b6 <- -0.255
  } else if(SI<=25.9){
    #G24
    b1 <- 0.523
    b2 <- -0.138
    b3 <- 0.092
    b4 <- 0.765
    b5 <- -0.702
    b6 <- -0.194
  } else if(SI<=29.9){
    #G28
    b1 <- 0.399
    b2 <- -0.136
    b3 <- 0.012
    b4 <- 0.821
    b5 <- -0.786
    b6 <- -0.131
  } else if(SI>=30){
    #G32
    b1 <-  0.442
    b2 <- -0.125
    b3 <- 0.021
    b4 <- 0.770
    b5 <- -0.814
    b6 <- -0.065
  }

  return(
    0.072*((BA1)^b1)*((thinningPercent+0.01)^b2)*((((QMDThinned_UB/QMD1)+0.1)*(((100*stemsThinned)/stems1)+0.01))^0.081)*((incrementPeriod/10)^b3)*((dominant_height*10)^b4)*((age/10)^b5)*(BA5CV^b6)
  )
}

#' @title Form height for spruce stands in Eriksson 1976
#' @return Form Height: F = VOL / BA
#' @rdname Eriksson1976
#' @export
Eriksson_1976_form_height <- function(dominant_height, SI, QMD){
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
    (10^(-1.141))*((dominant_height*10)^b1)*(QMD^0.123)
  )



}


#' Quotient between self-thinned QMD and QMD at start of period.
#' @details R = 0.405. Gives the quotient between the quadratic mean diameter for self-thinned trees (QMD4) and the quadratic mean diameter for all trees on the plot (QMD1)
#' @return Quotient of QMD of self-thinned stems to QMD at start of period.
#' @rdname Eriksson1976
#' @export
Eriksson_1976_self_thinning_diameter_quotient <- function(dominant_height){
  return(
    0.039*(dominant_height*10)^0.517
  )
}

#' @title Self-thinning in BA.
#' @details Author recommends that the function not be used when stems per ha exceed 4500 and dominant height at the same time is more than 15 m. Author still sees this as the best available information when it was written.
#' @return Annual self thinning in basal area over bark per hectare, m2.
#' @rdname Eriksson1976
#' @export
Eriksson_1976_self_thinning <- function(stems1, dominant_height, SI){

  dominant_height_dm <- dominant_height*10

  if(stems1>4500 && dominant_height_dm>150){
    warning("Correction needed to Ekö PM self-thinning function. Using Eriksson 1976 correction from p. 107.")
    stems1 <- 4500 + 0.1*(stems1-4500)
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
    3.25*(10^-10)*((stems1 / 1000)^b1)*(dominant_height_dm^b2)
  )
}


#' @title Calculate the number of self-thinned stems during a period as by Eriksson 1976.
#' @rdname Eriksson1976
#' @return Number of self-thinned stems per ha.
#' @export
Eriksson_1976_number_self_thinned_stems <- function(dominant_height,
                                                    stems1,
                                                    BA1,
                                                    SI
){

  BAThinned <- Eriksson_1976_self_thinning(stems1 = stems1,
                                                         dominant_height = dominant_height,
                                                         SI=SI)

  QMD1 <- quadratic_mean_diameter(Basal_area_m2_ha = BA1,stems_per_ha = stems1)

  diameter_quotient <- Eriksson_1976_self_thinning__diameter_quotient(dominant_height = dominant_height)

  QMD4 <- QMD1*diameter_quotient

  return((BAThinned*40000)/(QMD4^2*pi))
}


#' @title Volume increment under bark in Norway spruce stands.
#' @return Volume increment under bark in cubic meters per hectare.
#' @rdname Eriksson1976
#' @export
Eriksson_1976_volume_increment_under_bark_Spruce <- function(BA1,
                                                             SI,
                                                             thinningPercent,
                                                             QMDThinned_UB,
                                                             QMD1,
                                                             stemsThinned,
                                                             stems1,
                                                             incrementPeriod,
                                                             dominant_height,
                                                             age
){

  if(SI<=17.9){
    #G16
    b1 <- 0.253
    b2 <- -0.100
    b3 <- 0.067
    b4 <- 1.103
    b5 <- -0.906
    b6 <- -0.296
    c <- 0.447
  } else if(SI<=21.9){
    #G20
    b1 <- 0.370
    b2 <- -0.107
    b3 <- 0.080
    b4 <- 1.015
    b5 <- -0.939
    b6 <- -0.233
    c <- 0.488
  } else if(SI<=25.9){
    #G24
    b1 <- 0.435
    b2 <- -0.109
    b3 <- 0.109
    b4 <- 0.831
    b5 <- -0.730
    b6 <- -0.125
    c <- 0.498
  } else if(SI<=29.9){
    #G28
    b1 <- 0.322
    b2 <- -0.108
    b3 <- 0.042
    b4 <- 0.907
    b5 <- -0.795
    b6 <- -0.109
    c <- 0.568
  } else if(SI>=30){
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
    0.141*BA1^b1*
      (thinningPercent-0.01)^b2*
      ((QMDThinned_UB/QMD1+0.1)*(100*stemsThinned/stems1+0.01))^0.062*
      (incrementPeriod/10)^b3*
      (dominant_height*10)^b4*
      (age/10)^b5*
      (13.778*((age/10)^c)*((stems1/1000)^-0.052))^b6
  )

}


#Following two functions need more checking.


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
#' @name Eriksson_frequency


#' @title Volume Frequency for Norway spruce stands.
#' @rdname Eriksson_frequency
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


#' @title Stem Frequency Function for Norway spruce stands.
#' @rdname Eriksson_frequency
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

