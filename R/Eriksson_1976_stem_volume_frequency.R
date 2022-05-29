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
#' N.B. The values from this function seems to differ from the original output.
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

  #Expansion of the normal distribution
  phi_0 <- function(X){return(

    (1/(sqrt(2*pi)))*exp(-((X^2)/2))

  )}

  phi_3 <- function(X){return(
    ((-X^3)+3*X)*phi_0(X)
  )}

  phi_4 <- function(X){return(
    ((X^4)-(6*(X^2)) + 3)*phi_0(X)
  )}

  phi_6 <- function(X){return(
    ((X^6) - (15*(X^4)) + (45*(X^2)) -15)*phi_0(X)
  )}

  approximated_normal_frequency <- function(class_middle, #theoretical mean of actual diameter class.
                                            QMD, #QMD
                                            standard_deviation, #of chosen distribution
                                            asymmetry_coefficient, #for chosen distribution
                                            excess_coefficient, #for chosen distribution
                                            total){ #total stems or volume.

    X <- (class_middle - QMD)/standard_deviation

    return(
      (total/standard_deviation) * (
        phi_0(X) - (asymmetry_coefficient/factorial(3))*phi_3(X) +
          (excess_coefficient/factorial(4))*phi_4(X) +
          ((10*(asymmetry_coefficient^2))/factorial(6))*phi_6(X))
    )
  }

  volume_frequency <- approximated_normal_frequency(
    class_middle=class_middle,
    QMD=QMD,
    standard_deviation = volume_sd,
    asymmetry_coefficient = volume_asymmetry,
    excess_coefficient = volume_excess,
    total=100*class_width_cm #for relative values.
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

  #Expansion of the normal distribution
  phi_0 <- function(X){return(

    (1/(sqrt(2*pi)))*exp(-((X^2)/2))

  )}

  phi_3 <- function(X){return(
    ((-X^3)+3*X)*phi_0(X)
  )}

  phi_4 <- function(X){return(
    ((X^4)-(6*(X^2)) + 3)*phi_0(X)
  )}

  phi_6 <- function(X){return(
    ((X^6) - (15*(X^4)) + (45*(X^2)) -15)*phi_0(X)
  )}

  approximated_normal_frequency <- function(class_middle, #theoretical mean of actual diameter class.
                                            QMD, #QMD
                                            standard_deviation, #of chosen distribution
                                            asymmetry_coefficient, #for chosen distribution
                                            excess_coefficient, #for chosen distribution
                                            total){ #total stems or volume.

    X <- (class_middle - QMD)/standard_deviation

    return(
      (total/standard_deviation) * (
        phi_0(X) - (asymmetry_coefficient/factorial(3))*phi_3(X) +
          (excess_coefficient/factorial(4))*phi_4(X) +
          ((10*(asymmetry_coefficient^2))/factorial(6))*phi_6(X))
    )
  }

  stem_frequency <- approximated_normal_frequency(
    class_middle=class_middle,
    QMD=QMD,
    standard_deviation = diameter_sd,
    asymmetry_coefficient = diameter_asymmetry,
    excess_coefficient = diameter_excess,
    total=100*class_width_cm #for relative values.
  )




  return(
    stem_frequency
  )


}
