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
