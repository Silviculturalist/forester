#' Basal area in even aged stands of Scots Pine in Norway
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C., Astrup R.
#' (2022). A stand-level growth and yield model for thinned and unthinned
#' even-aged Scots pine forests in Norway. Silva Fennica vol. 56 no. 1 article
#' id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: 0.02192, Mean absolute error: 0.74455, relative MAE: 2.71853
#'
#' @param SI40 Dominant height of stand at a total age of 40 years.
#' @param stems_ha Stem density per hectare.
#' @param stems_ha2 Stem density per hectare at time point 2.
#' @param BA Basal Area at time point 1.
#' @param BA_before_thinning Stand Basal Area before thinning m2/ha
#' @param BA_removed Stand Basal Area removed by thinning m2/ha
#' @param age Total stand age.
#' @param age2 Total stand age at time point 2.
#' @param dominant_height Dominant height at time point 1.
#' @param dominant_height2 Dominant height at time point 2.
#' @param age_thin Total stand age at time of thinning.
#'
#' @return Basal area m2 ha.
#' @export

Kuehne_2022_BA_Norway_Scots_Pine <- function(
  SI40,
  stems_ha,
  stems_ha2,
  BA,
  BA_before_thinning,
  BA_removed,
  age,
  age2,
  age_thin,
  dominant_height,
  dominant_height2

){

  b1 <- 1.46553
  b2 <- 0.52449
  b3 <- 0.17701
  b4 <- 16.53755
  b5 <- -386.71670

  return(
    exp(
      (age/age2)*log(BA) + b1*(1-(age/age2)) + b2*(log(dominant_height2) - (age/age2)*log(dominant_height)) + b3*(log(stems_ha2) - (age/age2)*log(stems_ha)) + b4*((log(stems_ha2) - log(stems_ha))/age2) + b5*(((BA_removed/BA_before_thinning)/age_thin)*((1/age2)-(1/age)))
    )
  )

}
