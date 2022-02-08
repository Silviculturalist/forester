#' Stem density in even aged stands of Scots Pine in Norway
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C., Astrup R.
#' (2022). A stand-level growth and yield model for thinned and unthinned
#' even-aged Scots pine forests in Norway. Silva Fennica vol. 56 no. 1 article
#' id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: -0.11108, Mean absolute error: 46.25402, relative MAE: 2.74474
#'
#' @param SI40 Dominant height of stand at a total age of 40 years.
#' @param stems_ha Stem density per hectare.
#' @param BA_before_thinning Stand Basal Area before thinning m2/ha
#' @param BA_after_thinning Stand Basal Area after thinning m2/ha
#' @param age Total stand age.
#' @param age2 Total stand age at time point 2.
#'
#' @return Stem density.
#' @export

Kuehne_2022_stem_density_Norway_Scots_Pine <- function(
  SI40,
  stems_ha,
  BA_before_thinning,
  BA_after_thinning,
  age,
  age2
){

  b1 <- -1.56856
  b2 <- 0.00284
  b3 <- 4.14779
  b4 <- 4.87715

  (
    (stems_ha^b1) + (b2 * (BA_after_thinning/BA_before_thinning) * ((SI40/10000)^b3) * (age2*b4 - age*b4))
    )^(1/b1)

}
