#' Volume in even-aged stands of Scots Pine in Norway.
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C., Astrup R.
#' (2022). A stand-level growth and yield model for thinned and unthinned
#' even-aged Scots pine forests in Norway. Silva Fennica vol. 56 no. 1 article
#' id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: 0.13014, Mean absolute error: 3.92983, relative MAE: 1.97324
#'
#' @param BA Basal Area m2/ha.
#' @param BA_after_thinning Basal Area after thinning m2/ha
#' @param BA_before_thinning Basal Area before thinning m2 / ha.
#' @param age_thin Total stand age at thinning.
#' @param age Total stand age
#' @param dominant_height Dominant height of stand.
#'
#' @return Volume.
#' @export

Kuehne_2022_volume_Norway_Scots_Pine <- function(
  BA,
  BA_after_thinning,
  BA_before_thinning,
  age_thin,
  age,
  dominant_height
){

  b1 <- 0.65394
  b2 <- 0.96928
  b3 <- 0.91504
  b4 <- -2.05278
  b5 <- -0.06848

  return(
    b1 * (BA^b2) * (dominant_height^b3) * exp(b4/age)*(BA_after_thinning/BA_before_thinning)^(b5*(age_thin/age))
  )

}
