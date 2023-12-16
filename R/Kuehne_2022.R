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
#' Height trajectory for Scots Pine in Norway from Kuehne et al 2022.
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C.,
#'  Astrup R. (2022). A stand-level growth and yield model for thinned and
#'  unthinned even-aged Scots pine forests in Norway. Silva Fennica vol. 56
#'   no. 1 article id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: 0.01458, Mean absolute error: 0.34876, relative MAE: 2.18346
#'
#' @details Compared to Tveite (1976) [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#' and Sharma (2011), estimates a higher Site Index.
#' @param age Total stand age
#' @param age2 Total stand age at output.
#' @param dominant_height Dominant height, metres.
#' @param output One of "SIH100","Equation" or "Height" (default).
#'
#' @return One of "SIH100","Equation" or "Height".
#' @export
Kuehne_2022_height_trajectory_Norway_Scots_Pine <- function(
  age,
  age2,
  dominant_height,
  output="Height"
){

  b1 <- 68.41819
  b2 <- -24.04110
  b3 <- 1.46991

  X <- (dominant_height - b1) / (1 - b2*dominant_height*(age^-b3))


  if(output=="Height"){
    return(
      (b1 + X)/(1+b2*X*(age2^-b3))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~ (",b1," + ",X,")/(1+",b2,"*",X,"*(age^-",b3,"))")
    )
  }

  if(output=="SIH100"){
    return(
      (b1 + X)/(1+b2*X*(100^-b3))
    )
  }

}

Kuehne_2022_height_trajectory_Norway_Scots_Pine <-  Vectorize(Kuehne_2022_height_trajectory_Norway_Scots_Pine)
#' Reduction in number of trees and basal area in even-aged stands of Scots Pine in Norway
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C., Astrup R.
#' (2022). A stand-level growth and yield model for thinned and unthinned
#' even-aged Scots pine forests in Norway. Silva Fennica vol. 56 no. 1 article
#' id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: -0.00041, Mean absolute error: 0.03386, relative MAE: 5.17959
#'
#'
#' @param BA_before Basal area before thinning, m2 / ha
#' @param BA_after Basal area after thinning, m2 / ha
#'
#' @return Quotient
#' @export
#' @name Kuehne_quotient
#'
Kuehne_2022_stems_quotient_Norway_Scots_Pine <- function(
  BA_before,
  BA_after
){
  exp(-1.91239 + 1.94414*(BA_after/BA_before))
}


#' @rdname Kuehne_quotient
#' @export
#' @param stems_after Number of stems per ha after thinning.
#' @param stems_before Number of stems per ha before thinning
Kuehne_2022_BA_quotient_Norway_Scots_Pine <- function(
  stems_after,
  stems_before
){
  (log((stems_after/stems_before))--1.91239)/1.94414
}
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
