#' Ingrowth model for Uneven-sized stands in Finland
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#' @description Baskerville Correction has been added to the constant.
#' @details The Spruce model is based on 10545 living trees, 1127 dead trees. Best threshold probability is c. 0.5-0.6. Percentage of correct predictions 90.7.
#' @param stems_ha_spruce Spruce stems per hectare >5 cm dbh.
#' @param stems_ha_other Stems other than Spruce per hectare > 5 cm dbh.
#' @param vegetation One of [forester::Finland_vegetation_types()]
#' @param BA_m2_ha Total Basal Area (m2/ha) of trees >5 cm dbh.
#'
#' @return Number of ingrowth per hectare.
#' @name Pukkala_2009_ingrowth_Finland_Spruce
#' @export
#'
Pukkala_2009_ingrowth_Finland_Spruce <- function(
  stems_ha_spruce,
  stems_ha_other,
  vegetation,
  BA_m2_ha
){

  MT_minus = vegetation%in%c("MT","HMT","VT","EMT","CT","MClT","ClT")

  a1=4.688
  a2=-0.712
  a3=0
  a4=0.083
  a5=0
  a6=-0.567

  return(
  exp(a1 + a2*sqrt(BA_m2_ha) + a3*log(BA_m2_ha) + a4*sqrt(stems_ha_spruce) + a5*sqrt(stems_ha_other) + a6*MT_minus) - 1
  )


}

#' @rdname Pukkala_2009_ingrowth_Finland_Spruce
#' @details Betula and Pine model is based on 3230 living trees, 289 dead trees. Best threshold probability = 0.6. Percentage of correct predictions = 92.6
#' @export

Pukkala_2009_ingrowth_Finland_Betula_Pine <- function(
  stems_ha_spruce,
  stems_ha_other,
  vegetation,
  BA_m2_ha
){

  MT_minus = vegetation%in%c("MT","HMT","VT","EMT","CT","MClT","ClT")

  a1=6.154
  a2=0
  a3=-1.683
  a4=0
  a5=0.0642
  a6=0

  return(
    exp(a1 + a2*sqrt(BA_m2_ha) + a3*log(BA_m2_ha) + a4*sqrt(stems_ha_spruce) + a5*sqrt(stems_ha_other) + a6*MT_minus) -1
  )


}
