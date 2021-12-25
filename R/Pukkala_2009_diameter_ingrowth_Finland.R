#' Diameter of Ingrowth in Uneven-sized stands in Finland
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#'
#' @details Spruce model is based on 170 plot observations, R^2= 0.586, SD of resid. = 0.0473.
#' @name Pukkala_2009_diameter_ingrowth_Finland_Spruce
#' @inheritParams Pukkala_2009_ingrowth_Finland_Spruce
#'
#' @return Diameter cm dbh.
#' @export
#'
Pukkala_2009_diameter_ingrowth_Finland_Spruce <- function(
  BA_m2_ha,
  stems_ha_spruce,
  stems_ha_other,
  vegetation
){

  MT = vegetation=="MT"
  VT_minus = vegetation%in%c("VT","EMT","CT","MClT","ClT")

  a1=2.004
  a2=-0.101
  a3=-0.0176
  a4=-0.0646

  return(
    exp(a1 + a2*log(BA_m2_ha) + a3*MT + a4*VT_minus)
  )

}

#' @rdname Pukkala_2009_diameter_ingrowth_Finland_Spruce
#' @export
#' @details @details Pine & Betula model is based on 141 plot observations, R^2= 0.469, SD of resid. = 0.0730.

Pukkala_2009_diameter_ingrowth_Finland_Betula_Pine <- function(
  BA_m2_ha,
  stems_ha_spruce,
  stems_ha_other,
  vegetation
){

  MT = vegetation=="MT"
  VT_minus = vegetation%in%c("VT","EMT","CT","MClT","ClT")

  a1=1.958
  a2=-0.0841
  a3=-0.0425
  a4=-0.0556

  return(
    exp(a1 + a2*log(BA_m2_ha) + a3*MT + a4*VT_minus)
  )

}
