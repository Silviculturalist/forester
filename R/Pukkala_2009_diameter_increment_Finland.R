#' Diameter Increment for Uneven-sized Spruce forests in Finland from Pukkala 2009
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#'
#' @details Pine model is based on 15282 trees. R^2 = 0.4, SD of resid.= 0.527. Snowdon correction = 1.110
#'
#' @param BAL_Spruce Basal area of Spruce trees larger than the subject tree (m2 / ha)
#' @param BAL_other Basal area of trees other than Spruce larger than the subject tree (m2/ha)
#' @param BA_m2_ha Basal Area of all trees larger than 5 cm dbh. m2/ha.
#' @param diameter_cm Diameter of subject tree at breast height 1.3m, cm.
#' @param vegetation One of [Finland_vegetation_types()]
#' @param temperature_sum Degree days
#'
#' @name Pukkala_2009_diameter_increment_Finland_Pine
#' @md
#'
#' @return 5 year over-bark diameter increment.
#' @export
Pukkala_2009_diameter_increment_Finland_Pine <- function(
  BAL_Spruce,
  BAL_other,
  BA_m2_ha,
  diameter_cm,
  vegetation,
  temperature_sum
){

  MT = vegetation=="MT"
  VT = vegetation=="VT"
  CT = vegetation=="CT"
  ClT = vegetation=="ClT"

  a1=-7.758
  a2=-0.0530
  a3=-0.0335
  a4=-0.266
  a5=0.237
  a6=-0.000901
  a7=-0.238
  a8=-0.333
  a9=-0.612
  a10=-1.201
  a11=1.229

    return(
      exp((a1+a2*BAL_Spruce + a3*BAL_other + a4*ln(BA) + a5*sqrt(diameter_cm) + a6*(diameter_cm^2) + a7*MT + a8*VT + a9*CT + a10*ClT + a11*log(TS)))*1.110
    )

}

#' @details Spruce model is based on 24014 trees. R^2 = 0.568, SD of resid.= 0.604. Snowdon correction = 1.124
#' @rdname Pukkala_2009_diameter_increment_Finland_Pine
#' @export
#'
Pukkala_2009_diameter_increment_Finland_Spruce <- function(
  BAL_Spruce,
  BAL_other,
  BA_m2_ha,
  diameter_cm,
  vegetation,
  temperature_sum
){

  MT = vegetation=="MT"
  VT = vegetation=="VT"
  CT = vegetation=="CT"
  ClT = vegetation=="ClT"

  a1=-5.317
  a2=-0.0106
  a3=-0.0430
  a4=-0.486
  a5=0.455
  a6=-0.000927
  a7=-0.180
  a8=-0.450
  a9=-0.929
  a10=0
  a11=0.823

  return(
    exp((a1+a2*BAL_Spruce + a3*BAL_other + a4*ln(BA) + a5*sqrt(diameter_cm) + a6*(diameter_cm^2) + a7*MT + a8*VT + a9*CT + a10*ClT + a11*log(TS)))*1.124
  )

}

#'@details @details Betula model is based on 8219 trees. R^2 = 0.541, SD of resid.= 0.599. Snowdon correction = 1.127
#'@rdname Pukkala_2009_diameter_increment_Finland_Pine
#'@export

Pukkala_2009_diameter_increment_Finland_Betula <- function(
  BAL_Spruce,
  BAL_other,
  BA_m2_ha,
  diameter_cm,
  vegetation,
  temperature_sum
){

  MT = vegetation=="MT"
  VT = vegetation=="VT"
  CT = vegetation=="CT"
  ClT = vegetation=="ClT"

  a1=-11.873
  a2=-0.0304
  a3=-0.0474
  a4=-0.173
  a5=0.446
  a6=-0.00123
  a7=-0.121
  a8=-0.227
  a9=-0.524
  a10=0
  a11=1.627

  return(
    exp((a1+a2*BAL_Spruce + a3*BAL_other + a4*ln(BA) + a5*sqrt(diameter_cm) + a6*(diameter_cm^2) + a7*MT + a8*VT + a9*CT + a10*ClT + a11*log(TS)))*1.127
  )

}
