#' Natural Mortality Rate for Individual Trees in Norway.
#'
#' @details Calculates the natural mortality rate for Norway Spruce,
#' Scots Pine, Betula spp., European Aspen and other Broadleaves in Norway.
#' Based on 1771 permanent plots of size 100 m^2.
#'
#' @source Tuhus, E. 1995. Naturlig avgang av trær. (Natural mortality of trees) Rapp. Skogforsk. 6/97: 1-28. ISBN 82-7169-836-2. ISSN 0803-2858.
#'
#' @param diameter_cm Diameter of the tree at breast height (1.3 m)
#' @param BA_m2_ha Basal Area m2/ha.
#' @param BA_m2_ha_Spruce Basal Area m2/ha which is Spruce.
#' @param period_length Length of the period.
#'
#' @return Mortality Rate.
#' @export
#' @name Tuhus_1995_natural_mortality_Norway_Norway_Spruce
#'
Tuhus_1995_natural_mortality_Norway_Norway_Spruce <- function(
  diameter_cm,
  BA_m2_ha,
  BA_m2_ha_Spruce,
  period_length
){
  return(
    (1 + exp(-(5.4360 + 0.1972*sqrt(diameter_cm) + -0.0253*BA_m2_ha + -0.1931*BA_m2_ha_Spruce)))^(-period_length)
  )
}

#'
#' @rdname Tuhus_1995_natural_mortality_Norway_Norway_Spruce
#' @param dominant_height_m Dominant height of the stand in metres.
#' @export

Tuhus_1995_natural_mortality_Norway_Pine <- function(
  diameter_cm,
  BA_m2_ha,
  dominant_height_m,
  period_length
){
  return(
    (1 + exp(-(3.7502 + 0.3726*sqrt(diameter_cm) + -0.0227*BA_m2_ha + -0.1587*dominant_height_m)))^(-period_length)
  )
}

#'
#'@rdname Tuhus_1995_natural_mortality_Norway_Norway_Spruce
#'@param stand_age Age of the stand (at breast height?)
#'@export

Tuhus_1995_natural_mortality_Norway_Birch_Aspen <- function(
  diameter_cm,
  BA_m2_ha,
  stand_age,
  period_length
){
  return(
    (1 + exp(-(4.7971 + 0.1134*sqrt(diameter_cm) + -0.0161*BA_m2_ha + -0.0122*stand_age)))^(-period_length)
  )
}

#'
#'@rdname Tuhus_1995_natural_mortality_Norway_Norway_Spruce
#'@param stand_age Age of the stand (at breast height?)
#'@export

Tuhus_1995_natural_mortality_Norway_Broadleaf <- function(
  diameter_cm,
  BA_m2_ha,
  stand_age,
  period_length
){
  return(
    (1 + exp(-(3.7670 + 0.1160*sqrt(diameter_cm) + -0.0136*BA_m2_ha + -0.0084*stand_age)))^(-period_length)
  )
}
