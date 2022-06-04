#' Dry weight of stump and roots of a Pine from Marklund 1988
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param dominant_species Dominant species on the plot.
#' @param latitude Latitude, RT90.
#' @param age_at_breast_height Age at breast height.
#' @param Dry_soil If the ground water level is more than 2 m deep, TRUE,
#' otherwise FALSE.
#' @param Moist_soil If the ground water level is less than 1 m deep, TRUE,
#' otherwise FALSE.
#' @param peat If the humus layer is thicker than 30 cm, TRUE, otherwise
#' FALSE.
#' @param mobile_ground_water_longer_periods If surface/subsurface water flow
#' occurs during longer periods, TRUE, otherwise FALSE.
#' @param mobile_ground_water_shorter_periods If surface/subsurface water flow
#' occurs during shorter periods, TRUE, otherwise FALSE.
#' @param altitude Altitude, in km.
#'
#' @return Dry weight of stump and roots, in kilograms.
#' @export
Marklund_dry_weight_of_stump_and_roots_Pine <- function(
  diameter.cm,
  dominant_species,
  latitude,
  age_at_breast_height,
  Dry_soil,
  Moist_soil,
  peat,
  mobile_ground_water_longer_periods,
  mobile_ground_water_shorter_periods,
  altitude


){
  #T25
  try(
  dry_weight_stump_and_roots <-
  -3.3913+
  +11.1106*(diameter.cm/(diameter.cm+12))
)

  #T26
  try(
dry_weight_stump_and_roots <-
  -1.5530+
  +11.2246*(diameter.cm/(diameter.cm+12))+
  -0.0314*SI*pinus+
  -0.0268*SI*picea+
  -0.0192*latitude
)
  #T27
  try(
dry_weight_stump_and_roots <-
  -3.1628+
  +10.7181*(diameter.cm/(diameter.cm+12))+
  +0.0952*log(age_at_breast_height)+
  -0.0168*SI*pinus+
  -0.0136*SI*picea+
  -0.0808*Dry_soil+
  +0.2165*Moist_soil+
  +0.3088*peat+
  -0.1655*mobile_ground_water_longer_periods+
  -0.1070*mobile_ground_water_shorter_periods+
  -0.5221*altitude
)

  return(exp(dry_weight_stump_and_roots))
}
