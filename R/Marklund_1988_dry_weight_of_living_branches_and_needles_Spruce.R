#' Dry weight of living branches and needles for Spruce, from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, in metres.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param age_at_breast_height Age at breast height.
#' @param Dmax_10_m_radius Diameter of thickest tree on a plot with a 10 metre radius.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#' @param dominant_species Species for which SI was estimated, "Pinus sylvestris"
#' or "Picea abies".
#' @param SI Site index (m.)
#'
#' @return Dry weight of living branches and needles for Spruce, in kilograms.
#' @export
Marklund_1988_dry_weight_of_living_branches_and_needles_Spruce <- function(
  diameter.cm,
  height.m,
  crown_base_height.m,
  age_at_breast_height,
  Dmax_10_m_radius,
  five_years_diameter_increment.mm,
  dominant_species,
  SI

){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }


  #G11
  try(
dry_weight_of_living_branches_and_needles <-
  -1.2804+
  +8.5242*(diameter.cm/(diameter.cm+13))
)

#G12
  try(
dry_weight_of_living_branches_and_needles <-
  -1.2063+
  +10.9708*(diameter.cm/(diameter.cm+13))+
  -0.0124*height.m+
  -0.4923*log(height.m)
)

#G13
  try(
dry_weight_of_living_branches_and_needles <-
  -1.1209+
  +10.4621*(diameter.cm/(diameter.cm+13))+
  -1.5211*log(height.m)+
  +1.0179*log(height.m-crown_base_height.m)+
  +0.0121*SI*pinus+
  +0.0110*SI*picea
)


#G14
  try(
dry_weight_of_living_branches_and_needles <-
  -1.3242+
  +8.0106*(diameter.cm/(diameter.cm+13))+
  -0.9993*log(height.m)+
  +0.6623*log(height.m-crown_base_height.m)+
  +0.5003*log(crown_radius.m)+
  +0.2248*log(age_at_breast_height)+
  +0.2518*log(five_years_diameter_increment.mm)+
  -0.1640*log(Dmax_10_m_radius)
)

  return(exp(dry_weight_of_living_branches_and_needles))
}
