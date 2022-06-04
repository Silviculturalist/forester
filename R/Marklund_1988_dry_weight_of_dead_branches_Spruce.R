#' Dry weight of dead branches for Spruce from Marklund 1988
#'
#' @param diameter.cm Diameter at breast height, cm.
#' @param height.m Height of tree, metres.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param Dmax_10_m_radius Diameter above bark of the thickest tree on a 10 m radius plot.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#'
#' @return Dry weight of dead branches for a Spruce, kilograms.
#' @export
Marklund_1988_dry_weight_of_dead_branches_Spruce <- function(
  diameter.cm,
  height.m,
  crown_base_height.m,
  Dmax_10_m_radius,
  five_years_diameter_increment.mm
){

  #G19
  try(
  dry_weight_of_dead_branches <-
    -4.3308+
    +9.9550*(diameter.cm/(diameter.cm+18))
  )

  #G20
  try(
  dry_weight_of_dead_branches <-
    -4.6351+
    +3.6518*(diameter.cm/(diameter.cm+18))+
    +0.0493*height.m+
    +1.0129*log(height.m)
  )

  #G21
  try(
  dry_weight_of_dead_branches <-
    -5.3924+
    +5.6333*(diameter.cm/(diameter.cm+18))+
    +2.7826*log(height.m)+
    -1.7460*log(height.m-crown_base_height.m)
  )

  #G22
  try(
  dry_weight_of_dead_branches <-
    -5.0472+
    +5.7144*(diameter.cm/(diameter.cm+18))+
    +1.7185*log(height.m)+
    -0.5287*log(height.m-crown_base_height.m)+
    -0.5739*log(five_years_diameter_increment.mm)+
    +0.2804*log(Dmax_10_m_radius)
  )

  return(exp(dry_weight_of_dead_branches))

}
