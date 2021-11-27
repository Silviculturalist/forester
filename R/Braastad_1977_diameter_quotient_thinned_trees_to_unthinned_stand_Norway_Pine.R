#' Diameter Quotient of Thinned trees to remaining Standing trees for Scots Pine in Norway from Brastaad 1977.
#'
#' @source Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 274 function (7)
#'
#' @param dominant_height Dominant height of stand, m.
#'
#' @return Ratio Diameter of thinned trees / Diameter of unthinned stand.
#' @export
#'
#' @examples
Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine <- function(
  dominant_height
){
  return(
    0.4855 + 0.0355*dominant_height - 0.000845*(dominant_height^2)
  )
}
