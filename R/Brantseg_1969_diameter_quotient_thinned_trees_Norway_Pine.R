#' Diameter Quotient of Thinned trees to remaining Standing trees for Scots Pine in Norway from Brantseg 1969.
#'
#' @source Brantseg, A. 1969. Furu sønnafjells. Produksjonstabeller. Meddr. norske SkogforsVes. 26: 1-291. p. 53 function 13. in
#' Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 274 function (6)
#'
#' @param mean_height_Lorey Lorey's mean height, e.g. [forester::Braastad_1977_initial_stand_Norway_Pine()] element 1.
#'
#' @return Diameter of Thinned trees / Diameter of remaining standing trees.
#' @export
#'
#' @examples
#' Brantseg_1969_diameter_quotient_thinned_trees_to_remaining_Norway_Pine(9)
Brantseg_1969_diameter_quotient_thinned_trees_to_remaining_Norway_Pine <- function(
  mean_height_Lorey
){
  return(
    0.4855 + 0.03555*mean_height_Lorey - 0.000845*(mean_height_Lorey^2)
  )
}
