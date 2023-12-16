#' Time to breast height for stands of Scots Pine in Norway from Braastad 1977.
#'
#' @source Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 276 function (11)
#'
#' @param SIH40 Site Index H40, e.g. [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#'
#' @details
#'
#' Not applicable for SIHH40 lower than 8.
#'
#' R = 0.377
#'
#' Residual spread = ±3.8 yrs , ±35.3\%.
#'
#'
#' @return Years until breast height 1.3 m.
#' @export
#'
#' @example
#' Braastad_1977_time_to_breast_height_Norway_Pine(SIH40=17)
Braastad_1977_time_to_breast_height_Norway_Pine<- function(
  SIH40
){
  ifelse(SIH40<8,warning("Function not applicable to SIH40 < 8.0"),NA)
  return(
    4.3641 + (83.0857/SIH40)
  )
}


