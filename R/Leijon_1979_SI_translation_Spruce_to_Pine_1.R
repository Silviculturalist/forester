#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Funktion 7.1, p. 63.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0549
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 18.1.
#'
#' @param SI_Spruce SI Spruce H100, meters by Hägglund 1972,1973.
#'
#' @return SI Pine H100, meters by Hägglund 1974.
#'
#' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_3]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_3]
#' @export
#'
#' @examples
Leijon_1979_SI_translation_Spruce_to_Pine_1 <- function(
  SI_Spruce
){

  if(SI_Spruce<8|SI_Spruce>33){
    warning("SI Spruce may be outside underlying material. Maxmimum SI Pine at 33 m SI Spruce.")
  }


  return(
    exp(
      +1.6967*log(SI_Spruce*10)+
      -0.005179*(SI_Spruce*10)+
      -2.5397 #approximately corrected for logarithmic bias.
    )/10
  )
}
