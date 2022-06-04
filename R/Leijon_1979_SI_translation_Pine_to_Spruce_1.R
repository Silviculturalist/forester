#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Funktion 7.2, p. 63.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.1070.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 38.4.
#'
#' @param SI_Pine SI Pine H100, meters by Hägglund 1974.
#'
#' @return SI Spruce H100, meters by Hägglund 1972,1973.
#'
#' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_3]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_3]
#' @export
Leijon_1979_SI_translation_Pine_to_Spruce_1 <- function(
  SI_Pine
){

  if(SI_Pine<8|SI_Pine>30){
    warning("SI Pine may be outside underlying material. Minimum SI Spruce at 8.2 m SI Pine.")
  }

  return(
    exp(
      -0.9596*log(SI_Pine*10)+
        +0.01171*(SI_Pine*10)+
        +7.9209 #approximately corrected for logarithmic bias.
    )/10
  )
}
