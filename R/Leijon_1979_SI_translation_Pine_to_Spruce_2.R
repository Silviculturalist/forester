#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Funktion 7.4, p. 66.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0812.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 27.3.
#'
#' @param SI_Pine SI Pine H100, meters by Hägglund 1974.
#' @param latitude Degrees N.
#' @param altitude Meters above sea level.
#' @param continental TRUE/FALSE Is the plot located in a locally continental climate. [forester::local_climate_sweden()]
#'
#' @return SI Spruce H100, meters by Hägglund 1972,1973.
#'
#' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_3]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_3]
#' @export
Leijon_1979_SI_translation_Pine_to_Spruce_2 <- function(
  SI_Pine,
  latitude,
  altitude,
  continental
){

  if(SI_Pine<12|SI_Pine>30){
    warning("SI H100 Pine Outside of material.")
  }

  return(
    exp(
      +0.005217*(SI_Pine*10)+
      -1.3914*log(latitude)+
      -0.01114*(altitude/100)+
      -0.1024*continental+
      +9.9936
    )/10
  )
}
