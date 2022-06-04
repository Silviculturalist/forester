#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Funktion 7.3, p. 65.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0466.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 17.1.
#'
#' @param SI_Spruce SI Spruce H100, meters by Hägglund 1972,1973.
#' @param latitude Degrees N.
#' @param altitude Meters above sea level.
#' @param continental TRUE/FALSE Is the plot located in a locally continental climate. [forester::local_climate_sweden()]
#'
#' @return SI Pine H100, meters by Hägglund 1974.
#'
#' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_3]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_3]
#'
#' @export
Leijon_1979_SI_translation_Spruce_to_Pine_2 <- function(
  SI_Spruce,
  latitude,
  altitude,
  continental
){

  if(SI_Spruce<12 | SI_Spruce>34){
    warning("SI Spruce is outside of recommended area.")
  }

  return(
    exp(
      +1.2882*log(SI_Spruce*10)+
      -0.003806*(SI_Spruce*10)+
      -0.002334*((((latitude-60) + abs(latitude-60))/2)^2)+
      -0.01021*((((altitude-200) + abs(altitude-200))/200)^2)+
      +0.03884*continental+
      -0.6241
    )/10
  )
}
