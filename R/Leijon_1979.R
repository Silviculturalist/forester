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
#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Function 7.6, p. 68.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0782.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 25.8.
#'
#' @param SI_Pine SI Pine H100, meters by Hägglund 1974.
#' @param latitude Degrees N.
#' @param altitude Meters above sea level.
#' @param continental TRUE/FALSE Is the plot located in a locally continental climate. [forester::local_climate_sweden()]
#' @param dry TRUE/FALSE subsoil water depth >2 meters. SWE:"Torr" mark.
#' @param herbs TRUE/FALSE field strata of low- or rich-herbs. SWE: "Örttyper".
#'
#' @return SI Spruce H100, meters by Hägglund 1972,1973.
#'
#' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_3]
#'
#' @export
Leijon_1979_SI_translation_Pine_to_Spruce_3 <- function(
  SI_Pine,
  latitude,
  altitude,
  continental,
  dry,
  herbs
){

  if(SI_Pine<12|SI_Pine>30){
    warning("SI H100 Pine Outside of material.")
  }

  return(
    exp(
      +0.005099*log(SI_Pine*10)+
        -1.5904*log(latitude)+
        -0.01122*(altitude/100)+
        -0.08112*continental+
        -0.08125*dry+
        +0.01447*herbs+
        +10.8370
    )/10
  )
}
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
#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Funktion 7.5, p. 67.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0409.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 15.9.
#'
#' @param SI_Spruce SI Spruce H100, meters by Hägglund 1972,1973.
#' @param latitude Degrees N.
#' @param altitude Meters above sea level.
#' @param continental TRUE/FALSE Is the plot located in a locally continental climate. [forester::local_climate_sweden()]
#' @param dry TRUE/FALSE subsoil water depth >2 meters. SWE:"Torr" mark.
#' @param herbs TRUE/FALSE field strata of low- or rich-herbs. SWE: "Örttyper".
#'
#' @return SI Pine H100, meters by Hägglund 1974.
#'
#' #' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_3]
#'
#' @export
Leijon_1979_SI_translation_Spruce_to_Pine_3 <- function(
  SI_Spruce,
  latitude,
  altitude,
  continental,
  dry,
  herbs
){

  if(SI_Spruce<12|SI_Spruce>35){
    warning("SI H100 Spruce Outside of material.")
  }

  return(
    exp(
      +1.0612*log(SI_Spruce*10)+
      -0.002846*(SI_Spruce*10)+
      -0.002673*((((latitude-60)+(abs(latitude-60)))/2)^2)+
      -0.009990*((((altitude-200)+(abs(altitude-200)))/200)^2)+
      +0.04383*continental+
      +0.03433*dry+
      +0.05197*herbs+
      +0.3637
    )/10
  )
}
