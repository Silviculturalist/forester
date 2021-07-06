#' Stems per hectare for young stands of Scots Pine in southern Sweden.
#'
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.2.
#'
#'
#' Number of sites = 235.
#'
#' Mean stems per ha: 7.3192
#'
#' Standard deviation mean stems per ha: 0.523
#'
#'
#' standard deviation about the function: 0.371
#'
#' R = 0.72
#'
#' Cp = 7. (Daniel & Wood 1971, pp. 86)
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 42. pp. 75.
#'
#' @param stand_density 0-1.
#' @param pre_commercial_thinning Has the stand been pre-commercially thinned?
#' @param age_at_breast_height Stand age at breast height (1.3 m). For 'function', see [forester::Hagglund_age_to_height()]
#' @param latitude Latitude in degrees. Only Required if "age_at_breast_height" is "function".
#' @param SI SI H100 m. Only Required if "age_at_breast_height" is "function".
#' @param dominant_height_m Dominant height of stand. Defaults to 12.5 m. Only Required if "age_at_breast_height" is "function".
#' @param regeneration Method of stand establishment. One of "culture","natural regeneration" or "unknown". Only Required if "age_at_breast_height" is "function".
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @export
#'
#' @examples
Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Pine<- function(
  stand_density=0.65,
  pre_commercial_thinning,
  age_at_breast_height="function",
  latitude=60,
  SI,
  dominant_height_m=12.5,
  regeneration="culture"
){
  if(age_at_breast_height=="function"){
    age_at_breast_height <- Hagglund_age_to_height(latitude = latitude,
                                                   SI = SI,
                                                   dominant_height_m = dominant_height_m,
                                                   species = "Pinus sylvestris",
                                                   regeneration = regeneration)[[1]]
  }

  stand_density <- stand_density*10

  return(
    exp(
     6.148+
       +0.268*stand_density+
       -0.058*((stand_density^3)/100)+
       -0.006*age_at_breast_height+
       -0.310*pre_commercial_thinning
    )
  )


}

