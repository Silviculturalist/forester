#' Stems per hectare for young stands of Scots Pine in southern Sweden.
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.2.
#'
#'
#' Number of sites = 235.
#' Mean stems per ha: 7.3192
#' Standard deviation mean stems per ha: 0.523
#'
#'
#' standard deviation about the function: 0.371
#' R = 0.72
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
#' @param age_at_breast_height Stand age at breast height (1.3 m)
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @export
#'
#' @examples
Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Pine<- function(
  stand_density=0.65,
  pre_commercial_thinning,
  age_at_breast_height
){

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
