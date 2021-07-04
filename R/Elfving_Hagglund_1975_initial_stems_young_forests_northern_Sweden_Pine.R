#' Stems per hectare for young stands of Scots Pine in northern Sweden.
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.1.
#'
#'
#' Number of sites = 426.
#' Mean stems per ha: 7.2151
#' Standard deviation mean stems per ha: 0.571
#'
#'
#' standard deviation about the function: 0.394
#' R = 0.73
#' Cp = 12. (Daniel & Wood 1971, pp. 86)
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 42. pp. 75.
#'
#' @param latitude Latitude, degrees N.
#' @param altitude Altitude, meters above sea level.
#' @param stand_density 0-1.
#' @param dominant_height_m Dominant height of stand, in metres.
#' @param pre_commercial_thinning Has the stand been pre-commercially thinned?
#' @param even_or_somewhat_uneven_aged TRUE/FALSE
#' @param uneven_aged TRUE/FALSE
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @export
#'
#' @examples
Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Pine<- function(
  latitude,
  altitude,
  stand_density=0.65,
  dominant_height_m,
  pre_commercial_thinning,
  even_or_somewhat_uneven_aged,
  uneven_aged
){
  if(uneven_aged==TRUE && even_or_somewhat_uneven_aged==TRUE){
    stop("Only one of 'even_or_somewhat_uneven_aged' or 'uneven_aged' can be TRUE")

  } else if(uneven_aged==FALSE && even_or_somewhat_uneven_aged==FALSE){
    stop("Only one of 'even_or_somewhat_uneven_aged' or 'uneven_aged' can be FALSE")
  }

  dominant_height_dm <- dominant_height_m*10
  altitude <- altitude/100
  stand_density <- stand_density*10

  return(
    exp(
      8.856+
        -0.033*latitude+
        -0.062*altitude+
        +0.203*stand_density+
        -0.002*dominant_height_dm+
        -0.233*pre_commercial_thinning+
        -0.220*even_or_somewhat_uneven_aged+
        -0.074*uneven_aged
    )
  )



}
