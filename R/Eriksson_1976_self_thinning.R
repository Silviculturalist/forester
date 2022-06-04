#' Eriksson 1976 self thinning
#' @source From p. 86-88 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @description Author recommends that the function not be used when stems per ha exceed 4500 and dominant height at the same time is more than 15 m. Author still sees this as the best available information when it was written.
#' @param number_trees_per_ha_period_start Number of trees per hectare at the start of the period.
#' @param dominant_height_m Dominant height, m.
#' @param SI100 Site Index H100 Spruce from Hägglund 1972/1973, e.g. [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()], [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#'
#' @return Annual self thinning in basal area over bark per hectare, m2.
#' @export
Eriksson_1976_self_thinning <- function(number_trees_per_ha_period_start, dominant_height_m, SI){

  dominant_height_dm <- dominant_height_m*10

  if(number_trees_per_ha_period_start>4500 && dominant_height_dm>150){
    warning("Correction needed to Ekö PM self-thinning function. Using Eriksson 1976 correction from p. 107.")
    number_trees_per_ha_period_start <- 4500 + 0.1*(number_trees_per_ha_period_start-4500)
  }


  if(SI<=17.9){
    #G16
    b1 <- 0.524
    b2 <- 3.505

  } else if(SI<=21.9){
    #G20
    b1 <- 1.858
    b2 <- 3.376

  } else if(SI<=25.9){
    #G24
    b1 <- 2.613
    b2 <- 3.356

  } else if(SI<=29.9){
    #G28
    b1 <- 2.872
    b2 <- 3.347

  } else if(SI<=33.9){
    #G32
    b1 <- 3.229
    b2 <- 3.458

  } else if(SI>=34){
    #G36
    b1 <- 2.646
    b2 <- 3.479
  }

  return(
    3.25*(10^-10)*((number_trees_per_ha_period_start / 1000)^b1)*(dominant_height_dm^b2)
  )
}
