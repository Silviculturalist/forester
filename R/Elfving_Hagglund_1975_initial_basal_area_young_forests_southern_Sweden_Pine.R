#' Basal Area per hectare for young Pine stands in southern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.2
#'
#' N = 235
#'
#' Mean basal area per hectare  = 7.465
#'
#' Standard deviation about the mean = 0.440
#'
#' Standard deviation about the function = 0.280
#'
#' R = 0.71
#'
#'
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 53. pp. 75.
#'
#' @param altitude meters above sea level.
#' @param stand_density 0-1
#' @param stems_per_ha number of stems per hectare.
#' @param SI Site index H100
#' @param dominant_height Dominant height of stand.
#' @param uneven_aged TRUE/FALSE if stand is uneven-aged.
#' @param age_at_breast_height Age at breast height, years.
#' @param regeneration How was the stand established? One of "culture", "natural regeneration" or "unknown". Only required if age_at_breast_height=="function"
#' @param pre_commercial_thinning TRUE/FALSE, has pre-commercial thinning been done?
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#'
#' ggplot()+
#' geom_function(aes(linetype="h100=16"), fun= function (x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Pine(altitude = 300,
#'                                                                                                                                      stand_density = x,
#'                                                                                                                                      stems_per_ha = "function" ,
#'                                                                                                                                      SI = 16 ,
#'                                                                                                                                      dominant_height_m = 12.5 ,
#'                                                                                                                                      uneven_aged = FALSE,
#'                                                                                                                                      age_at_breast_height = "function" ,
#'                                                                                                                                      regeneration = "culture",
#'                                                                                                                                      pre_commercial_thinning = FALSE))+
#'   geom_function(aes(linetype="h100=24"), fun= function (x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Pine(altitude = 300,
#'                                                                                                                                        stand_density = x,
#'                                                                                                                                        stems_per_ha = "function" ,
#'                                                                                                                                        SI = 24 ,
#'                                                                                                                                        dominant_height_m = 12.5 ,
#'                                                                                                                                        uneven_aged = FALSE,
#'                                                                                                                                        age_at_breast_height = "function" ,
#'                                                                                                                                        regeneration = "culture",
#'                                                                                                                                        pre_commercial_thinning = FALSE))+
#'   xlim(c(0.4,1))+
#'   scale_y_continuous(limits=c(8,24),breaks=seq(8,24,2))
#'
Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Pine <- function(
  altitude,
  stand_density=0.65,
  stems_per_ha="function",
  SI,
  dominant_height_m,
  uneven_aged=FALSE,
  age_at_breast_height="function",
  regeneration,
  pre_commercial_thinning
){
  if(age_at_breast_height=="function"){
    age_at_breast_height <- Hagglund_age_to_height(latitude = latitude,SI = SI,dominant_height_m = dominant_height_m,species = "Pinus sylvestris",regeneration = regeneration)[[2]]
  }

  if(stems_per_ha=="function"){
    stems_per_ha <- Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Pine(stand_density = stand_density ,
                                                                                           pre_commercial_thinning = pre_commercial_thinning ,
                                                                                           age_at_breast_height = age_at_breast_height)
  }

  return(
    exp(
      +1.280+
        -0.089*log((altitude+1)/10)+
        +0.283*log(stand_density*10)+
        +0.370*log(stems_per_ha)+
        -0.174*log(SI*10)+
        +0.878*log(dominant_height_m*10)+
        -0.121*uneven_aged
    )/100
  )

}



