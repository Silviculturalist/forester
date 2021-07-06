#' Basal Area per hectare for young Pine stands in northern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.1
#'
#' N = 426
#'
#' Mean basal area per hectare  = 7.190
#'
#' Standard deviation about the mean = 0.496
#'
#' Standard deviation about the function = 0.334
#'
#' R = 0.76
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
#' @param SI Site index H100 m.
#' @param dominant_height_m Dominant height of stand.
#' @param broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#'
#'ggplot()+
#'  geom_function(aes(linetype="66N 400masl 16m"), fun= function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Pine(latitude = 66,altitude = 400,
#'                                                                                                                                              stand_density = x, stems_per_ha = "function",SI = 16,dominant_height_m = 12.5,broadleaves_percent_of_basal_area = 0,pre_commercial_thinning = FALSE,even_or_somewhat_uneven_aged = TRUE)
#'  )+
#'  geom_function(aes(linetype="62N 200masl 24m"), fun= function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Pine(latitude = 62,altitude = 200,
#'                                                                                                                                              stand_density = x, stems_per_ha = "function",SI = 24,dominant_height_m = 12.5,broadleaves_percent_of_basal_area = 0,pre_commercial_thinning = FALSE,even_or_somewhat_uneven_aged = TRUE)
#'  )+
#'  xlim(c(0.4,1.0))

Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Pine <- function(
  latitude,
  altitude,
  stand_density=0.65,
  stems_per_ha="function",
  SI,
  dominant_height_m,
  broadleaves_percent_of_basal_area,
  pre_commercial_thinning,
  even_or_somewhat_uneven_aged=TRUE
){

  if(even_or_somewhat_uneven_aged==TRUE){
    uneven_aged <- FALSE
  } else if(even_or_somewhat_uneven_aged==FALSE){
    uneven_aged <- TRUE
  }

  if(stems_per_ha=="function"){
    stems_per_ha <- Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Pine(latitude = latitude,
                                                                                           altitude = altitude,
                                                                                           stand_density = stand_density,
                                                                                           dominant_height_m = dominant_height_m,
                                                                                           pre_commercial_thinning = pre_commercial_thinning,
                                                                                           even_or_somewhat_uneven_aged = even_or_somewhat_uneven_aged,
                                                                                           uneven_aged = uneven_aged)
  }

  return(
    exp(
      -1.604+
        -0.170*log((altitude+1)/10)+
        +0.00993*((altitude+1)/10)+
        +0.314*log(stand_density*10)+
        +0.467*log(stems_per_ha)+
        -0.138*log(SI*10)+
        +1.204*log(dominant_height_m*10)+
        +0.032*log(broadleaves_percent_of_basal_area+1)
    )/100
  )

}

