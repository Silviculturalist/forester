#' Basal Area per hectare for young Spruce stands in northern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.3
#'
#' N = 262
#'
#' Mean basal area per hectare  = 7.411
#'
#' Standard deviation about the mean = 0.452
#'
#' Standard deviation about the function = 0.273
#'
#' R = 0.80
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
#' @param stems_per_ha number of stems per hectare. or "function".
#' @param SI Site index H100 m
#' @param dominant_height_m Dominant height of stand.
#' @param spatial_distribution 1 if even. 2 if somewhat uneven. 3 if grouped.
#' @param uneven_aged TRUE/FALSE if stand is uneven-aged.
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#' ggplot()+
#' geom_function(aes(linetype="16,500"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce(altitude = 500,stand_density = x,stems_per_ha = "function",SI = 16,broadleaves_percent_of_basal_area = 0,dominant_height_m = 12.5,spatial_distribution = 1,pre_commercial_thinning = FALSE,uneven_aged = FALSE))+
#'   geom_function(aes(linetype="16,200"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce(altitude = 200,stand_density = x,stems_per_ha = "function",SI = 16,broadleaves_percent_of_basal_area = 0,dominant_height_m = 12.5,spatial_distribution = 1,pre_commercial_thinning = FALSE,uneven_aged = FALSE))+
#'   geom_function(aes(linetype="28,200"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce(altitude = 200,stand_density = x,stems_per_ha = "function",SI = 28,broadleaves_percent_of_basal_area = 0,dominant_height_m = 12.5,spatial_distribution = 1,pre_commercial_thinning = FALSE,uneven_aged = FALSE))+
#'   xlim(c(0.4,1.0))+
#'   scale_y_continuous(limits = c(8,28),breaks=c(seq(8,28,2)))

Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce <- function(
  altitude,
  stand_density=0.65,
  stems_per_ha="function",
  SI,
  broadleaves_percent_of_basal_area=0,
  dominant_height_m,
  spatial_distribution,
  pre_commercial_thinning,
  uneven_aged=FALSE
){

  if(uneven_aged==TRUE){
    uneven_aged <- 2
    even_or_somewhat_uneven_aged <- FALSE
  } else {
    uneven_aged <- 1
    even_or_somewhat_uneven_aged <- TRUE
  }

  if(stems_per_ha=="function"){
    stems_per_ha <- Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Spruce(
      stand_density = stand_density,
      pre_commercial_thinning = pre_commercial_thinning,
      SI = SI,
      altitude = altitude,
      broadleaves_percent_of_basal_area = broadleaves_percent_of_basal_area,
      even_or_somewhat_uneven_aged = even_or_somewhat_uneven_aged,
      uneven_aged = ifelse(uneven_aged==2,TRUE,FALSE)
    )
  }

  return(
    exp(
      -1.659+
        -0.125*log(((altitude+1)/10))+
        +0.00918*(((altitude+1))/10)+
        +0.488*log((stand_density)*10)+
        +0.467*log(stems_per_ha)+
        -0.268*log(SI*10)+
        +1.219*log(dominant_height_m*10)+
        +0.153*spatial_distribution+
        -0.055*uneven_aged
    )/100
  )

}

