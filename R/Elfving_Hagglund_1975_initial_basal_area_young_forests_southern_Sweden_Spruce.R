#' Basal Area per hectare for young Spruce stands in northern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.4
#'
#' N = 183
#'
#' Mean basal area per hectare  = 7.531
#'
#' Standard deviation about the mean = 0.470
#'
#' Standard deviation about the function = 0.306
#'
#' R = 0.74
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
#' @param stems_per_ha number of stems per hectare.
#' @param dominant_height_m Dominant height of stand.
#' @param broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#' @param spatial_distribution 1 if even. 2 if somewhat uneven. 3 if grouped.
#' @param pre_commercial_thinning TRUE if pre_commercial thinning has taken place.
#' @param stand_density 0-1. Required only if stems_per_ha or age_at_breast_height is "function".
#' @param SI Site index H100 m. Required only if stems_per_ha or age_at_breast_height is "function".
#' @param age_at_breast_height Age of stand at breast height. Required only if stems_per_ha is "function".
#' @param even_or_somewhat_uneven_aged TRUE if even_aged or slightly uneven-aged.
#' @param regeneration One of "culture", "natural regeneration" or "unknown.". Required only if age_to_breast_height is "function".
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#'
#' ggplot()+
#' geom_function(aes(linetype="G20"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Spruce(altitude = 300,
#'                                                                                                                                    stems_per_ha = "function" ,
#'                                                                                                                                    dominant_height_m = 12.5 ,
#'                                                                                                                                    broadleaves_percent_of_basal_area = 0,
#'                                                                                                                                    spatial_distribution = 1 ,
#'                                                                                                                                    pre_commercial_thinning = FALSE,
#'                                                                                                                                    stand_density = x,
#'                                                                                                                                    SI = 20,
#'                                                                                                                                    age_at_breast_height = "function" ,
#'                                                                                                                                    even_or_somewhat_uneven_aged = TRUE ,
#'                                                                                                                                    regeneration = "culture"))+
#'   geom_function(aes(linetype="G36"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Spruce(altitude = 300,
#'                                                                                                                                      stems_per_ha = "function" ,
#'                                                                                                                                      dominant_height_m = 12.5 ,
#'                                                                                                                                      broadleaves_percent_of_basal_area = 0,
#'                                                                                                                                      spatial_distribution = 1 ,
#'                                                                                                                                      pre_commercial_thinning = FALSE,
#'                                                                                                                                      stand_density = x,
#'                                                                                                                                      SI = 36,
#'                                                                                                                                      age_at_breast_height = "function" ,
#'                                                                                                                                      even_or_somewhat_uneven_aged = TRUE ,
#'                                                                                                                                      regeneration = "culture"))+
#'   xlim(c(0.4,1))+
#'   scale_y_continuous(limits=c(8,28), breaks = seq(8,28,2))
Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Spruce <- function(
  altitude,
  stems_per_ha="function",
  dominant_height_m,
  broadleaves_percent_of_basal_area,
  spatial_distribution,
  pre_commercial_thinning,
  stand_density,
  SI,
  age_at_breast_height="function",
  even_or_somewhat_uneven_aged,
  regeneration="culture"
){

  if(age_at_breast_height=="function"){
    age_at_breast_height <- Hagglund_age_to_height(latitude = latitude,
                                                   SI = SI,
                                                   dominant_height_m = dominant_height_m,
                                                   species = "Picea abies",
                                                   locality = "southern" ,
                                                   regeneration = regeneration)[[1]]
  }

  if(stems_per_ha=="function"){
    stems_per_ha <-
      Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Spruce(stand_density = stand_density,
                                                                             pre_commercial_thinning = pre_commercial_thinning ,
                                                                             SI = SI ,
                                                                             age_at_breast_height = age_at_breast_height,
                                                                             altitude = altitude,
                                                                             broadleaves_percent_of_basal_area = broadleaves_percent_of_basal_area ,
                                                                             even_or_somewhat_uneven_aged = even_or_somewhat_uneven_aged )
  }



  return(
    exp(
      -0.102+
        -0.059*log((altitude+1)/10)+
        +0.584*log(stems_per_ha)+
        +0.723*log(dominant_height_m*10)+
        -0.025*log(broadleaves_percent_of_basal_area+1)+
        -0.098*spatial_distribution
    )/100
  )

}



