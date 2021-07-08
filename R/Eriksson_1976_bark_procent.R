#' Bark procent of basal area at breast height
#'
#' @source From p. 82-83 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @param age_bh_100_largest_trees_per_ha_years Age of 100 largest trees at breast height per hectare in years.
#' @param diameter_of_mean_basal_area_under_bark_cm Diameter corresponding to the mean basal area under bark, cm.
#' @param basal_area_under_bark_ha_m2 Basal area under bark per hectare, m2.
#' @param SI SI H100 Spruce, m.
#'
#' @return %
#' @export
#'
#' @examples
Eriksson_1976_bark_procent <- function(age_bh_100_largest_trees_per_ha_years, SI, diameter_of_mean_basal_area_under_bark_cm, basal_area_under_bark_ha_m2){

  if(SI<=17.9){
    #G16
    b1 <- -0.224

  } else if(SI<=21.9){
    #G20
    b1 <- -0.247

  } else if(SI<=25.9){
    #G24
    b1 <- -0.249

  } else if(SI<=29.9){
    #G28
    b1 <- -0.266

  } else if(SI<=33.9){
    #G32
    b1 <- -0.255

  } else if(SI>=34){
    #G36
    b1 <- -0.280
  }

  return(
    45.08*(diameter_of_mean_basal_area_under_bark_cm^b1)*(basal_area_under_bark_ha_m2^-0.281)*(age_bh_100_largest_trees_per_ha_years^0.125)
  )
}
