#' Bark subtraction procent
#'
#' @source From p. 82-83 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#' @param diameter_of_mean_basal_area_over_bark_cm Diameter corresponding to the mean basal area over bark, cm.
#' @param basal_area_over_bark_ha_m2 Basal area over bark per hectare, m2.
#' @param age_bh_100_largest_trees_per_ha_years Age of 100 largest trees at breast height per hectare in years.
#'
#' @return %
#' @export
#'
#' @examples
Eriksson_1976_bark_subtraction_procent <- function(diameter_of_mean_basal_area_over_bark_cm, basal_area_over_bark_ha_m2,age_bh_100_largest_trees_per_ha_years){

  if(site_index<=17.9){
    #G16
    b1 <- -0.138

  } else if(site_index<=21.9){
    #G20
    b1 <- -0.183

  } else if(site_index<=25.9){
    #G24
    b1 <- -0.206

  } else if(site_index<=29.9){
    #G28
    b1 <- -0.236

  } else if(site_index<=33.9){
    #G32
    b1 <- -0.243

  } else if(site_index>=34){
    #G36
    b1 <- -0.243
  }

  return(
    9.50*(diameter_of_mean_basal_area_over_bark_cm^b1)*(basal_area_over_bark_ha_m2^0.135)*(age_bh_100_largest_trees_per_ha_years^0.112)
  )
}
