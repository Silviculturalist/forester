#' Annual Volume Increment under bark per hectare, m3
#' @source From p. 90;91. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @param basal_area_ha_before_thinning_m2 Basal area under bark per hectare before thinning, m2.
#' @param site_index Spruce
#' @param thinning_percent_ba_under_bark Thinning as percentage of basal area under bark before thinning.
#' @param diameter_of_mean_basal_area_of_thinned_trees_cm Diameter corresponding to mean basal area under bark of the thinned trees, cm.
#' @param diameter_of_mean_basal_area_before_thinning_cm Diameter corresponding to the mean basal area of the stand before thinning, cm.
#' @param number_thinned_trees_per_ha Number of thinned trees per hectare.
#' @param number_trees_per_ha_before_thinning Number of trees per hectare in the stand before thinning.
#' @param increment_period_years Length of increment period, years.
#' @param dominant_height_dm Dominant height, dm.
#' @param age_bh_100_largest_trees_per_ha_years Age at breast height of the 100 largest trees per hectare, years.
#' @param basal_area_5_m_coef_of_variation Groupstructure index (Coefficient of variation for the basal area under bark on 5-metre circular plots inside the sample plot.) % .
#'
#'
#'
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
#'
#' @examples
Eriksson_1976_annual_volume_increment_under_bark_per_ha <- function(basal_area_ha_before_thinning.m2,
                                                                             site_index,
                                                                             thinning_percent_ba_under_bark,
                                                                             diameter_of_mean_basal_area_of_thinned_trees_cm,
                                                                             diameter_of_mean_basal_area_before_thinning_cm,
                                                                             number_thinned_trees_per_ha,
                                                                             number_trees_per_ha_before_thinning,
                                                                             increment_period_years,
                                                                             dominant_height_dm,
                                                                             age_bh_100_largest_trees_per_ha_years,
                                                                             basal_area_5_m_coef_of_variation){
  if(site_index<=17.9){
    #G16
    b1 <- 0.253
    b2 <- -0.100
    b3 <- 0.067
    b4 <- 1.103
    b5 <- -0.906
    b6 <- -0.296
  } else if(site_index<=21.9){
    #G20
    b1 <- 0.370
    b2 <- -0.107
    b3 <- 0.080
    b4 <- 1.015
    b5 <- -0.939
    b6 <- -0.233
  } else if(site_index<=25.9){
    #G24
    b1 <- 0.435
    b2 <- -0.109
    b3 <- 0.109
    b4 <- 0.831
    b5 <- -0.730
    b6 <- -0.125
  } else if(site_index<=29.9){
    #G28
    b1 <- 0.322
    b2 <- -0.108
    b3 <- 0.042
    b4 <- 0.907
    b5 <- -0.795
    b6 <- -0.109
  } else if(site_index>=30){
    #G32
    b1 <-  0.358
    b2 <- -0.097
    b3 <- 0.035
    b4 <- 0.868
    b5 <- -0.861
    b6 <- -0.042
  }

  return(
    0.141*((basal_area_ha_before_thinning.m2)^b1)*((thinning_percent_ba_under_bark-0.01)^b2)*((((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm)+0.1)*(((100*number_thinned_trees_per_ha)/number_trees_per_ha_before_thinning)+0.01))^0.062)*((increment_period_years/10)^b3)*(dominant_height_dm^b4)*((age_bh_100_largest_trees_per_ha_years/10)^b5)*(basal_area_5_m_coef_of_variation^b6)
  )
}
