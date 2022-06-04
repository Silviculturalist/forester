#' Annual increment in dry weight of stem wood, tons per hectare.
#' @source From p. 93;94. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
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
#' @param basal_area_5_m_coef_of_variation Groupstructure index (Coefficient of variation for the basal area under bark on 5-metre circular plots inside the sample plot.) \% .
#'
#'
#'
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
Eriksson_1976_dry_weight_increment_tons_per_hectare <- function(basal_area_ha_before_thinning.m2,
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
    b1 <- 0.320
    b2 <- -0.131
    b3 <- 0.065
    b4 <- 0.991
    b5 <- -0.842
    b6 <- -0.295
  } else if(site_index<=21.9){
    #G20
    b1 <- 0.455
    b2 <- -0.137
    b3 <- 0.064
    b4 <- 0.900
    b5 <- -0.866
    b6 <- -0.255
  } else if(site_index<=25.9){
    #G24
    b1 <- 0.523
    b2 <- -0.138
    b3 <- 0.092
    b4 <- 0.765
    b5 <- -0.702
    b6 <- -0.194
  } else if(site_index<=29.9){
    #G28
    b1 <- 0.399
    b2 <- -0.136
    b3 <- 0.012
    b4 <- 0.821
    b5 <- -0.786
    b6 <- -0.131
  } else if(site_index>=30){
    #G32
    b1 <-  0.442
    b2 <- -0.125
    b3 <- 0.021
    b4 <- 0.770
    b5 <- -0.814
    b6 <- -0.065
  }

  return(
    0.072*((basal_area_ha_before_thinning.m2)^b1)*((thinning_percent_ba_under_bark+0.01)^b2)*((((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm)+0.1)*(((100*number_thinned_trees_per_ha)/number_trees_per_ha_before_thinning)+0.01))^0.081)*((increment_period_years/10)^b3)*(dominant_height_dm^b4)*((age_bh_100_largest_trees_per_ha_years/10)^b5)*(basal_area_5_m_coef_of_variation^b6)
  )
}
