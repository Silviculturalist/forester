#' Annual Increment in Basal Area in Spruce Stand in Eriksson 1976
#' @source From p. 63;64. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @details Coefficient of variation for basal area  on a 5 plot has been calculated as per p.112.
#'
#' @param basal_area_ha_before_thinning_m2 Basal area under bark per hectare before thinning, m2.
#' @param site_index SI H100 m Spruce
#' @param thinning_percent_ba_under_bark Thinning as percentage of basal area under bark before thinning.
#' @param diameter_of_mean_basal_area_of_thinned_trees_cm Diameter corresponding to mean basal area under bark of the thinned trees, cm.
#' @param diameter_of_mean_basal_area_before_thinning_cm Diameter corresponding to the mean basal area of the stand before thinning, cm.
#' @param number_thinned_trees_per_ha Number of thinned trees per hectare.
#' @param number_trees_per_ha_before_thinning Number of trees per hectare in the stand before thinning.
#' @param increment_period_years Length of increment period, years.
#' @param dominant_height_dm Dominant height, dm.
#' @param age_bh_100_largest_trees_per_ha_years Age at breast height of the 100 largest trees per hectare, years.
#'
#'
#'
#' @return Annual increment in volume under bark per hectare, m3.
#' @export
#'
#' @examples
Eriksson_1976_basal_increment_under_bark <- function(basal_area_ha_before_thinning.m2,
                                                                    site_index,
                                                                    thinning_percent_ba_under_bark,
                                                                    diameter_of_mean_basal_area_of_thinned_trees_cm,
                                                                    diameter_of_mean_basal_area_before_thinning_cm,
                                                                    number_thinned_trees_per_ha,
                                                                    number_trees_per_ha_before_thinning,
                                                                    increment_period_years,
                                                                    dominant_height_dm,
                                                                    age_bh_100_largest_trees_per_ha_years
                                                                   ){

  if(site_index<=17.9){
    #G16
    b1 <- 0.250
    b2 <- -0.046
    b3 <- 0.049
    b4 <- 0.033
    b5 <- -0.748
    b6 <- -0.308
    c <- 0.447
  } else if(site_index<=21.9){
    #G20
    b1 <- 0.213
    b2 <- -0.055
    b3 <- 0.085
    b4 <- 0.460
    b5 <- -0.875
    b6 <- -0.313
    c <- 0.488
  } else if(site_index<=25.9){
    #G24
    b1 <- 0.304
    b2 <- -0.056
    b3 <- 0.126
    b4 <- 0.035
    b5 <- -0.571
    b6 <- -0.154
    c <- 0.498
  } else if(site_index<=29.9){
    #G28
    b1 <- 0.260
    b2 <- -0.059
    b3 <- 0.034
    b4 <- 0.112
    b5 <- -0.667
    b6 <- -0.066
    c <- 0.568
  } else if(site_index>=30){
    #G32
    b1 <-  0.337
    b2 <- -0.046
    b3 <- 0.056
    b4 <- 0.120
    b5 <- -0.789
    b6 <- -0.013
    c <- 0.763
  }

  return(
    2.635*basal_area_ha_before_thinning.m2^b1*
      (thinning_percent_ba_under_bark+0.01)^b2*
      ((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm+0.1)*(100*number_thinned_trees_per_ha/number_trees_per_ha_before_thinning+0.01))^0.024*
      increment_period_years^b3*
      dominant_height_dm^b4*
      age_bh_100_largest_trees_per_ha_years^b5*
      (13.778*((age_bh_100_largest_trees_per_ha_years/10)^c)*((number_trees_per_ha_before_thinning/1000)^-0.052))^b6
  )

}

