#' Increment in Basal Area in Spruce Stand per period in Eriksson 1976
#'
#' @source From p. 64;65. in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
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
#' @return Annual increment in basal area under bark per hectare, m2.
#' @export
#'
#' @examples
Eriksson_1976_basal_growth_ha_under_bark_Spruce <- function(basal_area_ha_before_thinning.m2,
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

  G16 <- FALSE
  G20 <- FALSE
  G24 <- FALSE
  G28 <- FALSE
  G32 <- FALSE


  if(site_index<=17.9){
    G16 <- TRUE
    b1 <- 0.250
    b2 <- -0.046
    b3 <- 0.049
    b4 <- 0.334
    b5 <- -0.748
    b6 <- -0.308
  } else if(site_index<=21.9){
    G20 <- TRUE
    b1 <- 0.213
    b2 <- -0.055
    b3 <- 0.085
    b4 <- 0.460
    b5 <- -0.875
    b6 <- -0.313
  } else if(site_index<=25.9){
    G24 <- TRUE
    b1 <- 0.304
    b2 <- -0.056
    b3 <- 0.126
    b4 <- 0.035
    b5 <- -0.571
    b6 <- -0.154
  } else if(site_index<=29.9){
    G28 <- TRUE
    b1 <- 0.260
    b2 <- -0.059
    b3 <- 0.034
    b4 <- 0.112
    b5 <- -0.667
    b6 <- -0.066
  } else if(site_index>=30){
    G32 <- TRUE
    b1 <-  0.337
    b2 <- -0.046
    b3 <- 0.056
    b4 <- 0.120
    b5 <- -0.789
    b6 <- -0.013
  }

  x1 <-   log10(basal_area_ha_before_thinning.m2)
  x2 <-   log10(thinning_percent_ba_under_bark+0.01)
  x3 <-   log10(increment_period_years)
  x4 <-   log10(dominant_height_dm)
  x5 <-   log10(age_bh_100_largest_trees_per_ha_years)
  x6 <-   log(basal_area_5_m_coef_of_variation)



return(
  10^(
  0.421+ # intercept includes logarithmic bias.
  +0.250*x1+
  -0.046*x2+
  +0.024*log10(((diameter_of_mean_basal_area_of_thinned_trees_cm/diameter_of_mean_basal_area_before_thinning_cm)+0.1)*(((100*number_thinned_trees_per_ha)/number_trees_per_ha_before_thinning)+0.01))+
  +0.049*x3+
  +0.334*x4+
  -0.748*x5+
  -0.308*x6+
  -0.037*x1*G20+
  +0.054*x1*G24+
  +0.010*x1*G28+
  +0.087*x1*G32+
  -0.009*x2*G20+
  -0.010*x2*G24+
  -0.013*x2*G28+
  -0.001*x2*G32+
  +0.036*x3*G20+
  +0.077*x3*G24+
  -0.015*x3*G28+
  +0.007*x3*G32+
  +0.126*x4*G20+
  -0.299*x4*G24+
  -0.222*x4*G28+
  -0.214*x4*G32+
  -0.127*x5*G20+
  +0.177*x5*G24+
  +0.081*x5*G28+
  -0.041*x5*G32+
  -0.005*x6*G20+
  +0.154*x6*G24+
  +0.242*x6*G28+
  +0.295*x6*G32
  )
)



}
