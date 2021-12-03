#' Braastad 1977 growth model for Pine stands in Norway.
#'
#' @param SIH40 Site Index H40, e.g. [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#' @param Age Stand age at breast height.
#' @param stems_per_ha Number of stems per hectare.
#' @param Basal_area_m2_ha Optional. Default = NULL. Initial Basal area m2/ha.
#' @param thinning_fix_stems Optional. Should thinning always remove a fixed number of stems? Default is NULL.
#' @param thinning_lower_relative_spacing Set the lower density limit after thinning to a Hart - Becking Index., e.g. [forester::Hart_Becking_relative_spacing_index()]
#' @param thinning_upper_relative_spacing Set the upper density limit after thinning to a Hart - Becking Index., e.g. [forester::Hart_Becking_relative_spacing_index()]
#' @param production_level Default = 1. Set to lower to calculate production for stand less or more productive than average. e.g. 0.90 for 10\% lower production than average.
#' @param height_reduction Default=1. Increase the difference between dominant height and Lorey's mean height, e.g. for stands with extreme snow damage set the value 0.95 to reduce Lorey's mean height by 5\%
#' @param reduce_thinned_diameter_if_number_removed_stems_lower_than Default = 0. If the number of removed stems is less than this value, the stem diameters will be reduced by 10\%
#' @param adjust_diameter_ratio_thinned Adjust ratio between the diameter of removed stems and the initial stand, calculated with [forester::Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine()]
#' @param BA_m2_ha_MAX Optional. Default = NULL. Provide an upper limit to Basal Area after thinning given in m2/ha.
#'
#' @return
#' @export
#'
#' @examples
Braastad_1977_stand_growth_model_Pine <- function(
  SIH40,
  Age,
  stems_per_ha,
  Basal_area_m2_ha=NULL,
  thinning_fix_stems=NULL,
  thinning_lower_relative_spacing=NULL,
  thinning_upper_relative_spacing=NULL,
  production_level=1,
  height_reduction=1,
  reduce_thinned_diameter_if_number_removed_stems_lower_than=0,
  adjust_diameter_ratio_thinned=1,
  BA_m2_ha_MAX=NULL
){

  #Time to breast height
  T13 <- Braastad_1977_time_to_breast_height_Norway_Pine(SIH40 = SIH40)
  stand_total_age <- T13+Age

  #Dominant height
  dominant_height<- Tveite_1976_height_trajectory_Norway_Pine(dominant_height = SIH40,age=40,age2=Age)

  #Returns a list of two elements. i) Lorey's mean height, ii) Basal area m2 per hectare.
  HL_before_thinning <- Braastad_1977_initial_stand_Norway_Pine(dominant_height = dominant_height,
                                                stand_total_age = stand_total_age,
                                                stems_per_ha = stems_per_ha,
                                                SIH40 = SIH40)[[1]]

  #Throws warning if basal area not given and outside of function limits for calculating initial basal area.
  ifelse(is.null(Basal_area_m2_ha) & stems_per_ha<1000 | is.null(Basal_area_m2_ha) & stems_per_ha>6000 | is.null(Basal_area_m2_ha) & HL_before_thinning<6 |is.null(Basal_area_m2_ha) & HL_before_thinning>12,
         warning("Outside limits for Basal area of initial stand. Calculable when 1000<=stems_per_ha<=6000 & 6<=HL_before_thinning<=12"),
         NA)

  #Initial values for basal area calculated if not given.
  Basal_area_m2_ha <- ifelse(is.null(Basal_area_m2_ha),
                             Braastad_1977_initial_stand_Norway_Pine(dominant_height = dominant_height,
                                                              stand_total_age = stand_total_age,
                                                              stems_per_ha = stems_per_ha,
                                                              SIH40 = SIH40)[[2]],
                             Basal_area_m2_ha)

  #Quadratic Mean Diameter before thinning
  QMD_before_thinning <- quadratic_mean_diameter(Basal_area_m2_ha=Basal_area_m2_ha,
                                stems_per_ha=stems_per_ha)

  #Volume before thinning
  Volume_before_thinning <- Braastad_1977_QMD_tree_volume_Norway_Pine(diameter_cm = QMD_before_thinning,height_m = HL_before_thinning )*stems_per_ha/1000

  #Set initial params if removed number of trees is fixed.

  relative_spacing_index<- Hart_Becking_relative_spacing_index(stems_per_ha = stems_per_ha,dominant_height = dominant_height)

  ifelse(is.null(thinning_fix_stems) & !is.null(thinning_lower_relative_spacing) & !is.null(thinning_upper_relative_spacing) & (relative_spacing_index-thinning_lower_relative_spacing)>0,
         assign("stems_thinned",stems_per_ha-((10^8)/((thinning_upper_relative_spacing^2)*(dominant_height^2))))
         ,assign("stems_thinned",0))

  #Calculate stems remaining
  stems_remaining <- stems_per_ha-stems_thinned


  #Get diameter of felled trees
  QMD_felled <- Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine(dominant_height = dominant_height)*adjust_diameter_ratio_thinned*QMD_before_thinning

  #Calculate thinned basal area
  BA_thinned <- stand_basal_area_QMD(stems_per_ha = stems_thinned,QMD = QMD_felled)

  #Calculate remaining basal area
  BA_remaining <- Basal_area_m2_ha-BA_thinned

  #Calculate QMD of remaining stems.
  QMD_remaining <- quadratic_mean_diameter(Basal_area_m2_ha = BA_remaingin, stems_per_ha = stems_remaining)

  #Estimate HLory after thinning
  HL_remaining <- Tveite_1967_Loreys_mean_height_Norway_Pine(dominant_height = dominant_height,stems_per_ha = stems_remaining,basal_area_m2_ha = BA_remaining,diameter_mean_basal_area_stem = QMD_remaining)

  #Estimate Volume remaining
  Volume_remaining<- Braastad_1977_QMD_tree_volume_Norway_Pine(diameter_cm = QMD_remaining,height_m = HL_remaining)*stems_remaining/1000

  #Estimate Volume removed
  Volume_removed <- Volume_before_thinning-Volume_remaining









  Tveite_1967_Loreys_mean_height_Norway_Pine(dominant_height = SIH40,stems_per_ha = stems_per_ha,basal_area_m2_ha = Basal_area_m2_ha,diameter_mean_basal_area_stem = )

  Braastad_1977_BA_increment_percent_Norway_Pine()
  Braastad_1977_initial_stand_Norway_Pine()
  Braastad_1977_volume_Norway_Pine()
  Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine()
  Brantseg_1969_diameter_quotient_thinned_trees_to_remaining_Norway_Pine()
  Tveite_1967_Loreys_mean_height_Norway_Pine()
  Tveite_1976_height_trajectory_Norway_Pine()




  #Estimate initial values for basal area

}
