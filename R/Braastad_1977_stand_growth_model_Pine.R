#' Braastad 1977 growth model for Pine stands in Norway.
#'
#' @param SIH40 Site Index H40, e.g. [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#' @param Age Stand age at breast height.
#' @param predict_ages A vector of ages to predict, starting with the Age.
#' @param stems_per_ha Number of stems per hectare.
#' @param Basal_area_m2_ha Optional. Default = NULL. Initial Basal area m2/ha.
#' @param thinning_fix_stems Optional. Should thinning always remove a fixed number of stems? Default is NULL.
#' @param thinning_lower_relative_spacing Set the lower density limit after thinning to a Hart - Becking Index., e.g. [forester::Hart_Becking_relative_spacing_index()]
#' @param thinning_upper_relative_spacing Set the upper density limit after thinning to a Hart - Becking Index., e.g. [forester::Hart_Becking_relative_spacing_index()]
#' @param production_level Default = 1. Set to lower to calculate production for stand less or more productive than average. e.g. 0.90 for 10\% lower production than average.
#' @param height_reduction Default=1. Increase the difference between dominant height and Lorey's mean height, e.g. for stands with extreme snow damage set the value 0.95 to reduce Lorey's mean height by 5\%
#' @param reduce_thinned_diameter_if_number_removed_stems_lower_than Default = 0. If the number of removed stems is less than this value, the stem diameters will be reduced by 10\%
#' @param adjust_diameter_ratio_thinned Default =1. Adjust ratio between the diameter of removed stems and the initial stand, calculated with [forester::Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine()]
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
  #Initiate
  i <- 3 #Last thinning, second last thinning, third last thinning
  BA_thinned <- c(0,0) #Must use index to find last, second last, third last thinning.
  SIST <- 1 # Loop counter. We start with 1 instead of 0.

  #Check start values
  if((SIH40-18.5)<0 & (SIH40-12.5)<0 & (Age-160)>0){
    warning("You are outside the limits of the programme!")
    SIST<- SIST+1
  }


  EGH <- MGH/PER





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

  #Reduce diameter of felled trees if stems remaining is less than ISE by 10 percent.
  ISE <- ifelse(stems_remaining>=ISE,1,0.9)
  QMD_felled <- QMD_felled*ISE


  #Calculate thinned basal area
  BA_thinned[i] <- stand_basal_area_QMD(stems_per_ha = stems_thinned,QMD = QMD_felled)

  #increment thinning counter.
  i <- i+1

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

  #Calculate ratio thinned / initial
  ratio_thinned_to_initial<- Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine(dominant_height = dominant_height)



  #Calculate BA increment
  BA_increment <-
    Braastad_1977_BA_increment_percent_Norway_Pine(
      SIH40 = SIH40,
      total_stand_age = stand_total_age,
      BA_m2_ha_after_thinning = BA_remaining,
      stems_after_thinning = stems_after_thinning,
      diameter_mean_basal_area_stem_after_thinning = QMD_remaining,
      diameter_ratio_thinned_to_initial = ratio_thinned_to_initial,
      thinned_BA_m2_ha_last_thinning = BA_thinned[],
      thinned_BA_m2_ha_second_last_thinning = ,
      thinned_BA_m2_ha_third_last_thinning =
    )

  #Scale BA increment for production levels..
  BA_increment <- BA_increment*production_level


  #Next stems
  stems_per_ha <- stems_remaining

  #Count years to increment between Age and next predict_ages.
  years_to_increment <- predict_ages[SIST+1]-Age


  #Next BA
  Basal_area_m2_ha <- BA_remaining+(BA_remaining*BA_increment/100)*years_to_increment

  #Next Diameter
  QMD_before_thinning <- quadratic_mean_diameter(Basal_area_m2_ha = Basal_area_m2_ha,
                                                 stems_per_ha = stems_per_ha)

  #Calculate Diameter increment mm per annum.
  Diameter_increment_mm <- ((QMD_before_thinning - QMD_remaining)/years_to_increment)*10

  #Calculate Basal Area increment M2 per annum
  BA_increment_m2 <- (Basal_area_m2_ha - BA_remaining)/years_to_increment

  ONT <- years_to_increment*(BA_remaining+Basal_area_m2_ha)/2
  MGH <- MGH+ONT
  PER <- PER + years_to_increment

  #Calculate Volume increment m3 per annum. (MAI)
  MAI <- (Volume_before_thinning- Volume_remaining) / years_to_increment















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
