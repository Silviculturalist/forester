#' Percent Basal Area Increment in stands of Scots Pine by Tveite 1976 for Braastad 1977
#'
#' @param SIH40 Site Index H40 e.g. Site Index H40 , e.g. Tveite 1976 ([forester::Tveite_1976_height_trajectory_Pine_Norway()])
#' @param total_stand_age Total stand age, years.
#' @param BA_m2_ha_after_thinning Basal Area, m2/ha after thinning
#' @param stems_after_thinning Stems per hectare, after thinning.
#' @param diameter_mean_basal_area_stem_after_thinning Diameter corresponding to the mean basal area tree after thinning.
#' @param diameter_ratio_thinned_to_initial Ratio of the diameter of the thinned stems to that of the initial stand, e.g. [forester::Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine()]
#' @param thinned_BA_m2_ha_last_thinning Basal Area m2/ha removed during the last thinning
#' @param thinned_BA_m2_ha_second_last_thinning Basal Area m2/ha removed during the last thinning
#' @param thinned_BA_m2_ha_third_last_thinning Basal Area m2/ha removed during the last thinning
#'
#' @return Basal Area Increment, Percent.
#' @export
Braastad_1977_BA_increment_percent_Norway_Pine <- function(
    SIH40,
    total_stand_age,
    BA_m2_ha_after_thinning,
    stems_after_thinning,
    diameter_mean_basal_area_stem_after_thinning,
    diameter_ratio_thinned_to_initial,
    thinned_BA_m2_ha_last_thinning,
    thinned_BA_m2_ha_second_last_thinning,
    thinned_BA_m2_ha_third_last_thinning
){

  HL70 <- 4.4704 + 0.2235*SIH40 + 0.04177*(SIH40^2)

  Hdiff <- -0.07440 + 0.01776*Loreys_mean_height_after_thinning + 0.02030*HL70 + 0.003197*BA_m2_ha_after_thinning - 0.000250*Loreys_mean_height_after_thinning*sqrt(stems_after_thinning)-0.001674*(Loreys_mean_height_after_thinning^2)+0.000260*(Loreys_mean_height_after_thinning^3)

  #1.050 is the climate value Kpg for low-land forests from Brantseg 1969 p. 64, row 4.
  percent_growth <- 0.94303 - 0.042535*Loreys_mean_height_after_thinning*sqrt(stems_after_thinning/diameter_mean_basal_area_stem_after_thinning) + 46.5657*(1/total_stand_age) - 0.117082*diameter_ratio_thinned_to_initial + 25.76632*(1/diameter_mean_basal_area_stem_after_thinning) + 0.36322*HL70*(sqrt((thinned_BA_m2_ha_last_thinning/2)+thinned_BA_m2_ha_second_last_thinning+(thinned_BA_m2_ha_third_last_thinning/2))/BA_m2_ha_after_thinning) - 0.976235*diameter_mean_basal_area_stem_after_thinning/Loreys_mean_height_after_thinning + 0.507327*1.050 + 60.37340*Hdiff/Loreys_mean_height_after_thinning

  return(
    percent_growth
  )
}

#' Diameter Quotient of Thinned trees to remaining Standing trees for Scots Pine in Norway from Brastaad 1977.
#'
#' @source Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 274 function (7)
#'
#' @param dominant_height Dominant height of stand, m.
#'
#' @return Ratio Diameter of thinned trees / Diameter of unthinned stand.
#' @export
Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine <- function(
    dominant_height
){
  return(
    0.4855 + 0.0355*dominant_height - 0.000845*(dominant_height^2)
  )
}

#' Initial values for young Pine stands in Norway from Braastad 1977.
#'
#' @source Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 274 function (4) function (5)
#'
#' @param dominant_height Dominant height of stand, m.
#' @param stand_total_age Total age of stand.
#' @param stems_per_ha Number of stems per hectare.
#' @param SIH40 Site Index H40 , e.g. Tveite 1976 ([forester::Tveite_1976_height_trajectory_Norway_Pine()])
#'
#' @return A list containing two elements: i) Lorey's Mean height. ii) Basal Area m2 per hectare.
#' @export
Braastad_1977_initial_stand_Norway_Pine <- function(
    dominant_height,
    stand_total_age,
    stems_per_ha,
    SIH40
){
  if(stems_per_ha<1000 | stems_per_ha>6000){
    warning("Outside eligible area! 1000<=stems_per_ha<=6000")
  }
  if(dominant_height<6 | dominant_height>12){
    warning("Outside eligible area! 6<=dominant_height<=12")
  }

  HL <- dominant_height - (219.655 - 16.64*dominant_height + 0.393*(dominant_height^2) + 1.34*stand_total_age - 0.0506*stand_total_age*dominant_height - 0.0513*stems_per_ha + 0.00637*stems_per_ha*dominant_height)/100

  Basal_area_1 <- -1.4443 + 1.2430*HL + 0.0829*SIH40 + 0.0003506*HL*stems_per_ha

  return(
    list(
      "Lorey's mean height"=HL,
      "Basal area m2 / ha"=Basal_area_1
    )
  )
}

#' Volume for Pine in Norway from Braastad 1977.
#'
#' @description Braastad uses two functions from Brantseg (1967). Lorey's height is weighted by 0.983 by basis of comparison with all experimental plots.
#'
#' @param diameter_cm Diameter corresponding the basal area mean tree.
#' @param height_m Lorey's mean height, m for the stand.
#'
#' @return Volume, m3sk.
#' @export
#'
#' @examples
#' Braastad_1977_QMD_tree_volume_Norway_Pine(23,19)
Braastad_1977_QMD_tree_volume_Norway_Pine <- function(
    diameter_cm,
    height_m
){

  return(
    ifelse(diameter_cm>10 & diameter_cm<14, #If between 10 and 14 cm, the mean of the smaller and larger function.
           ((2.0044 + 0.029886*(diameter_cm^2) + 0.036972*(diameter_cm^2)*(height_m*0.983))+(-9.98 + 0.20480*(diameter_cm^2) + 0.029966*(diameter_cm^2)*(height_m*0.983) + 0.003539*(diameter_cm^2)*(4.12 - 0.002817*(diameter_cm^2) + 0.02623*((height_m*0.983)^2) - 0.3184*((diameter_cm/(height_m*0.983))^2)) - 0.002918*(diameter_cm^2)*(2.96 + 1.15*diameter_cm - 0.73*(diameter_cm/(height_m*0.983)))  )/2),
           ifelse(diameter_cm<12,
                  2.0044 + 0.029886*(diameter_cm^2) + 0.036972*(diameter_cm^2)*(height_m*0.983), #smaller function
                  -9.98 + 0.20480*(diameter_cm^2) + 0.029966*(diameter_cm^2)*(height_m*0.983) + 0.003539*(diameter_cm^2)*(4.12 - 0.002817*(diameter_cm^2) + 0.02623*((height_m*0.983)^2) - 0.3184*((diameter_cm/(height_m*0.983))^2)) - 0.002918*(diameter_cm^2)*(2.96 + 1.15*diameter_cm - 0.73*(diameter_cm/(height_m*0.983))) #larger function
           )
    )
  )

}


#' Time to breast height for stands of Scots Pine in Norway from Braastad 1977.
#'
#' @source Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 276 function (11)
#'
#' @param SIH40 Site Index H40, e.g. [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#'
#' @details
#'
#' Not applicable for SIHH40 lower than 8.
#'
#' R = 0.377
#'
#' Residual spread = ±3.8 yrs , ±35.3\%.
#'
#'
#' @return Years until breast height 1.3 m.
#' @export
#'
#' @example Braastad_1977_time_to_breast_height_Norway_Pine(SIH40=17)
Braastad_1977_time_to_breast_height_Norway_Pine<- function(
    SIH40
){
  ifelse(SIH40<8,warning("Function not applicable to SIH40 < 8.0"),NA)
  return(
    4.3641 + (83.0857/SIH40)
  )
}


