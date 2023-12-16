#' Annual Diameter growth for Norway Spruce in Norway from Braastad 1974
#'
#' @source Braastad, H. 1974. Diametertilvekstfunksjoner for gran. Diameter increment functions for Picea abies. Medd. norsk inst. skogforskning:31:1. function 6.
#'
#' @details
#' Multiple correlation coefficient = 0.740
#' Residual spread = 0.585 mm.
#' Spread about the mean Diameter increment = 18.4 percent.
#'
#' @param SIH40 Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.
#' @param age Age at breast height.
#' @param dominant_height Dominant height of stand (arithmetic mean height of the 100 trees with largest diameter).
#' @param QMD_remaining Quadratic Mean Diameter at start.
#' @param BA_m2_ha_after_thinning Basal Area m2/ha at start.
#' @param Volume_remaining Volume m3 at start.
#' @param thinned_BA_m2_ha_last_thinning Basal Area m2/ha removed during the last thinning
#' @param thinned_BA_m2_ha_second_last_thinning Basal Area m2/ha removed during the second last thinning
#' @param thinned_BA_m2_ha_third_last_thinning Basal Area m2/ha removed during the third last thinning
#'
#' @return Diameter increment per annum, cm.
#' @export
Braastad_1974_BA_increment_Norway_Norway_Spruce <- function(
  SIH40,
  age,
  dominant_height,
  QMD_remaining,
  BA_m2_ha_after_thinning,
  Volume_remaining,
  thinned_BA_m2_ha_last_thinning,
  thinned_BA_m2_ha_second_last_thinning,
  thinned_BA_m2_ha_third_last_thinning
){

  return(
    -0.649 + 52.370*(1/age) - 220.215*((1/age)^2) - 7.237 * (1/dominant_height) + 2.314*(QMD_remaining/dominant_height) + 0.188*SIH40 + 0.088*BA_m2_ha_after_thinning+ 22.533*(1/BA_m2_ha_after_thinning) - 0.929*sqrt(BA_m2_ha_after_thinning) - 64.952*(1/Volume_remaining) - 0.00550*SIH40*(Volume_remaining/dominant_height) + 0.0353*(thinned_BA_m2_ha_last_thinning+thinned_BA_m2_ha_second_last_thinning+thinned_BA_m2_ha_third_last_thinning)
  )

}
#' Diameter quotient between felled trees and unthinned stand for Norway Spruce.
#'
#' @source Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 371. function (VI.A-2)
#'
#' @details
#' Multiple correlation coefficient: 0.463
#'
#' Residual spread : 0.134
#'
#' Spread about the mean: 16.8 percent.
#'
#' @param quadratic_mean_diameter_before Quadratic mean diameter of stand before thinning. i.e. [forester::quadratic_mean_diameter()]
#' @param Basal_area_m2_ha_before Basal area m2 / ha before thinning.
#' @param stems_remaining Stems per hectare remaining after thinning.
#' @param SIH40 Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.
#' @param age Age at breast height of stand.
#'
#' @return Diameter quotient.
#' @export
Braastad_1974_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Norway_Spruce <- function(
  quadratic_mean_diameter_before,
  Basal_area_m2_ha_before,
  stems_remaining,
  SIH40,
  age
){

  return(
    0.683 + 0.0385*quadratic_mean_diameter_before - 0.000553*(quadratic_mean_diameter_before^2) - 0.00455*Basal_area_m2_ha_before + 0.0000543*stems_remaining - 0.00828*SIH40 - 0.00178*age
  )
}
#' Calculate initial Basal Area for stands of Norway Spruce in Norway from Braastad 1974.
#'
#' @source Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 369. function (V.1)
#'
#' @details
#' Multiple correlation coefficient: 0.863.
#'
#' Residual spread : 3.2 m^2.
#'
#' Spread about the mean: 16.7 percent of mean Basal area.
#'
#' @param SIH40 Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.
#' @param stems_per_ha Stems per hectare.
#' @param dominant_height Dominant height.
#'
#' @return Basal Area m2 / ha.
#' @export
Braastad_1974_initial_Basal_area_Norway_Spruce <- function(
  SIH40,
  stems_per_ha,
  dominant_height
){
  return(
    -29.152 + 0.379*SIH40 + 3.247*dominant_height + 0.0026*stems_per_ha
  )
}
#' Volume of the BA weighted mean tree for Norway Spruce in Norway from Braastad 1974
#'
#' @description Uses single tree volume functions from Vestjordet (1967).
#' Height is adjusted by the factor 0.967 such that the functions returned 'measured volume', cf. Eide 1923, as differed from volume estimates from means.
#'
#' @source Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 397. Code section: 1160,1162,1163.
#'
#' @param diameter_cm Quadratic mean diameter, cm.
#' @param height_m Lorey's mean height, m. e.g. [forester::Tveite_1967_Loreys_mean_height_Norway_Norway_Spruce()]
#'
#' @return Volume of the BA weighted mean tree (m3).
#' @export
Braastad_1974_QMD_tree_volume_Norway_Norway_Spruce <- function(
  diameter_cm,
  height_m
){

    height_m<- 0.967*height_m

    return(
    ifelse(diameter_cm<10,
           (0.52 + 0.02403*(diameter_cm^2)*height_m + 0.01463*diameter_cm*(height_m^2)-0.10983*(height_m^2) + 0.15195*diameter_cm*height_m)/1000,
           ifelse(diameter_cm<=13,
                  (-31.57+0.0016*diameter_cm*(height_m^2)+0.0186*(height_m^2)+0.63*diameter_cm*height_m - 2.34*height_m + 3.20*diameter_cm)/1000,
                  (10.14 + 0.0124*(diameter_cm^2)*height_m + 0.03117*diameter_cm*(height_m^2) - 0.36381*(height_m^2) + 0.28578*diameter_cm*height_m)/1000
                  )
           )
    )


}
