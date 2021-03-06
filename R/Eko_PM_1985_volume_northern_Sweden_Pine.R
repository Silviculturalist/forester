#' Volume function for Scots Pine in northern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 105
#'
#'  @description Multiple correlation coefficient: 0.98
#'
#'  Standard deviation about the function: 0.23
#'
#'  Sf/Standard deviation about the mean: 0.17
#'
#'
#'
#'
#' @param basal_area.m2 Basal area over bark of Scots Pine (m2/ha)
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param stem_number_ha Number of stems per hectare.
#' @param SI Site index H100 Pine , m.
#' @param thinned_previous_five_years 1 if the stand has been thinned during the last five years, otherwise 0.
#' @param thinned_before_previous_five_years 1 if the stand has been thinned before the last five years, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#'
#' @return m3sk.
#' @export
#'
#' @examples
Eko_PM_1985_volume_northern_Sweden_Pine <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  thinned_previous_five_years,
  thinned_before_previous_five_years,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.06
  b2 <-  -2.3

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.24296*log(basal_area.m2)+
    -0.472530*F4basal_area+
    +1.05864*F4age+
    -0.170140*log(stem_number_ha)+
    +0.247550*log(SIdm)+
    +0.213800E-01*thinned_before_previous_five_years+
    +0.295300E-01*thinned_previous_five_years+
    +0.510332E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +1.08339


  return(exp(
    lnVolume + 0.0275
  ))

}
