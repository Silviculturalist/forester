#' Volume function for other Broadleaves in southern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 109
#'
#'  @description Multiple correlation coefficient: 0.97
#'
#'  Standard deviation about the function: 0.37
#'
#'  Sf/Standard deviation about the mean: 0.26
#'
#'
#'
#'
#' @param basal_area.m2 Basal area over bark of Broadleaves (m2/ha)
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param stem_number_ha Number of stems per hectare.
#' @param SI Site index H100 Spruce , m.
#' @param latitude Degrees.
#' @param thinned 1 if the stand has been thinned, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#'
#' @return m3sk/ha
#' @export
#'
#' @examples
Eko_PM_1985_volume_southern_Sweden_Broadleaves <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  latitude,
  thinned,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  total_basal_area <- basal_area.m2+basal_area_other_species

  b1 <-  -0.075
  b2 <-  -2.1

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.148700E-01*basal_area.m2+
    +1.29359*log(basal_area.m2)+
    -0.784820*F4basal_area+
    +1.18741*F4age+
    -0.135830*log(stem_number_ha)+
    +0.219890*log(SIdm)+
    +2.02656*log(latitude)+
    +0.242500E-01*thinned+
    +0.859600E-01*total_basal_area+
    +0.509488E-03*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +7.50102

  return(exp(
    lnVolume + 0.0671
  ))

}
