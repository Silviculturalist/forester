#' Volume function for Beech in Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 108
#'
#'  @description Multiple correlation coefficient: 0.99
#'
#'  Standard deviation about the function: 0.28
#'
#'  Sf/Standard deviation about the mean: 0.17
#'
#'
#'
#'
#' @param basal_area.m2 Basal area over bark of Beech (m2/ha)
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param stem_number_ha Number of stems per hectare.
#' @param SI Site index H100 Spruce , m.
#' @param thinned 1 if the stand has been thinned, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#'
#' @return m3sk.
#' @export
#'
#' @examples
Eko_PM_1985_volume_Sweden_Beech <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  thinned,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.02
  b2 <-  -2.3

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.111600E-01*basal_area.m2+
    +1.30527*log(basal_area.m2)+
    -0.676190*F4basal_area+
    +0.490740*F4age+
    -0.151930*log(stem_number_ha)+
    -0.572600E-01*log(SIdm)+
    +0.628000E-01*thinned+
    +0.203927E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +2.85509


  return(exp(
    lnVolume + 0.0392
  ))

}
