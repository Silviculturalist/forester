#' Volume function for Birch in southern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 107
#'
#'  @description Multiple correlation coefficient: 0.96
#'
#'  Standard deviation about the function: 0.35
#'
#'  Sf/Standard deviation about the mean: 0.26
#'
#'
#'
#'
#' @param basal_area.m2 Basal area over bark of Birch (m2/ha)
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param stem_number_ha Number of stems per hectare.
#' @param SI Site index H100 Spruce , m.
#' @param latitude Degrees
#' @param fertilised 1 if the stand has been fertilised, otherwise 0.
#' @param thinned 1 if the stand has been thinned, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#'
#' @return m3sk.
#' @export
#'
#' @examples
Eko_PM_1985_volume_southern_Sweden_Birch <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  latitude,
  fertilised,
  thinned,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.07
  b2 <-  -2.1

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.786906E-02*basal_area.m2+
    +1.35254*log(basal_area.m2)+
    -1.30862*basal_area_weighted_mean_diameter+
    -0.524630*F4basal_area+
    +1.01779*F4age+
    -0.254630*log(stem_number_ha)+
    +0.204880*log(SIdm)+
    +2.75025*log(latitude)+
    +0.774000E-01*fertilised+
    +0.434800E-01*thinned+
    +0.250449E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    -9.38127


  return(exp(
    lnVolume + 0.0595
  ))

}
