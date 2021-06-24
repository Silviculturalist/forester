#' Volume function for other Broadleaves in northern and central Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 109
#'
#'  @description Multiple correlation coefficient: 0.94
#'
#'  Standard deviation about the function: 0.41
#'
#'  Sf/Standard deviation about the mean: 0.33
#'
#'
#'
#'
#' @param basal_area.m2 Basal area over bark of other broadleaves (m2/ha)
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param stem_number_ha Number of stems per hectare.
#' @param SI Site index H100 Spruce , m.
#' @param latitude Degrees.
#' @param altitude Meters above sea level.
#' @param thinned 1 if the stand has been thinned, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#'
#' @return m3sk.
#' @export
#'
#' @examples
Eko_PM_1985_volume_northern_central_Sweden_Broadleaves <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  latitude,
  altitude,
  thinned,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.04
  b2 <-  -2.3

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.26649*log(basal_area.m2)+
    -0.580030*F4basal_area+
    +0.486310*F4age+
    -0.172050*log(stem_number_ha)+
    +0.174930*log(SIdm)+
    -1.51968*log(latitude)+
    -0.368300E-01*log(altitude)+
    +0.547400E-01*thinned+
    +0.417126E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +7.79034

  return(exp(
    lnVolume + 0.0853
  ))

}
