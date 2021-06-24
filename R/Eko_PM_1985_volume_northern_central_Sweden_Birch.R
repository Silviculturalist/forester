#' Volume function for Birch in northern and central Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 107
#'
#'  @description Multiple correlation coefficient: 0.95
#'
#'  Standard deviation about the function: 0.39
#'
#'  Sf/Standard deviation about the mean: 0.32
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
#' @param altitude Meters above sea level.
#' @param fertilised 1 if the stand has been fertilised, otherwise 0.
#' @param thinned 1 if the stand has been thinned, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#'
#' @return m3sk.
#' @export
#'
#' @examples
Eko_PM_1985_volume_northern_central_Sweden_Birch <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  latitude,
  altitude,
  fertilised,
  thinned,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.035
  b2 <-  -2.05

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.26244*log(basal_area.m2)+
    -0.459580*F4basal_area+
    +0.540420*F4age+
    -0.176040*log(stem_number_ha)+
    +0.201360*log(SIdm)+
    -1.68251*log(latitude)+
    -0.404000E-01*log(altitude)+
    +0.757200E-01*fertilised+
    +0.301200E-01*thinned+
    +0.401844E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +8.44862


  return(exp(
    lnVolume + 0.0755
  ))

}
