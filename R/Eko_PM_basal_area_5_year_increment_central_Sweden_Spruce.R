#' Basal area growth function for Spruce in central Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 67.
#'
#' @description
#' Number of observations: 1773
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<180 \tab 180≤SI<220 \tab 220≤SI<260 \tab SI≥260  \cr
#' Unthinned \tab 212 \tab 291 \tab 322 \tab 270 \cr
#' Thinned \tab 102 \tab 169 \tab 235 \tab 172  \cr
#' }
#'
#' Multiple correlation coefficient: 0.93
#'
#'
#' Standard deviation about the function, Sf : 0.427
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 39
#'
#' @param basal_area.m2 Basal area over bark (m2/ha)
#' @param stem_number_ha Number of stems per hectare.
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param SI Site index H100 Pine , m.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter coressponding to mean basal area (m) of other species on plot.
#' @param thinned 1 if the stand been thinned, otherwise 0.
#' @param thinned_previous_5_years 1 if the stand has been thinned within 5 years, otherwise 0.
#' @param vegetation Variable indicating vegetation type scaled from -5 to +4 as follows:
#'
#' \tabular{llr}{
#' Field Layer Code (NFI) \tab Description \tab Index \cr
#' 1\tab Tall herbs w/o dwarf shrubs \tab 4 \cr
#' 2\tab Tall herbs with bilberry \tab 2.5 \cr
#' 3\tab Tall herbs with cowberry \tab 2 \cr
#' 4\tab Low herbs w/o dwarf shrubs \tab 3 \cr
#' 5\tab Low herbs with bilberry \tab 2.5 \cr
#' 6\tab Low herbs with cowberry \tab 2 \cr
#' 7\tab No field layer \tab 3 \cr
#' 8\tab broad-leafed grasses \tab 2.5 \cr
#' 9\tab narrow-leaved grasses \tab 1.5 \cr
#' 10\tab Sedge, tall \tab -3 \cr
#' 11\tab Sedge, low \tab -3 \cr
#' 12\tab Horsetail \tab 1 \cr
#' 13\tab Bilberry \tab 0 \cr
#' 14\tab Cowberry \tab -0.5 \cr
#' 15\tab Crowberry \tab -3 \cr
#' 16\tab Poor shrub \tab -5 \cr
#' 17\tab Lichen-rich \tab -0.5 \cr
#' 18\tab Lichen-dominated \tab -1 \cr
#' }
#' @param south_sweden 1 if the plot is situated in southern Sweden.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param altitude Metres above sea level
#' @param TAX77 Always 0 for simulation. Otherwise see original text.
#' @param fertilised 1 if the stand has been fertilised, otherwise 0.
#' @param latitude Latitude, degrees.
#'
#' @return Basal area growth over bark during the five year growth period.
#' @export
#'
#' @examples
Eko_PM_basal_area_5_year_increment_central_Sweden_Spruce <- function(
  basal_area.m2,
  stem_number_ha,
  age_at_breast_height,
  basal_area_other_species,
  SI,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species,
  thinned,
  thinned_previous_5_years,
  vegetation,
  south_sweden,
  soil_moisture,
  altitude,
  TAX77=0,
  fertilised,
  latitude
){
  SIdm <- SI*10

  ground_veg_indicator <- if(vegetation%in%c(1:6,8,9)){
    1
  } else if(vegetation%in%c(1:9) && south_sweden==TRUE){
    1
  } else {
    0
  }

  if(soil_moisture==5){ #vattensjuk eller mycket vattensjuk tolkas som blöt.
    wet <- 1
  } else {
    wet <- 0
  }

  if(soil_moisture==1){ #torr eller mycket torr tolkas som torr.
    dry <- 1
  } else {
    dry <- 0
  }

  independent_vars <-
    -1.16597*ba_quotient_chronic_mortality+
    -0.299327*ba_quotient_acute_mortality+
    +0.783806E-01*thinned_previous_5_years+
    +0.572131E-01*ground_veg_indicator+
    -0.112938E-01*wet+ #Typo in book. No number or sign after E. Assuming -1.
    +0.546176E-01*latitude+
    +0.332621E-01*TAX77




  if(SIdm<180){

    if(thinned==0){
      dependent_vars <-
        -0.802837E-02*basal_area.m2+
        +0.751220*log(basal_area.m2)+
        -0.800241E-04*stem_number_ha+
        +0.239814*log(stem_number_ha)+
        -0.148757E-02*age_at_breast_height+
        -0.476534*log(age_at_breast_height)+
        -0.308451E-01*basal_area_other_species+
        -4.02484

    } else {
      dependent_vars <-
        -0.330623E-01*basal_area.m2+
        +1.06539*log(basal_area.m2)+
        +0.145290E-03*stem_number_ha+
        +0.422450E-01*log(stem_number_ha)+
        +0.110998E-01*age_at_breast_height+
        -1.71468*log(age_at_breast_height)+
        -0.236447E-01*basal_area_other_species+
        +1.06383
    }



  } else if(SIdm>=180 && SIdm<220){
    dependent_vars <-
      if(thinned==0){

        -0.211171E-01*basal_area.m2+
          +0.837241*log(basal_area.m2)+
          -0.800241E-04*stem_number_ha+
          +0.239814*log(stem_number_ha)+
          +0.492578E-02*age_at_breast_height+
          -0.839650*log(age_at_breast_height)+
          -0.269523E-02*basal_area_other_species+
          -2.91926

      } else {
        dependent_vars <-
          -0.180419E-01*basal_area.m2+
          +0.943986*log(basal_area.m2)+
          +0.145290E-03*stem_number_ha+
          +0.422450E-01*log(stem_number_ha)+
          +0.525585E-02*age_at_breast_height+
          -0.982261*log(age_at_breast_height)+
          -0.786807E-02*basal_area_other_species+
          -1.56544
      }



  } else if(SIdm>=220 && SIdm<260){

    if(thinned==0){
      dependent_vars <-
        -0.263745E-01*basal_area.m2+
        +0.915196*log(basal_area.m2)+
        -0.800241E-04*stem_number_ha+
        +0.239814*log(stem_number_ha)+
        -0.384471E-02*age_at_breast_height+
        -0.847753*log(age_at_breast_height)+
        -0.252559E-01*basal_area_other_species+
        +2.85518

    } else {
      dependent_vars <-
        -0.217674E-01*basal_area.m2+
        +0.847682*log(basal_area.m2)+
        -0.145290E-03*stem_number_ha+
        +0.422450E-01*log(stem_number_ha)+
        +0.101626E-01*age_at_breast_height+
        -1.37782*log(age_at_breast_height)+
        -0.268779E-01*basal_area_other_species+
        +0.178428
    }



  } else if(SIdm>=260){

    if(thinned==0){

      dependent_vars <-
        -0.244742E-01*basal_area.m2+
        +0.787195*log(basal_area.m2)+
        -0.800241E-04*stem_number_ha+
        +0.239814*log(stem_number_ha)+
        +0.371613E-02*age_at_breast_height+
        -0.561641*log(age_at_breast_height)+
        -0.298097E-01*basal_area_other_species+
        -3.17570

    } else {
      dependent_vars <-
        -0.239679E-01*basal_area.m2+
        +0.924765*log(basal_area.m2)+
        +0.145290E-03*stem_number_ha+
        +0.422450E-01*log(stem_number_ha)+
        +0.631561E-03*age_at_breast_height+
        -0.893401*log(age_at_breast_height)+
        -0.908286E-02*basal_area_other_species+
        -1.46143
    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.0712
    )
  )



}
