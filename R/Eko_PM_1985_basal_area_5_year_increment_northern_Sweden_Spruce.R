#' Basal area growth function for Norway Spruce in northern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 66.
#'
#' @description
#' Number of observations: 3959
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<160 \tab 160≤SI<200 \tab SI<200   \cr
#' Unthinned \tab 1405 \tab 1012 \tab 431  \cr
#' Thinned \tab 476 \tab 455 \tab 180 \cr
#' }
#'
#' Multiple correlation coefficient: 0.94
#'
#'
#' Standard deviation about the function, Sf : 0.391
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 36
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
Eko_PM_1985_basal_area_5_year_increment_northern_Sweden_Spruce <- function(
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

  bilberry_cowberry <- if(vegetation%in%c(13:14)){
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
    -0.767477*ba_quotient_chronic_mortality+
    -0.514297*ba_quotient_acute_mortality+
    -1.43974*basal_area_weighted_mean_diameter+
    -0.386338E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.204732*fertilised+
    +0.186343*ground_veg_indicator+
    +0.392021E-01*bilberry_cowberry+
    -0.807207E-01*dry+
    +0.833252




  if(SIdm<160){

    if(thinned==0){
      dependent_vars <-
        -0.736655E-02*basal_area.m2+
        +0.875788*log(basal_area.m2)+
        -0.642060E-04*stem_number_ha+
        +0.125396*log(stem_number_ha)+
        +0.159356E-02*age_at_breast_height+
        -0.764340*log(age_at_breast_height)+
        -0.594334E-02*basal_area_other_species


    } else {
      dependent_vars <-
        -0.187226-01*basal_area.m2+
        +0.855970*log(basal_area.m2)+
        +0.106942E-03*stem_number_ha+
        +0.107612*log(stem_number_ha)+
        +0.321033E-02*age_at_breast_height+
        -0.737062*log(age_at_breast_height)+
        -0.206053E-01*basal_area_other_species

    }



  } else if(SIdm>=160 && SIdm<200){
    dependent_vars <-
      if(thinned==0){

        -0.191493E-01*basal_area.m2+
          +0.942389*log(basal_area.m2)+
          -0.145476E-03*stem_number_ha+
          +0.158511*log(stem_number_ha)+
          +0.289628E-02*age_at_breast_height+
          -0.804217*log(age_at_breast_height)+
          -0.125949E-01*basal_area_other_species


      } else {
        dependent_vars <-
          -0.255254E-01*basal_area.m2+
          +0.955380*log(basal_area.m2)+
          -0.642149E-04*stem_number_ha+
          +0.164265*log(stem_number_ha)+
          +0.554025E-02*age_at_breast_height+
          -0.866520*log(age_at_breast_height)+
          -0.889755E-02*basal_area_other_species

      }



  } else if(SIdm>=200){

    if(thinned==0){
      dependent_vars <-
        -0.210737E-01*basal_area.m2+
        +0.932275*log(basal_area.m2)+
        -0.572335E-04*stem_number_ha+
        +0.152017*log(stem_number_ha)+
        +0.342622E-02*age_at_breast_height+
        -0.811183*log(age_at_breast_height)+
        -0.905176E-02*basal_area_other_species


    } else {
      dependent_vars <-
        -0.133941E-01*basal_area.m2+
        +0.837783*log(basal_area.m2)+
        -0.245946E-03*stem_number_ha+
        +0.205142*log(stem_number_ha)+
        +0.602419E-02*age_at_breast_height+
        -0.862195*log(age_at_breast_height)+
        -0.135941E-01*basal_area_other_species

    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.0564
    )
  )



}
