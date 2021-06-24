#' Basal area growth function for Pine in northern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 63.
#'
#' @description
#' Number of observations: 3461
#'
#' \tabular{lrrr}{
#' Thinning \tab SI<160 \tab 160≤SI<200 \tab SI≥200 \cr
#' Unthinned \tab 920 \tab 1014 \tab 523 \cr
#' Thinned \tab 319 \tab 441 \tab 244 \cr
#' }
#'
#' Multiple correlation coefficient: 0.91
#'
#'
#' Standard deviation about the function, Sf : 0.411
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 41
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
#'
#' @return Basal area growth over bark during the five year growth period.
#' @export
#'
#' @examples
Eko_PM_1985_basal_area_5_year_increment_northern_Sweden_Pine <- function(
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
  TAX77=0
){
  SIdm <- SI*10

  ground_veg_indicator <- if(vegetation%in%c(1:6,8,9)){
    1
  } else if(vegetation%in%c(1:9) && south_sweden==TRUE){
    1
  } else {
    0
  }

  if(soil_moisture%in%c(5)){ #vattensjuk eller mycket vattensjuk tolkas som blöt.
    wet <- 1
  } else {
    wet <- 0
  }

  independent_vars <-
    -0.598419*ba_quotient_chronic_mortality+
    -0.486198*ba_quotient_acute_mortality+
    -0.952624E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.674527E-01*thinned_previous_5_years+
    +0.100135*ground_veg_indicator+
    -0.104076*wet+
    -0.329437E-01*log(altitude)+
    +0.526479E-01*TAX77+
    +0.164446




  if(SIdm<160){

    if(thinned==0){
dependent_vars <-
    -0.342051E-01*basal_area.m2+
    +0.757840*log(basal_area.m2)+
    -0.161442E-03*stem_number_ha+
    +0.367048*log(stem_number_ha)+
    +0.313386E-02*age_at_breast_height+
    -0.842335*log(age_at_breast_height)+
    -0.157312E-01*basal_area_other_species

    } else {
dependent_vars <-
    -0.222808E-01*basal_area.m2+
    +0.707173*log(basal_area.m2)+
    -0.407064E-03*stem_number_ha+
    +0.386522*log(stem_number_ha)+
    +0.309020E-02*age_at_breast_height+
    -0.840856*log(age_at_breast_height)+
    -0.168721E-01*basal_area_other_species
    }



  } else if(SIdm>=160 && SIdm<200){

    if(thinned==0){
dependent_vars <-

      -0.264194E-01*basal_area.m2+
        +0.759517*log(basal_area.m2)+
        -0.172838E-03*stem_number_ha+
        +0.354319*log(stem_number_ha)+
        +0.282339E-02*age_at_breast_height+
        -0.830969*log(age_at_breast_height)+
        -0.920265E-02*basal_area_other_species

    } else {
dependent_vars <-
      -0.215557E-01*basal_area.m2+
        +0.678298*log(basal_area.m2)+
        -0.223194E-03*stem_number_ha+
        +0.345910*log(stem_number_ha)+
        +0.230893E-02*age_at_breast_height+
        -0.759426*log(age_at_breast_height)+
        -0.129081E-01*basal_area_other_species
    }



  } else if(SIdm>=200){

    if(thinned==0){
dependent_vars <-
      -0.242773E-01*basal_area.m2+
        +0.743286*log(basal_area.m2)+
        -0.127080E-03*stem_number_ha+
        +0.328240*log(stem_number_ha)+
        +0.203892E-02*age_at_breast_height+
        -0.756105*log(age_at_breast_height)+
        -0.136312E-01*basal_area_other_species

    } else {
dependent_vars <-
      -0.100435E-01*basal_area.m2+
        +0.659451*log(basal_area.m2)+
        -0.181913E-03*stem_number_ha+
        +0.369130*log(stem_number_ha)+
        +0.227817E-02*age_at_breast_height+
        -0.793134*log(age_at_breast_height)+
        -0.817145E-02*basal_area_other_species
    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.0645
    )
  )



}
