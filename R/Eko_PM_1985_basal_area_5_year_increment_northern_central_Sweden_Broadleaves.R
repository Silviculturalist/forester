#' Basal area growth function for Broadleaves in northern and central Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 73
#'
#' @description
#' Number of observations: 1367
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<160 \tab 160≤SI<200 \tab 200≤SI<240 \tab SI>240   \cr
#' Unthinned \tab 319 \tab 335 \tab 225 \tab 160  \cr
#' Thinned \tab 85 \tab 100 \tab 75 \tab 68 \cr
#' }
#'
#' Multiple correlation coefficient: 0.89
#'
#'
#' Standard deviation about the function, Sf : 0.608
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 47
#'
#' @param basal_area.m2 Basal area over bark (m2/ha)
#' @param stem_number_ha Number of stems per hectare.
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param SI Site index H100 Spruce , m.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
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
#' @param ba_quotient_chronic_mortality Quotient of basal area (total?) expected to die during the next five-year period, from e.g. crowding, drought (factors that have affected the growth of the stand)
#'
#' @return Basal area growth over bark during the five year growth period.
#' @export
Eko_PM_1985_basal_area_5_year_increment_northern_central_Sweden_Broadleaves <- function(
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
  latitude,
  ba_quotient_chronic_mortality
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
    -0.345933*ba_quotient_chronic_mortality+
    -0.138015*ground_veg_indicator+
    -0.650878E-01*bilberry_cowberry+
    -0.175149E-01*latitude+
    -0.570035E-03*altitude+
    +0.151318*TAX77




  if(SIdm<160){

    if(thinned==0){
      dependent_vars <-
        +0.865166E-01*basal_area.m2+
        +0.755603*log(basal_area.m2)+
        -0.806548E-03*stem_number_ha+
        +0.275974*log(stem_number_ha)+
        -0.540881E-02*age_at_breast_height+
        -0.117056*log(age_at_breast_height)+
        -0.187866E-01*basal_area_other_species+
        -1.18519


    } else {
      dependent_vars <-
        +0.865166E-01*basal_area.m2+
        +0.755603*log(basal_area.m2)+
        -0.806548E-03*stem_number_ha+
        +0.275974*log(stem_number_ha)+
        -0.540881E-02*age_at_breast_height+
        -0.117056*log(age_at_breast_height)+
        -0.187866E-01*basal_area_other_species+
        -0.952398
    }



  } else if(SIdm>=160 && SIdm<200){
    if(thinned==0){
      dependent_vars <-
        -0.129773E-01*basal_area.m2+
        +0.989525*log(basal_area.m2)+
        -0.715363E-04*stem_number_ha+
        +0.490676E-01*log(stem_number_ha)+
        +0.218728E-02*age_at_breast_height+
        -0.944317*log(age_at_breast_height)+
        -0.143834E-01*basal_area_other_species+
        +2.78296

    } else {
      dependent_vars <-
        -0.129773E-01*basal_area.m2+
        +0.989525*log(basal_area.m2)+
        -0.715363E-04*stem_number_ha+
        +0.490676E-01*log(stem_number_ha)+
        +0.218728E-02*age_at_breast_height+
        -0.944317*log(age_at_breast_height)+
        -0.143834E-01*basal_area_other_species+
        +2.87671

    }



  } else if(SIdm>=200 && SIdm<240){

    if(thinned==0){
      dependent_vars <-
        +0.517826E-01*basal_area.m2+
        +0.768565*log(basal_area.m2)+
        -0.381320E-03*stem_number_ha+
        +0.201267*log(stem_number_ha)+
        +0.131078E-02*age_at_breast_height+
        -0.831523*log(age_at_breast_height)+
        -0.122796E-01*basal_area_other_species+
        +1.65650


    } else {
      dependent_vars <-
        +0.517826E-01*basal_area.m2+
        +0.768565*log(basal_area.m2)+
        -0.381320E-03*stem_number_ha+
        +0.201267*log(stem_number_ha)+
        +0.131078E-02*age_at_breast_height+
        -0.831523*log(age_at_breast_height)+
        -0.122796E-01*basal_area_other_species+
        +1.59209
    }

  } else if(SIdm>=240){

    if(thinned==0){
      dependent_vars <-
        +0.243920E-02*basal_area.m2+
        +0.857832*log(basal_area.m2)+
        -0.949555E-04*stem_number_ha+
        +0.192173*log(stem_number_ha)+
        -0.292753E-02*age_at_breast_height+
        -0.570009*log(age_at_breast_height)+
        -0.240816E-01*basal_area_other_species+
        +0.916942


    } else {
      dependent_vars <-
        +0.243920E-02*basal_area.m2+
        +0.857832*log(basal_area.m2)+
        -0.949555E-04*stem_number_ha+
        +0.192173*log(stem_number_ha)+
        -0.292753E-02*age_at_breast_height+
        -0.570009*log(age_at_breast_height)+
        -0.240816E-01*basal_area_other_species+
        +1.17865

    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.1648
    )
  )



}
