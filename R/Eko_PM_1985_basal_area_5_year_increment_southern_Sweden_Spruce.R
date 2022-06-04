#' Basal area growth function for Spruce in southern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 68.
#'
#' @description
#' Number of observations: 4864
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<220 \tab 220≤SI<260 \tab 260≤SI<300 \tab SI≥300  \cr
#' Unthinned \tab 333 \tab 643 \tab 1121 \tab 1031 \cr
#' Thinned \tab 163 \tab 404 \tab 685 \tab 484  \cr
#' }
#'
#' Multiple correlation coefficient: 0.93
#'
#'
#' Standard deviation about the function, Sf : 0.433
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 48
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
Eko_PM_1985_basal_area_5_year_increment_southern_Sweden_Spruce <- function(
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
    -0.780391*ba_quotient_chronic_mortality+
    -0.252170*ba_quotient_acute_mortality+
    -0.318464E-01*thinned_previous_5_years+
    +0.778093E-01 *fertilised+
    +0.127135E-02*SIdm+
    +0.262484E-01*ground_veg_indicator+
    -0.736690E-01*dry+
    -0.269193E-01*latitude+
    -0.959785E-01*TAX77




  if(SIdm<220){

    if(thinned==0){
      dependent_vars <-
        -0.149200E-01*basal_area.m2+
        +0.794859*log(basal_area.m2)+
        -0.120956E-03*stem_number_ha+
        +0.255053*log(stem_number_ha)+
        -0.720252*log(age_at_breast_height)+
        -0.229139E-01*basal_area_other_species+
        +1.52732

    } else {
      dependent_vars <-
        -0.227763E-01*basal_area.m2+
        +0.838105*log(basal_area.m2)+
        +0.519813E-03*stem_number_ha+
        +0.141232*log(stem_number_ha)+
        -0.722723*log(age_at_breast_height)+
        -0.237689E-01*basal_area_other_species+
        +1.93218
    }



  } else if(SIdm>=220 && SIdm<260){

    if(thinned==0){
      dependent_vars <-
        -0.167127E-01*basal_area.m2+
        +0.794738*log(basal_area.m2)+
        -0.923244E-04*stem_number_ha+
        +0.279717*log(stem_number_ha)+
        -0.790588*log(age_at_breast_height)+
        -0.187801E-01*basal_area_other_species+
        +1.67230

    } else {
      dependent_vars <-
        -0.167448E-01*basal_area.m2+
        +0.835811*log(basal_area.m2)+
        -0.995431E-04*stem_number_ha+
        +0.258612*log(stem_number_ha)+
        -0.931549*log(age_at_breast_height)+
        -0.167010E-01*basal_area_other_species+
        +2.34225
    }



  } else if(SIdm>=260 && SIdm<300){

    if(thinned==0){

      dependent_vars <-
        -0.221875E-01*basal_area.m2+
        +0.832287*log(basal_area.m2)+
        -0.110872E-03*stem_number_ha+
        +0.271386*log(stem_number_ha)+
        -0.735989*log(age_at_breast_height)+
        -0.196143E-01*basal_area_other_species+
        +1.50310

    } else {
      dependent_vars <-
        -0.203970E-01*basal_area.m2+
        +0.836890*log(basal_area.m2)+
        -0.755155E-04*stem_number_ha+
        +0.248563*log(stem_number_ha)+
        -0.716504*log(age_at_breast_height)+
        -0.151436E-01*basal_area_other_species+
        +1.50719
    }



  } else if(SIdm>=300){

    if(thinned==0){

      dependent_vars <-
        -0.243263E-01*basal_area.m2+
        +0.902730*log(basal_area.m2)+
        -0.706319E-04*stem_number_ha+
        +0.198283*log(stem_number_ha)+
        -0.713230*log(age_at_breast_height)+
        -0.135840E-01*basal_area_other_species+
        +1.71136

    } else {
      dependent_vars <-
        -0.218319E-01*basal_area.m2+
        +0.855200*log(basal_area.m2)+
        -0.176554E-03*stem_number_ha+
        +0.269091*log(stem_number_ha)+
        -0.765104*log(age_at_breast_height)+
        -0.180257E-01*basal_area_other_species+
        +1.62508
    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.0737
    )
  )



}
