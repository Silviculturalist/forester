#' Basal area growth function for Pine in central Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 65.
#'
#' @description
#' Number of observations: 1631
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<160 \tab 160≤SI<200 \tab 200≤SI<240 \tab SI≥240  \cr
#' Unthinned \tab 223 \tab 461 \tab 835 \tab 1114 \cr
#' Thinned \tab 84 \tab 244 \tab 523 \tab 656  \cr
#' }
#'
#' Multiple correlation coefficient: 0.92
#'
#'
#' Standard deviation about the function, Sf : 0.409
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 40
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
Eko_PM_1985_basal_area_5_year_increment_southern_Sweden_Pine <- function(
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
    -1.04202*ba_quotient_chronic_mortality+
    -0.637943*ba_quotient_acute_mortality+
    -1.75160*basal_area_weighted_mean_diameter+
    -0.592599E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+ ## Assuming HKD is a typo and should be HK
    +0.637421E-01*thinned_previous_5_years+
    +0.462966E-01*fertilised+
    +0.522489E-01*ground_veg_indicator+
    -0.702839E-01*dry+
    -0.111568E-01*latitude+
    -0.466973E-01*TAX77




  if(SIdm<160){

    if(thinned==0){
      dependent_vars <-
        -0.497800E-01*basal_area.m2+
        +1.19990*log(basal_area.m2)+
        +0.114548E-04*stem_number_ha+
        +0.164713*log(stem_number_ha)+
        -0.884162E-03*age_at_breast_height+
        -0.564604*log(age_at_breast_height)+
        -0.153879E-01*basal_area_other_species+
        +0.579562

    } else {
      dependent_vars <-
        -0.302305E-01*basal_area.m2+
        +0.938947*log(basal_area.m2)+
        +0.563241E-03*stem_number_ha+
        +0.148914*log(stem_number_ha)+
        +0.419586E-02*age_at_breast_height+
        -1.15586*log(age_at_breast_height)+
        -0.138465E-01*basal_area_other_species+
        +2.72773
    }



  } else if(SIdm>=160 && SIdm<200){
    dependent_vars <-
      if(thinned==0){

        -0.123212E-01*basal_area.m2+
          +0.864851*log(basal_area.m2)+
          -0.497769E-04*stem_number_ha+
          +0.200066*log(stem_number_ha)+
          +0.211976E-02*age_at_breast_height+
          -0.821163*log(age_at_breast_height)+
          -0.941390E-02*basal_area_other_species+
          +1.59527

      } else {
        dependent_vars <-
          -0.216126E-02*basal_area.m2+
          +0.938131*log(basal_area.m2)+
          -0.169034E-03*stem_number_ha+
          +0.621225E-01*log(stem_number_ha)+
          +0.305833E-02*age_at_breast_height+
          -1.18279*log(age_at_breast_height)+
          -0.439063E-03*basal_area_other_species+
          +3.39954
      }



  } else if(SIdm>=200 && SIdm<240){

      if(thinned==0){
        dependent_vars <-
        -0.107718E-01*basal_area.m2+
          +0.796896*log(basal_area.m2)+
          -0.975686E-04*stem_number_ha+
          +0.230066*log(stem_number_ha)+
          -0.577520E-03*age_at_breast_height+
          -0.570857*log(age_at_breast_height)+
          -0.155230E-01*basal_area_other_species+
          +0.784527

      } else {
        dependent_vars <-
          -0.632941E-02*basal_area.m2+
          +0.767710*log(basal_area.m2)+
          -0.173551E-03*stem_number_ha+
          +0.173044*log(stem_number_ha)+
          +0.163026E-02*age_at_breast_height+
          -0.945376*log(age_at_breast_height)+
          -0.133437E-01*basal_area_other_species+
          +2.49514
      }



  } else if(SIdm>=240){

    if(thinned==0){

        dependent_vars <-
        -0.738511E-02*basal_area.m2+
        +0.809028*log(basal_area.m2)+
        -0.207393E-03*stem_number_ha+
        +0.199179*log(stem_number_ha)+
        +0.259619E-03*age_at_breast_height+
        -0.663161*log(age_at_breast_height)+
        -0.142082E-01*basal_area_other_species+
        +1.27892

    } else {
      dependent_vars <-
        -0.207497E-01*basal_area.m2+
        +1.00931*log(basal_area.m2)+
        -0.653755E-05*stem_number_ha+
        +0.851371E-01*log(stem_number_ha)+
        -0.307386E-02*age_at_breast_height+
        -0.635182*log(age_at_breast_height)+
        -0.110970E-01*basal_area_other_species+
        +1.57124
    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.0636
    )
  )



}
