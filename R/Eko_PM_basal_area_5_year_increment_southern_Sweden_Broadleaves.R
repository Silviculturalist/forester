#' Basal area growth function for Broadleaves in southern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 74
#'
#' @description
#' Number of observations: 1404
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<240 \tab 240≤SI<280 \tab 280≤SI<320 \tab SI>320   \cr
#' Unthinned \tab 131 \tab 209 \tab 424 \tab 266 \cr
#' Thinned \tab 60 \tab 75 \tab 151 \tab 88 \cr
#' }
#'
#' Multiple correlation coefficient: 0.90
#'
#'
#' Standard deviation about the function, Sf : 0.622
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 45
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
#'
#' @return Basal area growth over bark during the five year growth period.
#' @export
#'
#' @examples
Eko_PM_basal_area_5_year_increment_southern_Sweden_Broadleaves <- function(
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
    -1.20049*ba_quotient_chronic_mortality+
    -0.367064*ba_quotient_acute_mortality+
    +0.125048*thinned_previous_5_years+
    +0.246684*fertilised+
    +0.141955*ground_veg_indicator+
    +0.354866E-01*latitude+
    -0.361988E-03*altitude




  if(SIdm<240){

    if(thinned==0){
      dependent_vars <-
        +0.857153*log(basal_area.m2)+
        -0.541853E-04*stem_number_ha+
        +0.152684*log(stem_number_ha)+
        -0.803085E-02*age_at_breast_height+
        -0.570230*log(age_at_breast_height)+
        -0.100518*log(basal_area_other_species)+ #assuming KG is GK typo.
        -1.93895


    } else {
      dependent_vars <-
        +0.857153*log(basal_area.m2)+
        -0.541853E-04*stem_number_ha+
        +0.152684*log(stem_number_ha)+
        -0.803085E-02*age_at_breast_height+
        -0.570230*log(age_at_breast_height)+
        -0.100518*log(basal_area_other_species)+ #assuming KG is GK typo.
        -2.01960
    }



  } else if(SIdm>=240 && SIdm<280){
    if(thinned==0){
      dependent_vars <-
        +0.794405*log(basal_area.m2)+
        -0.247009*stem_number_ha+
        +0.202344*log(stem_number_ha)+
        -0.250423*age_at_breast_height+
        -0.669629*log(age_at_breast_height)+
        -0.101205*log(basal_area_other_species)+ #assuming KG is GK typo.
        -1.93895

    } else {
      dependent_vars <-
        +0.794405*log(basal_area.m2)+
        -0.247009*stem_number_ha+
        +0.202344*log(stem_number_ha)+
        -0.250423*age_at_breast_height+
        -0.669629*log(age_at_breast_height)+
        -0.101205*log(basal_area_other_species)+ #assuming KG is GK typo.
        -2.01960

    }



  } else if(SIdm>=280 && SIdm<320){

    if(thinned==0){
      dependent_vars <-
        +0.782374*log(basal_area.m2)+
        -0.125111E-03*stem_number_ha+
        +0.239626*log(stem_number_ha)+
        -0.787146E-03*age_at_breast_height+
        -0.733575*log(age_at_breast_height)+
        -0.823802E-01*log(basal_area_other_species)+ #assuming KG is GK typo.
        -1.93895


    } else {
      dependent_vars <-
        +0.782374*log(basal_area.m2)+
        -0.125111E-03*stem_number_ha+
        +0.239626*log(stem_number_ha)+
        -0.787146E-03*age_at_breast_height+
        -0.733575*log(age_at_breast_height)+
        -0.823802E-01*log(basal_area_other_species)+ #assuming KG is GK typo.
        -2.01960
    }

  } else if(SIdm>=320){

    if(thinned==0){
      dependent_vars <-
        +0.771398*log(basal_area.m2)+
        +0.427071E-04*stem_number_ha+
        +0.167037*log(stem_number_ha)+
        -0.190695E-02*age_at_breast_height+
        -0.587696*log(age_at_breast_height)+
        -0.113489*log(basal_area_other_species)+ #assuming KG is GK typo.
        -1.93895


    } else {
      dependent_vars <-
        +0.771398*log(basal_area.m2)+
        +0.427071E-04*stem_number_ha+
        +0.167037*log(stem_number_ha)+
        -0.190695E-02*age_at_breast_height+
        -0.587696*log(age_at_breast_height)+
        -0.113489*log(basal_area_other_species)+ #assuming KG is GK typo.
        -2.01960

    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.1734
    )
  )



}
