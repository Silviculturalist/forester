#' Basal area growth function for Birch in southern Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 70
#'
#' @description
#' Number of observations: 3072
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<220 \tab 220≤SI<260 \tab 260≤SI<300 \tab SI>300   \cr
#' Unthinned \tab 249 \tab 406 \tab 772 \tab 756  \cr
#' Thinned \tab 89 \tab 181 \tab 334 \tab 285 \cr
#' }
#'
#' Multiple correlation coefficient: 0.88
#'
#'
#' Standard deviation about the function, Sf : 0.599
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
#'
#' @examples
Eko_PM_basal_area_5_year_increment_southern_Sweden_Birch <- function(
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
    -0.617367*ba_quotient_chronic_mortality+
    -0.350920*ba_quotient_acute_mortality+
    -0.134245E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.277904*fertilised+
    +0.154562*ground_veg_indicator+
    +0.554711E-01*TAX77




  if(SIdm<220){

    if(thinned==0){
      dependent_vars <-
        -0.850224E-02*basal_area.m2+
        +0.931518*log(basal_area.m2)+
        -0.874696E-04*stem_number_ha+
        +0.124964*log(stem_number_ha)+
        -0.890226E-02*age_at_breast_height+
        -0.498825*log(age_at_breast_height)+
        -0.493910E-02*basal_area_other_species+
        -0.135041


    } else {
      dependent_vars <-
        +0.144427*basal_area.m2+
        +0.332109*log(basal_area.m2)+
        -0.457988E-03*stem_number_ha+
        +0.474159*log(stem_number_ha)+
        +0.922378E-02*age_at_breast_height+
        -1.50315*log(age_at_breast_height)+
        -0.116043E-01*basal_area_other_species+
        +1.19213
    }



  } else if(SIdm>=220 && SIdm<260){
      if(thinned==0){
        dependent_vars <-

          +0.129783E-01*basal_area.m2+
          +0.688150*log(basal_area.m2)+
          -0.158067E-03*stem_number_ha+
          +0.304149*log(stem_number_ha)+
          +0.411176E-02*age_at_breast_height+
          -0.864501*log(age_at_breast_height)+
          -0.533730E-02*basal_area_other_species+
          -0.135041

      } else {
        dependent_vars <-
          -0.235447E-01*basal_area.m2+
          +0.962877*log(basal_area.m2)+
          +0.103737E-03*stem_number_ha+
          +0.186790*log(stem_number_ha)+
          -0.127109E-02*age_at_breast_height+
          -1.02854*log(age_at_breast_height)+
          -0.849201E-02*basal_area_other_species+
          +1.19213

      }



  } else if(SIdm>=260 && SIdm<300){

    if(thinned==0){
      dependent_vars <-
        -0.110984E-01*basal_area.m2+
        +0.748193*log(basal_area.m2)+
        -0.434390E-04*stem_number_ha+
        +0.270476*log(stem_number_ha)+
        +0.823613E-03*age_at_breast_height+
        -0.718419*log(age_at_breast_height)+
        -0.174522E-01*basal_area_other_species+
        -0.135041


    } else {
      dependent_vars <-
        -0.438786E-03*basal_area.m2+
        +0.818427*log(basal_area.m2)+
        -0.304146E-03*stem_number_ha+
        +0.241055*log(stem_number_ha)+
        +0.106700E-01*age_at_breast_height+
        -1.16385*log(age_at_breast_height)+
        -0.1978220E-01*basal_area_other_species+
        +1.19213

    }

  } else if(SIdm>=300){

    if(thinned==0){
      dependent_vars <-
        -0.204315E-01*basal_area.m2+
        +0.792798*log(basal_area.m2)+
        -0.179026E-03*stem_number_ha+
        +0.316913*log(stem_number_ha)+
        +0.262117E-02*age_at_breast_height+
        -0.791796*log(age_at_breast_height)+
        -0.146037E-01*basal_area_other_species+
        -0.135041


    } else {
      dependent_vars <-
        +0.255898E-02*basal_area.m2+
        +0.730671*log(basal_area.m2)+
        +0.256307E-04*stem_number_ha+
        +0.256131*log(stem_number_ha)+
        +0.126785E-01*age_at_breast_height+
        -1.24005*log(age_at_breast_height)+
        -0.341768E-02*basal_area_other_species+
        +1.19213

    }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.1590
    )
  )



}
