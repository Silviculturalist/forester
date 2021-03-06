#' Basal area growth function for Birch in northern and central Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 69
#'
#' @description
#' Number of observations: 4429
#'
#' \tabular{lrrrr}{
#' Thinning \tab SI<140 \tab 140≤SI<180 \tab 180≤SI<220 \tab SI>220   \cr
#' Unthinned \tab 859 \tab 1157 \tab 792 \tab 482  \cr
#' Thinned \tab 243 \tab 378 \tab 269 \tab 249 \cr
#' }
#'
#' Multiple correlation coefficient: 0.87
#'
#'
#' Standard deviation about the function, Sf : 0.607
#'
#'
#' Sf/ Standard deviation about the mean, (\%): 50
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
Eko_PM_1985_basal_area_5_year_increment_northern_central_Sweden_Birch <- function(
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
    -0.474848*ba_quotient_chronic_mortality+
    -0.207333*ba_quotient_acute_mortality+
    -0.202362E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.914442E-01*thinned_previous_5_years
    +0.176843*fertilised+
    +0.256714*ground_veg_indicator+
    -0.488706E-01*wet+
    -0.139928E-01*latitude+
    -0.462992*altitude+
    +0.189383*TAX77




  if(SIdm<140){

    if(thinned==0){
      dependent_vars <-
        +0.281210E-02*basal_area.m2+
        +0.718062*log(basal_area.m2)+
        -0.264120E-03*stem_number_ha+
        +0.360947*log(stem_number_ha)+
        -0.513560*log(age_at_breast_height)+
        -0.146581E-01*basal_area_other_species+
        -0.768510


    } else {
      dependent_vars <-
        +0.856585-01*basal_area.m2+
        +0.488507*log(basal_area.m2)+
        -0.549010E-03*stem_number_ha+
        +0.467588*log(stem_number_ha)+
        -0.618645*log(age_at_breast_height)+
        -0.477226E-02*basal_area_other_species+
        -0.768510
    }



  } else if(SIdm>=140 && SIdm<180){

      if(thinned==0){
        dependent_vars <-

        +0.831133E-02*basal_area.m2+
          +0.660201*log(basal_area.m2)+
          -0.161770E-03*stem_number_ha+
          +0.361272*log(stem_number_ha)+
          -0.609806*log(age_at_breast_height)+
          -0.133204E-01*basal_area_other_species+
          -0.355882


      } else {
        dependent_vars <-
          +0.665931E-02*basal_area.m2+
          +0.700295*log(basal_area.m2)+
          -0.221485E-03*stem_number_ha+
          +0.316196*log(stem_number_ha)+
          -0.489888*log(age_at_breast_height)+
          -0.246752E-01*basal_area_other_species+
          -0.355882

      }



  } else if(SIdm>=180 && SIdm<220){

    if(thinned==0){
      dependent_vars <-
        -0.371203E-02*basal_area.m2+
        +0.835899*log(basal_area.m2)+
        -0.141238E-03*stem_number_ha+
        +0.221611*log(stem_number_ha)+
        -0.732659*log(age_at_breast_height)+
        -0.131446E-01*basal_area_other_species+
        +0.891049


    } else {
      dependent_vars <-
        -0.134251E-02*basal_area.m2+
        +0.838751*log(basal_area.m2)+
        -0.237653E-03*stem_number_ha+
        +0.192259*log(stem_number_ha)+
        -0.707746*log(age_at_breast_height)+
        -0.499067E-02*basal_area_other_species+
        +0.891049

    }

    } else if(SIdm>=220){

      if(thinned==0){
        dependent_vars <-
          -0.281602E-01*basal_area.m2+
          +0.800357*log(basal_area.m2)+
          +0.673284E-04*stem_number_ha+
          +0.205233*log(stem_number_ha)+
          -0.631139*log(age_at_breast_height)+
          -0.176494E-01*basal_area_other_species+
          +0.731245


      } else {
        dependent_vars <-
          -0.177526E-01*basal_area.m2+
          +0.814686*log(basal_area.m2)+
          +0.781625E-04*stem_number_ha+
          +0.183532*log(stem_number_ha)+
          -0.593656*log(age_at_breast_height)+
          -0.211444E-01*basal_area_other_species+
          +0.731245

      }



  }

  return(
    exp(
      dependent_vars + independent_vars + 0.1642
    )
  )



}
