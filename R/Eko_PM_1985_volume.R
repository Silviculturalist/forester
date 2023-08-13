#' Volume function for for even-aged monocultures or mixed stands in Sweden.
#'
#' @source Ekö, Per Magnus (1985) "En produktionsmodell för skog i Sverige, baserad på bestånd från
#' riksskogstaxeringens provytor: A growth simulator for Swedish forests, based on
#' data from the national forest survey. Rapporter nr. 16. Swedish University of
#' Agricultural Sciences, dept. of Silviculture. Umeå. ISBN 91-576-2386-4. ISSN
#'  0348-8969.  p. 105
#'
#'  @description
#'  \strong{Pine, northern Sweden}
#'  Multiple correlation coefficient: 0.98
#'
#'  Standard deviation about the function: 0.23
#'
#'  Sf/Standard deviation about the mean: 0.17
#'  \strong{Pine, central Sweden}
#'  Multiple correlation coefficient: 0.98
#'
#'  Standard deviation about the function: 0.23
#'
#'  Sf/standard deviation about the mean: 0.17
#'
#'  \strong{Pine, southern Sweden}
#'  Multiple correlation coefficient: 0.98
#'
#'  Standard deviation about the function: 0.23
#'
#'  Sf/standard deviation about the mean: 0.18
#'
#'  \strong{Norway Spruce, northern Sweden}
#'  Multiple correlation coefficient: 0.98
#'
#'  Standard deviation about the function: 0.29
#'
#'  Sf/standard deviation about the mean: 0.20
#'  \strong{Norway Spruce, central Sweden}
#'  Multiple correlation coefficient: 0.98
#'
#'  Standard deviation about the function: 0.29
#'
#'  Sf/standard deviation about the mean: 0.20
#'
#'  \strong{Norway Spruce, southern Sweden}
#'  Multiple correlation coefficient: 0.99
#'
#'  Standard deviation about the function: 0.25
#'
#'  Sf/standard deviation about the mean: 0.17
#'
#'  \strong{Birch, northern and central Sweden}
#'  Multiple correlation coefficient: 0.95
#'
#'  Standard deviation about the function: 0.39
#'
#'  Sf/Standard deviation about the mean: 0.32
#'
#'  \strong{Birch, southern Sweden}
#'  Multiple correlation coefficient: 0.96
#'
#'  Standard deviation about the function: 0.35
#'
#'  Sf/Standard deviation about the mean: 0.26
#'
#'  \strong{Broadleaves, northern and central Sweden}
#'  Multiple correlation coefficient: 0.94
#'
#'  Standard deviation about the function: 0.41
#'
#'  Sf/Standard deviation about the mean: 0.33
#'
#'  \strong{Broadleaves, southern Sweden}
#'  Multiple correlation coefficient: 0.97
#'
#'  Standard deviation about the function: 0.37
#'
#'  Sf/Standard deviation about the mean: 0.26
#'
#'  \strong{Beech}
#'  Multiple correlation coefficient: 0.99
#'
#'  Standard deviation about the function: 0.28
#'
#'  Sf/Standard deviation about the mean: 0.17
#'
#'
#'  \strong{Oak}
#'  Multiple correlation coefficient: 0.97
#'
#'  Standard deviation about the function: 0.39
#'
#'  Sf/Standard deviation about the mean: 0.26
#'
#' @param basal_area.m2 Basal area over bark of Pine (m2/ha)
#' @param basal_area_other_species Basal area of other species on the plot. (m2/ha)
#' @param age_at_breast_height Age at breast height, calculated as the mean age of the two thickest trees (years).
#' @param stem_number_ha Number of stems per hectare.
#' @param SI Site index H100 Pine , m.
#' @param thinned 1 if the stand has been thinned before, otherwise 0.
#' @param thinned_previous_five_years 1 if the stand has been thinned during the last five years, otherwise 0.
#' @param thinned_before_previous_five_years 1 if the stand has been thinned before the last five years, otherwise 0.
#' @param basal_area_weighted_mean_diameter Diameter corresponding to mean basal area (m).
#' @param basal_area_weighted_mean_diameter_other_species Diameter corresponding to mean basal area (m) of other species on plot.
#' @param latitude Degrees.
#' @param altitude Meters above sea level.
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
#' @param fertilised 1 if the stand has been fertilised, otherwise 0.
#'
#' @return m3sk.
#' @export
#' @name PM_Vol

Eko_PM_1985_volume_central_Sweden_Pine <- function(
  basal_area.m2,
  basal_area_other_species,
  age_at_breast_height,
  stem_number_ha,
  SI,
  thinned,
  basal_area_weighted_mean_diameter,
  basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.06
  b2 <-  -2.2

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +0.778157E-02*basal_area.m2+
    +1.14159*log(basal_area.m2)
    +0.927460*F4age+
    -0.166730*log(stem_number_ha)+
    +0.304900*log(SIdm)+
    +0.270200E-01*thinned+
    +0.292836E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.910330


  return(exp(
    lnVolume + 0.0273
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_northern_Sweden_Pine <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    thinned_previous_five_years,
    thinned_before_previous_five_years,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
    ){
      SIdm <- 10*SI

      b1 <-  -0.06
      b2 <-  -2.3

      F4age <- (1 - exp(b1*age_at_breast_height))
      F4basal_area <- (1- exp(b2*basal_area.m2))

      lnVolume <-
        +1.24296*log(basal_area.m2)+
        -0.472530*F4basal_area+
        +1.05864*F4age+
        -0.170140*log(stem_number_ha)+
        +0.247550*log(SIdm)+
        +0.213800E-01*thinned_before_previous_five_years+
        +0.295300E-01*thinned_previous_five_years+
        +0.510332E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
        +1.08339


      return(exp(
        lnVolume + 0.0275
      ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_southern_Sweden_Pine <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    thinned,
    vegetation,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.075
  b2 <-  -2.2

  ground_veg_indicator <- if(vegetation%in%c(1:6,8,9)){
    1
  } else if(vegetation%in%c(1:9) && south_sweden==TRUE){
    1
  } else {
    0
  }

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.21272*log(basal_area.m2)+
    -0.299900*F4basal_area+
    +1.01970*F4age+
    -0.172300*log(stem_number_ha)+
    +0.369930*log(SIdm)+
    +1.65136*log(latitutde)+
    +0.349200E-01*log(altitude)+
    -0.197100E-01*ground_veg_indicator+
    +0.229100E-01*thinned+
    +0.526017E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    -6.46337


  return(exp(
    lnVolume + 0.0260
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_northern_Sweden_Spruce <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.065
  b2 <-  -2.05

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +0.362521E-02*basal_area.m2+
    +1.35682*log(basal_area.m2)+
    -1.47258*basal_area_weighted_mean_diameter+
    -0.438770*F4basal_area+
    +1.46910*F4age+
    -0.314730*log(stem_number_ha)+
    +0.228700*log(SIdm)+
    +0.118700E-01*thinned+
    +0.254896E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +1.970094




  return(exp(
    lnVolume + 0.0388
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_central_Sweden_Spruce <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    vegetation,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.065
  b2 <-  -2.05

  ground_veg_indicator <- if(vegetation%in%c(1:6,8,9)){
    1
  } else if(vegetation%in%c(1:9) && south_sweden==TRUE){
    1
  } else {
    0
  }

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.28359*log(basal_area.m2)+
    -0.380690*F4basal_area+
    +1.21756*F4age+
    -0.216690*log(stem_number_ha)+
    +0.350370*log(SIdm)+
    +0.413000E-01*ground_veg_indicator+
    +0.362100E-01*thinned+
    +0.268645E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.700490




  return(exp(
    lnVolume + 0.0563
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_southern_Sweden_Spruce <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.04
  b2 <-  -2.05


  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.22886*log(basal_area.m2)+
    -0.349820*F4basal_area+
    +0.485170*F4age+
    -0.152050*log(stem_number_ha)+
    +0.337640*log(SIdm)+
    +0.129800E-01*thinned+
    +0.548055E-03*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +0.584600




  return(exp(
    lnVolume + 0.0325
  ))

}

#' @rdname PM_Vol
#' @export
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

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_southern_Sweden_Birch <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    latitude,
    fertilised,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.07
  b2 <-  -2.1

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.786906E-02*basal_area.m2+
    +1.35254*log(basal_area.m2)+
    -1.30862*basal_area_weighted_mean_diameter+
    -0.524630*F4basal_area+
    +1.01779*F4age+
    -0.254630*log(stem_number_ha)+
    +0.204880*log(SIdm)+
    +2.75025*log(latitude)+
    +0.774000E-01*fertilised+
    +0.434800E-01*thinned+
    +0.250449E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    -9.38127


  return(exp(
    lnVolume + 0.0595
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_northern_central_Sweden_Broadleaves <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    latitude,
    altitude,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.04
  b2 <-  -2.3

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    +1.26649*log(basal_area.m2)+
    -0.580030*F4basal_area+
    +0.486310*F4age+
    -0.172050*log(stem_number_ha)+
    +0.174930*log(SIdm)+
    -1.51968*log(latitude)+
    -0.368300E-01*log(altitude)+
    +0.547400E-01*thinned+
    +0.417126E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +7.79034

  return(exp(
    lnVolume + 0.0853
  ))

}


#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_southern_Sweden_Broadleaves <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    latitude,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  total_basal_area <- basal_area.m2+basal_area_other_species

  b1 <-  -0.075
  b2 <-  -2.1

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.148700E-01*basal_area.m2+
    +1.29359*log(basal_area.m2)+
    -0.784820*F4basal_area+
    +1.18741*F4age+
    -0.135830*log(stem_number_ha)+
    +0.219890*log(SIdm)+
    +2.02656*log(latitude)+
    +0.242500E-01*thinned+
    +0.859600E-01*total_basal_area+
    +0.509488E-03*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +7.50102

  return(exp(
    lnVolume + 0.0671
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_Sweden_Beech <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.02
  b2 <-  -2.3

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.111600E-01*basal_area.m2+
    +1.30527*log(basal_area.m2)+
    -0.676190*F4basal_area+
    +0.490740*F4age+
    -0.151930*log(stem_number_ha)+
    -0.572600E-01*log(SIdm)+
    +0.628000E-01*thinned+
    +0.203927E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +2.85509


  return(exp(
    lnVolume + 0.0392
  ))

}

#' @rdname PM_Vol
#' @export
Eko_PM_1985_volume_Sweden_Oak <- function(
    basal_area.m2,
    basal_area_other_species,
    age_at_breast_height,
    stem_number_ha,
    SI,
    thinned,
    basal_area_weighted_mean_diameter,
    basal_area_weighted_mean_diameter_other_species
){
  SIdm <- 10*SI

  b1 <-  -0.055
  b2 <-  -2.3

  F4age <- (1 - exp(b1*age_at_breast_height))
  F4basal_area <- (1- exp(b2*basal_area.m2))

  lnVolume <-
    -0.106300E-01*basal_area.m2+
    +1.27353*log(basal_area.m2)+
    -0.463790*F4basal_area+
    +0.801580*F4age+
    -0.157080*log(stem_number_ha)+
    +0.159030*log(SIdm)+
    +0.503200E-01*thinned+
    +0.188030E-02*(basal_area_weighted_mean_diameter_other_species/basal_area_weighted_mean_diameter)*basal_area_other_species+
    +1.40608


  return(exp(
    lnVolume + 0.0756
  ))

}
