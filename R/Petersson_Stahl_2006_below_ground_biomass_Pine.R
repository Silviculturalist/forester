#' Below-ground biomass for Pine from Petersson, H. & Stahl. G (2006)
#'
#' @source Petersson, H., Ståhl, G. (2006) "Functions for below-ground biomass of
#' Pinus sylvestris, Picea abies, Betula pendula and Betula pubescens in Sweden.
#' Scandinavian Journal of Forest Research. Vol. 21. pp. 84-93. DOI:
#' <https://doi.org/10.1080/14004080500486864>
#'
#' @description n=328
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, m.
#' @param age_at_breast_height Age at breast height.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param height.m.basal_area_weighted Basal area weighted height of stand.
#' @param age_basal_area_weighted Basal area weighted age of stand.
#' @param basal_area Basal area of the stand, m2 per hectare.
#' @param altitude Meters above sea level.
#' @param Dry_soil If Dry or Mesic : 1, otherwise 0.
#' @param Root_detail 2 or 5 mm resolution of smallest included roots.
#'
#' @return
#' @export
Petersson_Stahl_2006_below_ground_biomass_Pine <- function(
  diameter.cm,
  age_at_breast_height,
  height.m,
  crown_base_height.m,
  height.m.basal_area_weighted,
  age_basal_area_weighted,
  basal_area,
  altitude,
  Dry_soil,
  Root_detail

){
  diameter.mm <- diameter.cm*10
  height.dm <- height.m*10
  crown_base_height.dm <- crown_base_height.m*10
  crown_length <- height.dm-crown_base_height.dm

  if(Root_detail==5){
    try(
      biomass <-
      +3.39014+
      +11.06822*((diameter.mm)/(diameter.mm+113))
    )

    try(
      biomass <-
      +3.57249+
      +11.07427*((diameter.mm)/(diameter.mm+113))+
      -0.05119*diameter.mm/age_at_breast_height
    )

    try(
      biomass <-
      +3.50127+
      +10.96210*((diameter.mm)/(diameter.mm+113))+
      +0.00250*age_basal_area_weighted+
      -0.37595*Dry_soil
    )


  } else if(Root_detail==2){

    try(
      biomass <-
      +3.44275+
      +11.06537*((diameter.mm)/(diameter.mm+113))
    )

    try(
      biomass <-
      +3.62193+
      +11.07117*((diameter.mm)/(diameter.mm+113))+
      -0.05029*diameter.mm/age_at_breast_height
    )

    try(
      biomass <-
      +3.56553+
      +10.96370*((diameter.mm)/(diameter.mm+113))+
      +0.00236*age_basal_area_weighted+
      -0.38089*Dry_soil

    )



  }

  return(exp(biomass))



}
