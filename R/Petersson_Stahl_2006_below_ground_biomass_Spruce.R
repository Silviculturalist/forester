#' Below-ground biomass for Spruce from Petersson, H. & Stahl. G (2006)
#'
#' @source Petersson, H., Ståhl, G. (2006) "Functions for below-ground biomass of
#' Pinus sylvestris, Picea abies, Betula pendula and Betula pubescens in Sweden.
#' Scandinavian Journal of Forest Research. Vol. 21. pp. 84-93. DOI:
#' <https://doi.org/10.1080/14004080500486864>
#'
#' @description n=339
#'
#' @param diameter.cm Diameter at breast height, in cm.
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
#'
#' @examples
Petersson_Stahl_2006_below_ground_biomass_Spruce <- function(
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
        +4.52965+
        +10.57571*((diameter.mm)/(diameter.mm+142))
    )

    try(
      biomass <-
        +4.60559+
        +10.60542*((diameter.mm)/(diameter.mm+142))+
        -0.02489*diameter.mm/age_at_breast_height
    )

    try(
      biomass <-
        +4.98414+
        +9.89245*((diameter.mm)/(diameter.mm+142))+
        -0.03411*(diameter.mm/age_at_breast_height)+
        -0.00769*basal_area+
        +0.00317*crown_length+
        -0.23375*Dry_soil
    )


  } else if(Root_detail==2){

    try(
      biomass <-
        +4.58761+
        +10.44035*((diameter.mm)/(diameter.mm+138))
    )

    try(
      biomass <-
        +4.69287+
        +10.45700*((diameter.mm)/(diameter.mm+138))+
        -0.03057*diameter.mm/age_at_breast_height
    )

    try(
      biomass <-
        +5.00171+
        +9.89713*((diameter.mm)/(diameter.mm+138))+
        -0.03653*age_basal_area_weighted+
        -0.00636*basal_area+
        +0.00261*crown_length+
        -0.21705*Dry_soil

    )



  }

  return(exp(biomass))



}
