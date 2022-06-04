#' Below-ground biomass for Birch from Petersson, H. & Stahl. G (2006)
#'
#' @details Betula pendula & Betula pubescens.
#'
#' @description n=13
#'
#' @source Petersson, H., Ståhl, G. (2006) "Functions for below-ground biomass of
#' Pinus sylvestris, Picea abies, Betula pendula and Betula pubescens in Sweden.
#' Scandinavian Journal of Forest Research. Vol. 21. pp. 84-93. DOI:
#' <https://doi.org/10.1080/14004080500486864>
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param Root_detail 2 or 5 mm resolution of smallest included roots.
#'
#' @return
#' @export
Petersson_Stahl_2006_below_ground_biomass_Birch <- function(
  diameter.cm,
  Root_detail

){
  diameter.mm <- diameter.cm*10

  if(Root_detail==5){
    try(
      biomass <-
        +4.90864+
        +9.91194*(diameter.mm/diameter.mm+138)
    )


  } else if(Root_detail==2){

    try(
      biomass <-
        +6.17080+
        +10.01111*(diameter.mm/diameter.mm+225)
    )

  }

  return(exp(biomass))



}
