#' Dry weight of stump for Spruce from Marklund 1988
#'
#' @param diameter.cm Diameter of tree at breast height, cm.
#' @param dominant_species Species for which SI was estimated. "Pinus sylvestris"
#' or "Picea abies".
#' @param SI Site index, m.
#' @param latitude Latitude, RT90.
#'
#' @return Dry weight stump for Spruce, in kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_stump_Spruce <- function(
  diameter.cm,
  dominant_species,
  SI,
  latitude
){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }



  #G26
  try(
dry_weight_of_stump <-
  -3.3645+
  +10.6686*(diameter.cm/(diameter.cm+17))
)

#G27
  try(
dry_weight_of_stump <-
  -0.8963+
  +10.6925*(diameter.cm/(diameter.cm+17))+
  -0.0196*SI*pinus+
  -0.0188*SI*picea+
  -0.0305*latitude
)

return(exp(dry_weight_of_stump))


}
