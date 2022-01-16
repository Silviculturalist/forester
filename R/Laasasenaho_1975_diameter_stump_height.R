#' Laasasenaho 1975 diameter at stump height
#'
#' @source Laasasenaho, J. 1975. Runkopuun saannon riippuvuus kannon korkeudesta
#'  ja latvan katkaisuläpimitasta. Summary: Dependence of the amount of
#'  harvestable timber upon the stump height and the top-logging diameter.
#'  Folia Forestalia 233. 20 p.
#'
#' @param diameter_cm Diameter of the tree (cm) at breast height, 1.3 m.
#'
#' @return Diameter at Stump height (cm)
#' @export

Laasasenaho_1975_diameter_stump_height <- function(
  diameter_cm
){
  return(
    2 + 1.25*diameter_cm
  )
}
