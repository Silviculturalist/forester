#' Simple Volume function for Norway Spruce in Norway
#'
#' @source Vestjordet, E. 1967. Functions and tables for volume of standing
#' trees. Norway spruce. Reports of the Norwegian Forest Research Institute,
#' 22, 539-574.
#'
#' @param diameter_cm Diameter of tree in cm at breast height, 1.3 m.
#' @param height_m Total tree height, m.
#' @param above_bark TRUE (default) or FALSE.
#'
#' @return Volume of tree in dm^3.
#' @export

Vestjordet_1967_volume_Norway_Norway_Spruce <- function(
    diameter_cm,
    height_m,
    above_bark=TRUE){
  stopifnot(above_bark%in%c(TRUE,FALSE,1,0))

  # VOLUM MED BARK I DM3. D <= 10CM ER VGB1, 10CM < D <= 13CM ER VGB2 OG D > 13CM ER VGB3
  if(aboveBark){
    v<-		ifelse (diameter_cm <= 10, {VGB1 = 0.52 + 0.02403 * diameter_cm * diameter_cm * height_m + 0.01463 * diameter_cm * height_m * height_m - 0.10983 * height_m * height_m + 0.15195 * diameter_cm * height_m},
                 ifelse (diameter_cm > 10 & diameter_cm <= 13, {VGB2 = -31.57 + 0.0016 * diameter_cm * height_m * height_m + 0.0186 * height_m * height_m + 0.63 * diameter_cm * height_m - 2.34 * height_m + 3.2 * diameter_cm},
                         ifelse (diameter_cm > 13, {VGB3 = 10.14 + 0.0124 * diameter_cm * diameter_cm * height_m + 0.03117 * diameter_cm * height_m * height_m - 0.36381 * height_m * height_m + 0.28578 * diameter_cm * height_m}, NA)))
  }


  # VOLUM UNDER BARK I DM3. D <= 10CM ER VGU1, 10CM < D <= 13CM ER VGU2 OG D > 13CM ER VGU3.
  if(aboveBark == FALSE){
    v<-
      ifelse (diameter_cm <= 10, {VGU1 = 0.38 + 0.02524 * diameter_cm * diameter_cm * height_m + 0.01269 * diameter_cm * height_m * height_m - 0.07726 * height_m * height_m + 0.11671 * diameter_cm * height_m},
              ifelse (diameter_cm > 10 & diameter_cm <= 13, {VGU2 = -27.19 + 0.0073 * diameter_cm * height_m * height_m - 0.0228 * height_m * height_m + 0.5667 * diameter_cm * height_m - 1.98 * height_m + 2.75 * diameter_cm},
                      ifelse (diameter_cm > 13, {VGU3 = 8.66 + 0.01218 * diameter_cm * diameter_cm * height_m + 0.02976 * diameter_cm * height_m * height_m - 0.31373 * height_m * height_m + 0.25452 * diameter_cm * height_m},NA)))
  }

  return(v)

}
