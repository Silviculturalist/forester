#' Stand volume functions for Picea abies in Eastern, Central and Northern
#' Norway.
#' @source Erik Næsset & Bjorn Tveite (1999) Stand Volume Functions for Picea
#' abies in Eastern, Central and Northern Norway, Scandinavian Journal of Forest
#'  Research, 14:2, 164-174, DOI: \href{https://doi.org/10.1080/02827589950152890}{10.1080/02827589950152890}
#'
#'  @param BA_m2_ha Basal Area, m^2 / ha.
#'  @param mean_height_Lorey Lorey's mean height e.g. [forester::Lorey_mean_height()]
#'  @param SIH40 Site Index H40 from Tveite 1977. e.g. [Tveite_1977_height_trajectory_Norway_Norway_Spruce()]
#'
#'
#' @return Stand Volume, m^3 / ha.
#' @details Log scale statistics:
#'  \tabular{lr}{
#' R^2 \tab 0.997 \cr
#' RMSE (m^3ha^-1) \tab 14.30\cr
#' C.V. (%) \tab 5.43 \cr
#' }
#'
#' @export
#' @examples
Naesset_1999_stand_volume_Norway_Norway_Spruce <- function(
    BA_m2_ha,
    mean_height_Lorey,
    SIH40
){
  if(BA_m2_ha<4) warning("Basal area below lower limit (4 m^2 /ha)")
  if(SIH40<6) warning("SIH40 below lower limit (H40 = 6 m)")
  if(mean_height_Lorey<6) warning("Basal area below lower limit (6 m)")
  return(
    exp(log(0.6031) + 0.9905*log(BA_m2_ha) + 0.8407*log(mean_height_Lorey) + 0.0995*log(SIH40))
  )
}

