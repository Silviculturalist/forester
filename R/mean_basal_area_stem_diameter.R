#' Calculate mean diameter corresponding to the mean basal area per stem.
#'
#' @description
#' A simple helper function which calculates the corresponding diameter of the mean basal area stem.
#'
#' \strong{NB} The mean basal area stem should not be confused with the \emph{basal area weighted mean stem}.
#'
#' The mean basal area stem is calculated (?):
#' \deqn{D_g = \frac{BA_{plot}}{Number of stems}}{D_g = Plot Basal Area / Number of stems}
#'
#'
#' The Diameter corresponding to the mean basal area of stand,cm.
#'
#' (Pettersson diss. 1992)
#'
#' \deqn{D_g = \sqrt{\frac{\sum{d^2}}{n}}}{D_g = sqrt( sum(d^2)/n  )}
#'
#'
#' The basal area weighted mean stem is calculated:
#' \deqn{DGV = \frac{\sum{D_i \times BA_i }}{BA_{plot}}}{DGV = ( \sum (Diameter_i * BA_i)) / Plot Basal Area}
#'
#' @param basal_area_m2_ha Basal area at breast height 1.3 m.
#' @param stem_count Number of stems.
#'
#' @return Diameter, cm.
#' @export
#'
#' @examples
mean_basal_area_stem_diameter <- function(basal_area_m2_ha,
                                          stem_count){
  stop("This function not yet verified.")
  return(
    2*sqrt((basal_area_m2_ha/stem_count)/pi)*100
  )

}
