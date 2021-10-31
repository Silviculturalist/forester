#' Mean diameter of co-dominant trees
#'
#' @description Presented in Elfving, B. (2003) Ålderstilldelning till enskilda träd i skogliga tillväxtprognoser. SLU, inst. f. skogsskötsel. Arbetsrapport 182.
#'
#' @param total_stand_age total stand age
#' @param SIS_100 site index according site factors, given as top height at age 100, m.
#' @param Basal_area_stand OBS. Of the entire stand! As estimated with relascope in field. m^2 / ha
#' @export
#'
#' @return Mean diameter of co-dominants, cm.
#'
#'
Elfving_2003_mean_diameter_codominant_trees <- function(total_stand_age, SIS, Basal_area_plot){
  lnd <- -0.9231 + 1.0032*log(total_stand_age) - 0.00701*total_stand_age - 4.005/SIS + 0.0186*SIS - 1.882/Basal_area_stand + 0.036

  return(exp(lnd))
}
