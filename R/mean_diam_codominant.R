#' Mean diameter of co-dominant trees
#'
#' @description Presented in Elfving, B. (2003) Ålderstilldelning till enskilda träd i skogliga tillväxtprognoser. SLU, inst. f. skogsskötsel. Arbetsrapport 182.
#'
#' @param total_stand_age total stand age
#' @param SIS site index according site factors, given as top height at age 100, m.
#' @param total_ba As estimated with relascope in field. m^2 / ha
#'
#' @return
#'
#'
mean_diam_codominant <- function(total_stand_age, SIS, total_ba){
  lnd <- -0.9231 + 1.0032*log(total_stand_age) - 0.00701*total_stand_age - 4.005/SIS + 0.0186*SIS - 1.882/total_ba + 0.036

  return(lnd)
}
