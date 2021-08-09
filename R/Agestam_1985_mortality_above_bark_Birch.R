#' Mortality for Birch per annum according to Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 50.
#'
#' @details Material:
#'
#' Number of plots: 87
#'
#' Multiple correlation coefficient: 0.484
#'
#' Standard deviation about the function (sf) : 1.58
#'
#' sf/standard deviation about the mean:  92\%
#'
#' @param stems_ha_all_species Total number of stems per hectare.
#' @param basal_area_m2_ha_Birch Basal area m2/ha of Birch.
#'
#' @return m2/ha mortality above bark per annum.
#' @export
#'
#' @examples
Agestam_1985_mortality_above_bark_Birch <- function(
  stems_ha_all_species,
  basal_area_m2_ha_Birch
){
  return(
    exp(
      -8.3764+
      +0.4199*log(stems_ha_all_species)+
      +0.3236*log(basal_area_m2_ha_Birch)
    )
  )
}
