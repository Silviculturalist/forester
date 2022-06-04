#' Mortality for Spruce per annum according to Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 50.
#'
#' @details Material:
#'
#' Number of plots: 134
#'
#' Multiple correlation coefficient: 0.447
#'
#' Standard deviation about the function (sf) : 1.74
#'
#' sf/standard deviation about the mean:  90\%
#'
#' @param stems_ha Number of Spruce stems per hectare.
#' @param basal_area_above_bark_all_species Total basal area including bark, m2/ha.
#'
#' @return m2/ha mortality above bark per annum.
#' @export

Agestam_1985_mortality_above_bark_Spruce <- function(
  stems_ha,
  basal_area_above_bark_all_species
){
  return(
    exp(
      -16.996+
      +1.4344*log(stems_ha)+
      +0.9816*log(basal_area_above_bark_all_species)
    )
  )
}
