#' Mortality for Pine per annum according to Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 50.
#'
#' @details Material:
#'
#' Number of plots: 134
#'
#' Multiple correlation coefficient: 0.400
#'
#' Standard deviation about the function (sf) : 1.99
#'
#' sf/standard deviation about the mean:  93\%
#'
#' @param stems_Pine_ha Number of Pine stems per hectare.
#' @param basal_area_above_bark_all_species Basal area above bark for all tree species. m2/ha.
#' @param SI_Pine Site Index H100 for Pine according to Hägglund (1974), meters.
#' @param age_at_breast_height Age at breast height of Pine stems.
#'
#' @return m2/ha mortality above bark per annum.
#' @export
#'
#' @examples
Agestam_1985_mortality_above_bark_Pine <- function(
  stems_Pine_ha,
  basal_area_above_bark_all_species,
  SI_Pine,
  age_at_breast_height
){
  return(
    exp(
      16.039+
      +0.4809*log(stems_Pine_ha)+
      +2.2526*log(basal_area_above_bark_all_species)+
      -3.5439*log(SI_Pine*10)+
      -2.6191*log(age_at_breast_height)
    )
  )
}
