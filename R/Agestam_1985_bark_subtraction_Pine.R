#' Bark subtraction function for Pine from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 143
#'
#' Multiple correlation coefficient: 0.982
#'
#' Standard deviation about the function (sf) : 0.085
#'
#' sf/standard deviation about the mean:  19\%
#'
#'
#'
#' @param basal_area_over_bark_Pine Basal area Pine including bark, m2/ha.
#' @param SI_Pine Site Index H100 for Scots Pine from Hägglund (1974)
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height for Pine, m2/ha.
#' @export

Agestam_1985_bark_subtraction_Pine <- function(
  basal_area_over_bark_Pine,
  SI_Pine,
  latitude
){
  return(
    exp(
      11.69+
      +1.025*log(basal_area_over_bark_Pine)+
      -0.151*log(SI_Pine)+
      -3.020*log(latitude)
    )
  )
}
