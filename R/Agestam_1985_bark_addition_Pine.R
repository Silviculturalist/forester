#' Bark addition function for Pine from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 143
#'
#' Multiple correlation coefficient: 0.968
#'
#' Standard deviation about the function (sf) : 0.112
#'
#' sf/standard deviation about the mean:  26\%
#'
#'
#'
#' @param basal_area_under_bark_Pine Basal area Pine under bark, m2/ha.
#' @param SI_Pine Site Index H100 for Scots Pine from Hägglund (1974)
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height for Pine, m2/ha.
#' @export
#'
#' @examples
Agestam_1985_bark_addition_Pine <- function(
  basal_area_under_bark_Pine,
  SI_Pine,
  latitude
){
  return(
    exp(
      15.98+
      +1.013*log(basal_area_under_bark_Pine)+
      -0.197*log(SI_Pine)+
      -3.928*log(latitude)
    )
  )
}
