#' Bark subtraction function for Spruce from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 119
#'
#' Multiple correlation coefficient: 0.954
#'
#' Standard deviation about the function (sf) : 0.138
#'
#' sf/standard deviation about the mean:  30\%
#'
#'
#' @param basal_area_above_bark_Spruce Basal area including bark m2/ha.
#' @param SI_Spruce SI H100 Spruce from Hägglund 1972,1973.
#'
#' @return Basal area bark at breast height, m2/ha.
#' @export
Agestam_1985_bark_subtraction_Spruce <- function(
  basal_area_above_bark_Spruce,
  SI_Spruce
){
  return(
    exp(
      0.808+
      +0.956*log(basal_area_above_bark_Spruce)+
      -0.495*log(SI_Spruce)
    )
  )
}
