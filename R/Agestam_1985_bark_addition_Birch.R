#' Bark addition function for Birch from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 20
#'
#' Multiple correlation coefficient: 0.966
#'
#' Standard deviation about the function (sf) : 0.271
#'
#' sf/standard deviation about the mean:  28\%
#'
#'
#' @param basal_area_under_bark_Birch Basal Area Birch under bark, m2/ha
#' @param age_at_breast_height Age at breast height for Pine stems.
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height, Birch, m2/ha.
#' @export
#'
#' @examples
Agestam_1985_bark_addition_Birch <- function(
  basal_area_under_bark_Birch,
  age_at_breast_height,
  latitude
){
  return(
    exp(
      +19.276+
      +0.818*log(basal_area_under_bark_Birch)+
      +0.269*log(age_at_breast_height)+
      -5.25*log(latitude)
    )
  )
}
