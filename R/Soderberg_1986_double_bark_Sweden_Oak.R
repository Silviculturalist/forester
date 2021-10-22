#' Double bark thickness for Oak in Sweden from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 251.
#'
#' @details Multiple correlation coefficient R = 0.89
#'
#' Spread about the function sf = 0.20
#'
#' sf/Spread about the mean = 0.38
#'
#' Number of observations = 315
#'
#'
#' @param diameter_cm_under_bark Diameter under bark of tree, in cm.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1986_double_bark_Sweden_Oak <- function(
  diameter_cm_under_bark,
  age_at_breast_height,
  latitude
){

  return(
    exp(
      +0.64773*10^(-2)*(diameter_cm_under_bark*10)+
        -0.63950*10^(-5)*((diameter_cm_under_bark*10)^2)+
        +0.95725*10^(-2)*age_at_breast_height+
        -0.54041*10^(-4)*(age_at_breast_height^2)+
        +3.2234*latitude+
        -0.27582*10^(-1)*(latitude^2)+
        -92.368
    )
  )
}
