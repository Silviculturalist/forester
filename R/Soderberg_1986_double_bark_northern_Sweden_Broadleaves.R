#' Double bark thickness in northern Sweden for Broadleaves from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 250.
#'
#' @description
#' \strong{Applicable counties:}
#'
#' *Norrbottens Lappmark
#' *Norrbottens kustland
#' *Västerbottens lappmark
#' *Västerbottens kustland
#' *Västernorrland - Ångermanlands landskap
#' *Västernorrland - Medelpads landskap
#' *Jämtland - Jämtlands landskap
#' *Jämtland - Härjedalens landskap
#' *Kopparberg - Sälen-Idre.
#'
#' @details Multiple correlation coefficient R = 0.84
#'
#' Spread about the function sf = 0.29
#'
#' sf/Spread about the mean = 0.52
#'
#' Number of observations = 244
#'
#'
#' @param diameter_cm_under_bark Diameter under bark of tree, in cm.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance()]
#' @param SI100 Site Index H100, m.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1986_double_bark_northern_Sweden_Broadleaves <- function(
  diameter_cm_under_bark,
  age_at_breast_height,
  latitude,
  altitude,
  distance_to_coast_km,
  SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  B2 <- ifelse(SI100>=22.1,1,0)

  return(
    exp(
      +0.59990*10^(-2)*(diameter_cm_under_bark*10)+
        +0.13983*10^(-1)*age_at_breast_height+
        -0.38181*10^(-4)*(age_at_breast_height^2)+
        +0.24932*10^(-1)*latitude+
        +0.24780*10^(-2)*altitude+
        -0.74190*10^(-1)*(altitude^(1/2))+
        -0.18749*far_from_coast+
        +0.16729*B2+
        +0.49494*10^(-1)
    )
  )
}
