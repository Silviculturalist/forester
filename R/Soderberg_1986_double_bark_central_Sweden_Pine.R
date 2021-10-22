#' Double bark thickness in central Sweden for Scots Pine from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 246.
#'
#' @description
#' \strong{Applicable counties:}
#'
#' *Kopparberg - övriga
#' *Gävleborg - Hälsinglands landskap
#' *Gävleborg - övriga
#' *Kopparberg - övriga
#' *Värmland
#'
#' @details Multiple correlation coefficient R = 0.85
#'
#' Spread about the function sf = 0.23
#'
#' sf/Spread about the mean = 0.46
#'
#' Number of observations = 2014
#'
#'
#' @param diameter_cm_under_bark Diameter under bark of tree, in cm.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance()]
#' @param SI100 Site Index H100, m.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1986_double_bark_central_Sweden_Pine <- function(
  diameter_cm_under_bark,
  age_at_breast_height,
  latitude,
  altitude,
  soil_moisture,
  distance_to_coast_km,
  SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  dry <- ifelse(soil_moisture==1,1,0)

  B1 <- ifelse(SI100>=14.1 & SI100 <=22.0,1,0)

  B2 <- ifelse(SI100>=22.1,1,0)

  return(
    exp(
      +0.99156*10^(-2)*(diameter_cm_under_bark*10)+
        -0.13367*10^(-4)*((diameter_cm_under_bark*10)^2)+
        +0.33766*10^(-2)*age_at_breast_height+
        -0.13367*10^(-4)*(age_at_breast_height^2)+
        -0.56911*10^(-1)*latitude+
        +0.15138*10^(-2)*altitude+
        -0.29419*10^(-4)*latitude*altitude+
        -0.23967*10^(-1)*dry+
        +0.11524*far_from_coast+
        -0.11249*10^(-1)*B1+
        -0.55964*10^(-1)*B2+
        +5.1825
    )
  )
}
