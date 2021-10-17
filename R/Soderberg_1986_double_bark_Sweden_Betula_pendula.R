#' Double bark thickness in Sweden for Betula pendula from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 248.
#'
#' @details Multiple correlation coefficient R = 0.83
#'
#' Spread about the function sf = 0.32
#'
#' sf/Spread about the mean = 0.47
#'
#' Number of observations = 846
#'
#' NB: \emphasis{Betula pendula} is in the original source referred as \emphasis{Betula verrucosa}
#'
#'
#' @param diameter_cm_under_bark Diameter under bark of tree, in cm.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param vegetation Vegetation type according to follows Swedish National forest inventory FALTSKIKT:
#' \tabular{ll}{
#' Code \tab Vegetation \cr
#' 1 \tab  Rich-herb without shrubs \cr
#' 2 \tab Rich-herb with shrubs/bilberry \cr
#' 3 \tab Rich-herb with shrubs/lingonberry \cr
#' 4 \tab Low-herb without shrubs \cr
#' 5 \tab Low-herb with shrubs/bilberry \cr
#' 6 \tab Low-herb with shrubs/lingonberry \cr
#' 7 \tab No field layer \cr
#' 8 \tab Broadleaved grass \cr
#' 9 \tab Thinleaved grass \cr
#' 10 \tab Sedge, high \cr
#' 11 \tab Sedge, low \cr
#' 12 \tab Horsetail, Equisetum ssp. \cr
#' 13 \tab Bilberry \cr
#' 14 \tab Lingonberry \cr
#' 15 \tab Crowberry \cr
#' 16 \tab Poor shrub \cr
#' 17 \tab Lichen, frequent occurrence \cr
#' 18 \tab Lichen, dominating \cr
#' }
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance()]
#' @param SI100 Site Index H100, m.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1986_double_bark_Sweden_Betula_pendula <- function(
  diameter_cm_under_bark,
  age_at_breast_height,
  latitude,
  vegetation,
  soil_moisture,
  SI100,
  distance_to_coast_km
){


  dry <- ifelse(soil_moisture==1,1,0)
  moist <- ifelse(soil_moisture>3,1,0)

  herb <- ifelse(vegetation<7,1,0)

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0 )

  B3 <- ifelse(SI100>=261,1,0)


  return(
    exp(
      +0.12948*10^(-1)*(diameter_cm_under_bark*10)+
        -0.15211*10^(-4)*((diameter_cm_under_bark*10)^2)+
        +0.56088*10^(-2)*age_at_breast_height+
        -0.17412*10^(-4)*(age_at_breast_height^2)+
        -0.42668*10^(-1)*latitude+
        -0.10724*herb+
        +0.57244*dry+
        -0.43211*moist+
        +0.12738*far_from_coast+
        -0.88281*10^(-1)*B3+
        +3.4646
    )
  )
}
