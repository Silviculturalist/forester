#' Double bark thickness in southern Sweden for Scots Pine from Söderberg 1986.
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
#' *Stockholm
#' *Södermanland
#' *Uppsala
#' *Östergötland
#' *Kalmar
#' *Västmanland
#' *Blekinge
#' *Kristianstad
#' *Malmöhus
#' *Västra Götaland
#' *Halland
#' *Gotland
#'
#' @details Multiple correlation coefficient R = 0.83
#'
#' Spread about the function sf = 0.24
#'
#' sf/Spread about the mean = 0.49
#'
#' Number of observations = 4833
#'
#'
#' @param diameter_cm_under_bark Diameter under bark of tree, in cm.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
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
#' @param continental TRUE, if the plot is situated in a continental climatic region. cf. Ångström 1958. e.g. [forester::local_climate_Sweden()]. Otherwise FALSE.
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
  vegetation,
  continental,
  SI100
){

  dry <- ifelse(soil_moisture==1,1,0)

  herb <- ifelse(vegetation<7,1,0)


  B1 <- ifelse(SI100>=141 & SI100 <=220,1,0)

  B2 <- ifelse(SI100>=221,1,0)

  return(
    exp(
      +0.84207*10^(-2)*(diameter_cm_under_bark*10)+
        -0.10834*10^(-4)*((diameter_cm_under_bark*10)^2)+
        +0.62648*10^(-2)*age_at_breast_height+
        -0.25723*10^(-4)*(age_at_breast_height^2)+
        +5.22630*latitude+
        -0.44767*10^(-1)*(latitude^2)+
        +0.21438*10^(-1)*altitude+
        -0.37426*10^(-3)*latitude*altitude+
        -0.20067*10^(-1)*dry+
        +0.15725*10^(-1)*herb+
        -0.30724*10^(-1)*continental+
        -0.37919*10^(-1)*B1+
        -0.59201*10^(-1)*B2+
        -150.42
    )
  )
}
