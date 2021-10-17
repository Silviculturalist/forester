#' Double bark thickness in northern Sweden for Scots Pine from Söderberg 1986.
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
#' @details Multiple correlation coefficient R = 0.87
#'
#' Spread about the function sf = 0.22
#'
#' sf/Spread about the mean = 0.42
#'
#' Number of observations = 2316
#'
#'
#' @param diameter_cm_under_bark Diameter under bark of tree, in cm.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance()]
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
Soderberg_1986_double_bark_northern_Sweden_Pine <- function(
  diameter_cm_under_bark,
  age_at_breast_height,
  latitude,
  altitude,
  soil_moisture,
  distance_to_coast_km,
  vegetation,
  continental,
  SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  herb <- ifelse(vegetation<7,1,0)

  dry <- ifelse(soil_moisture==1,1,0)

  B1 <- ifelse(SI100>=141 & SI100 <=220,1,0)

  B2 <- ifelse(SI100>=221,1,0)

  return(
    exp(
      +0.10562*10^(-1)*(diameter_cm_under_bark*10)+
      -0.13895*10^(-4)*((diameter_cm_under_bark*10)^2)+
      +0.21565*10^(-2)*age_at_breast_height+
      -0.54700*10^(-5)*(age_at_breast_height^2)+
      -0.30599*10^(-1)*latitude+
      -0.57487*10^(-3)*altitude+
      +0.73755*10^(-5)*latitude*altitude+
      -0.47475*10^(-1)*dry+
      +0.61779*10^(-1)*far_from_coast+
      +0.59693*10^(-1)*herb+
      -0.44162*10^(-1)*B1+
      -0.57575*10^(-1)*B2+
      +3.48671
    )
  )
}
