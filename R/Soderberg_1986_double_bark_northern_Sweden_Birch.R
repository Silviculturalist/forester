#' Double bark thickness for Birch in northern Sweden from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 249.
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
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been distinguished
#' by Sylvén 1917. The distribution of the different forms of Scots Pine is assumed
#' to depend on the expansion-history of the Pine, according to which the Pine
#' immigrated both from the north and the south with a transitional zone in central
#' Sweden, where both forms are present. For other species, the change in genotype
#' is more continuous over the country (Kiellander 1974). For Norway Spruce, it is
#' assumed that it has had time for 30-50 generations in northern Sweden, contrasted
#' against only 10-20 in southern Sweden (Kiellander 1966). Therefore the same regional
#' division is used for Norway Spruce as for Scots Pine. For the other species, the regional
#' division is limited to two regions, wherein the northern and central regions have been merged.

#'
#' @details Multiple correlation coefficient R = 0.85
#'
#' Spread about the function sf = 0.22
#'
#' sf/Spread about the mean = 0.43
#'
#' Number of observations = 1093
#'
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
#' @param SI100 Site Index H100, m.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1986_double_bark_northern_Sweden_Birch <- function(
  diameter_cm_under_bark,
  age_at_breast_height,
  latitude,
  vegetation,
  soil_moisture,
  SI100,
  distance_to_coast_km
){


  dry <- ifelse(soil_moisture==1,1,0)

  herb <- ifelse(vegetation<7,1,0)

  B1 <- ifelse(SI100>=14.1 & SI100<=18.0,1,0)
  B2 <- ifelse(SI100>=18.1,1,0)


  return(
    exp(
      +0.82582*10^(-2)*(diameter_cm_under_bark*10)+
        -0.75897*10^(-5)*((diameter_cm_under_bark*10)^2)+
        +0.89957*10^(-2)*age_at_breast_height+
        -0.22729*10^(-4)*(age_at_breast_height^2)+
        -0.60489*latitude+
        +0.47242*10^(-2)*(latitude^2)+
        -0.42615*10^(-1)*herb+
        +0.24851*dry+
        -0.75997*10^(-1)*B1+
        -0.13227*B2+
        +20.421
    )
  )
}
