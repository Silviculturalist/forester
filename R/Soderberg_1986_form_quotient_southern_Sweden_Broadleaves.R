#' Form quotient for other Broadleaves, southern Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 226.
#'
#' @description
#'
#' \strong{Applicable counties:}
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
#'
#' @details
#' Multiple correlation coefficient R = 0.80
#'
#' Spread about the function sf = 0.187
#'
#' sf/Spread about the mean = 0.609
#'
#' Number of observations = 831
#'
#'
#' NB in Söderberg (1986), no correction for logarithmic bias was introduced, as, (freely translated) p. 114:
#' "At the presentation of the functions we lightly touched on the effects of the measurement
#' errors in the variables which had been included in the regression. It was then concluded
#' that several factors contrived to that the spread about the functions is overestimated.
#' Therefore no correction for logarithmic bias was carried out, primarily because the
#' effect of the errors in the indipendent variables on the spread about the function
#' are difficult to establish. If one were to assume that the spread about the function
#' was overestimated by 10 percent, the predicted growth from the functions would be increased by
#' roughly 2 percent. For 20 percents overestimation, an increase of predicted growth by 4.5 perpcent."
#'
#'
#'
#'
#' @param diameter_cm Diameter at breast height
#' @param diameter_largest_tree_on_plot_cm Diameter at breast height of the largest tree on the plot.
#' @param Basal_area_Spruce_m2_ha Basal area Spruce on the plot, m^2 / ha.
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param SI_species Species for which SIH100 was estimated. One of : 'Picea abies' or 'Pinus sylvestris'.
#' @param SI100 Site Index H100, m.
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
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param aspect Aspect, one of: "north","south" or 0.
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_form_quotient_southern_Sweden_Broadleaves <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  Basal_area_Spruce_m2_ha,
  Basal_area_plot_m2_ha,
  SI_species,
  SI100,
  age_at_breast_height,
  latitude,
  vegetation,
  soil_moisture,
  aspect,
  divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Spruce <- Basal_area_Spruce_m2_ha/Basal_area_plot_m2_ha
  south <- ifelse(aspect=="south",1,0)
  herb <- ifelse(vegetation<7,1,0)
  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      -0.14927E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.37752E+04*((1/((diameter_cm*10) + 50))^2)+ #diameter in mm
        -0.25858E+02*((1/(age_at_breast_height+10))^2)+
        +0.10999E-02*spruce*SI100*10+#m to dm.
        +0.10513E-02*pine*SI100*10+ #m to dm.
        +0.11070E-01*Basal_area_plot_m2_ha+
        -0.10669E-03*(Basal_area_plot_m2_ha^2)+
        +0.28134E+00*diameter_quotient+
        -0.29273E+00*(diameter_quotient^2)+
        +0.22956E-01*latitude+
        +0.75214E-01*BA_quotient_Spruce+
        -0.72064E-01*divided_plot+
        +0.10339E+00*south+
        -0.27334E-01*herb+
        -0.44593E-01*moist+
        +0.58178E+00
    )
  )


}
