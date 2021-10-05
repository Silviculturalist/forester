#' Form quotient for Oak, Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 230.
#'
#' @description
#'
#' \strong{OBSERVE:} The function has a typo in the book. Although the variable
#' description states that it is Altitude, the symbol shows LAT. I have
#' interpreted this as that it should be Altitude, since the size of the
#' coefficient is most alike coefficients for ALT in other related functions.
#'
#'
#' @details Multiple correlation coefficient R = 0.86
#'
#' Spread about the function sf = 0.180
#'
#' sf/Spread about the mean = 0.516
#'
#' Number of observations = 292
#'
#'
#' NB in Söderberg (1986), no correction for logarithmic bias was introduced,
#' as, (freely translated) p. 114: "At the presentation of the functions we
#' lightly touched on the effects of the measurement errors in the variables
#' which had been included in the regression. It was then concluded that several
#' factors contrived to that the spread about the functions is overestimated.
#' Therefore no correction for logarithmic bias was carried out, primarily
#' because the effect of the errors in the indipendent variables on the spread
#' about the function are difficult to establish. If one were to assume that the
#' spread about the function was overestimated by 10 percent, the predicted
#' growth from the functions would be increased by roughly 2 percent. For 20
#' percents overestimation, an increase of predicted growth by 4.5 perpcent."
#'
#'
#'
#'
#' @param diameter_cm Diameter at breast height
#' @param diameter_largest_tree_on_plot Diameter at breast height of the largest tree on the plot.
#' @param Basal_area_Beech_m2_ha Basal area Beech on the plot, m^2 / ha.
#' @param Basal_area_Birch_m2_ha Basal area Birch on the plot, m^2 / ha.
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 /
#'   ha.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param SI_species Species for which SIH100 was estimated. One of : 'Picea abies' or 'Pinus sylvestris'.
#' @param SI100 Site Index H100, m.
#' @param altitude Altitude, meters above sea level.
#' @param divided_plot 1 for plots described in different parts, which appears
#'   when the original plot consists of different land classes, density classes
#'   or cutting classes or belongs to different owners. 0 for full plots
#'   (default).
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
#' @param lateral_water Type 1="Missing", 2="Seldom",3="Shorter periods", 4="Longer periods",5="Slope".
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_form_quotient_Sweden_Oak <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  Basal_area_Beech_m2_ha,
  Basal_area_Birch_m2_ha,
  Basal_area_plot_m2_ha,
  SI_species,
  SI100,
  age_at_breast_height,
  vegetation,
  soil_moisture,
  lateral_water,
  altitude,
  divided_plot=0,
  county
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  BA_quotient_Beech <- Basal_area_Beech_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Birch <- Basal_area_Birch_m2_ha/Basal_area_plot_m2_ha
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)
  herb <- ifelse(vegetation<7,1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      -0.27070E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.11454E+05*((1/((diameter_cm*10) + 50))^2)+ #diameter in mm
        -0.16097E-02*((1/(age_at_breast_height+10))^2)+
        -0.17742E-01*(diameter_cm*10/age_at_breast_height)+ #diameter in mm
        +0.39008E-03*spruce*SI100*10+#m to dm.
        +0.36858E-03*pine*SI100*10+ #m to dm
        +0.15098E-01*Basal_area_plot_m2_ha+
        -0.14109E-03*(Basal_area_plot_m2_ha^2)+
        +0.57341E+00*diameter_quotient+
        -0.52971E+00*(diameter_quotient^2)+
        -0.31421E-03*altitude+
        -0.91133E-01*BA_quotient_Birch+
        +0.21363E+00*BA_quotient_Beech+
        -0.12131E+00*divided_plot+
        +0.64025E-01*herb+
        +0.12112E+00*moist+
        -0.65506E-01*seldom_lateral_water+
        -0.38960E-01*region5+
        +0.25636E+01
    )
  )


}
