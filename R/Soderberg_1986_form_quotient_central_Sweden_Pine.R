#' Form quotient for Scots Pine, central Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 210.
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
#' @details
#' Multiple correlation coefficient R = 0.94
#'
#' Spread about the function sf = 0.105
#'
#' sf/Spread about the mean = 0.328
#'
#' Number of observations = 2248
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
#' @param Basal_area_of_tree_m2 Basal area of the tree, m^2.
#' @param Basal_area_Pine_m2_ha Basal area Pine on the plot, m^2 / ha.
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param SI_species Species for which SIH100 was estimated. One of : 'Picea abies' or 'Pinus sylvestris'.
#' @param SI100 Site Index H100, m.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, meters above sea level.
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param fertilised_plot 1 for fertilised plots, 0 for others (default).
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#'
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_form_quotient_central_Sweden_Pine <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  distance_to_coast_km,
  Basal_area_Pine_m2_ha,
  Basal_area_plot_m2_ha,
  SI_species,
  SI100,
  age_at_breast_height,
  latitude,
  altitude,
  soil_moisture,
  fertilised_plot,
  divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Pine <- Basal_area_Pine_m2_ha/Basal_area_plot_m2_ha
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)
  moist <- ifelse(soil_moisture>3,1,0)


  return(
    exp(
      -0.23404E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.75190E+04*((1/((diameter_cm*10) + 50))^2)+ #diameter in mm
        -0.13151E+02*(1/(age_at_breast_height+10))+
        +0.17182E+03*((1/(age_at_breast_height+10))^2)+
        -0.15626E-02*age_at_breast_height+
        -0.33891E-01*(diameter_cm*10/age_at_breast_height)+ #diameter in mm
        +0.11113E-02*spruce*SI100*10+#m to dm.
        +0.11987E-02*pine*SI100*10+ #m to dm.
        +0.10184E-01*Basal_area_plot_m2_ha+
        -0.90301E-04*(Basal_area_plot_m2_ha^2)+
        -0.18661E+00*diameter_quotient+
        +0.48146E-01*(diameter_quotient^2)+
        +0.35143E+01*latitude+
        -0.28628E-01*(latitude^2)+
        -0.33545E-06*(altitude^2)+
        -0.45949E-01*close_to_coast+
        -0.46950E-01*divided_plot+
        +0.11568E-01*fertilised_plot+
        -0.12351E-01*moist+
        -0.16899E-01*BA_quotient_Pine+
        -0.104909E+03

    )
  )


}
