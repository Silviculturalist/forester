#' Form quotient for Norway Spruce, southern Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 218.
#'
#' @description
#'
#' \strong{OBSERVE:} As per p. 64 , if county is equal to "Gotland": "Residual studies for these species indicate the functions for the mainland (Spruce south, Birch south) can be used with a reduction of the constant term amounting to -0.089 and -0.067, respectively".
#'
#' \strong{Applicable counties:}
#'
#' *Stockholm
#' *Uppsala
#' *Västmanland
#' *Södermanland
#' *Örebro
#' *Östergötland
#' *Skaraborg
#' *Älvsborg - Västergötlands landskap
#' *Älvsborg - Dalslands landskap
#' *Jönköping
#' *Kronoberg
#' *Kalmar
#' *Halland
#' *Kristianstad
#' *Malmöhus
#' *Blekinge
#' *Gotland
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
#' @details
#' Multiple correlation coefficient R = 0.94
#'
#' Spread about the function sf = 0.128
#'
#' sf/Spread about the mean = 0.346
#'
#' Number of observations = 7311
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
#' @param altitude Altitude, meters above sea level.
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#' @param continental TRUE, if the plot is situated in a continental climatic region. cf. Ångström 1958. e.g. [forester::local_climate_Sweden()]. Otherwise FALSE.
#' @param maritime TRUE, if the plot is situated in a maritime climatic region. cf. Ångström 1958. e.g. [forester::local_climate_Sweden()]. Otherwise FALSE.
#' @param county County name.
#' @param aspect If more than 2:20 / 5\%, one of the following. Otherwise 0.
#'
#' \tabular{cl}{
#' 1 \tab North  \cr
#' 2 \tab North-East \cr
#' 3 \tab East \cr
#' 4 \tab South-East \cr
#' 5 \tab South \cr
#' 6 \tab South-West \cr
#' 7 \tab West \cr
#' 8 \tab North-West \cr
#' }
#' @param lateral_water Type 1="Missing", 2="Seldom",3="Shorter periods", 4="Longer periods",5="Slope".
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_form_quotient_southern_Sweden_Spruce <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  continental,
  maritime,
  Basal_area_Spruce_m2_ha,
  Basal_area_plot_m2_ha,
  SI_species,
  SI100,
  age_at_breast_height,
  aspect,
  latitude,
  altitude,
  lateral_water,
  divided_plot=0,
  county
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Spruce <- Basal_area_Spruce_m2_ha/Basal_area_plot_m2_ha
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)

  south_eastern_county <- ifelse(county%in%c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  value_constant <- ifelse(county!="Gotland",0.27730E+01, ((0.27730E+01)-0.089))

  return(
    exp(
      -0.25255E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.52037E+04*((1/((diameter_cm*10) + 50))^2)+ #diameter in mm
        -0.79332E+01*(1/(age_at_breast_height+10))+
        +0.15360E+03*((1/(age_at_breast_height+10))^2)+
        -0.25202E-02*age_at_breast_height+
        -0.50822E-01*(diameter_cm*10/age_at_breast_height)+ #diameter in mm
        +0.95744E-03*spruce*SI100*10+#m to dm.
        +0.11126E-02*pine*SI100*10+ #m to dm.
        +0.86560E-02*Basal_area_plot_m2_ha+
        -0.68753E-04*(Basal_area_plot_m2_ha^2)+
        +0.52388E+00*diameter_quotient+
        -0.44192E+00*(diameter_quotient^2)+
        +0.33543E-03*altitude+
        -0.11486E-05*(altitude)^2+
        +0.14504E+00*BA_quotient_Spruce+
        +0.21586E-01*continental+
        -0.18175E-01*maritime+
        +0.16159E-01*north+
        -0.51101E-01*divided_plot+
        -0.14608E-01*seldom_lateral_water+
        +0.17760E-01*south_eastern_county+
        +value_constant
    )
  )


}
