#' Form quotient for Birch, southern Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 222.
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
#' Multiple correlation coefficient R = 0.85
#'
#' Spread about the function sf = 0.161
#'
#' sf/Spread about the mean = 0.533
#'
#' Number of observations = 1725
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
#' @param Basal_area_Birch_m2_ha Basal area Birch on the plot, m^2 / ha.
#' @param Basal_area_Pine_m2_ha Basal area Pine on the plot, m^2 / ha.
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age_at_breast_height Age at breast height of the tree.
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
#' @param SI_species Species for which SIH100 was estimated. One of : 'Picea abies' or 'Pinus sylvestris'.
#' @param SI100 Site Index H100, m.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, meters above sea level.
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#' @param maritime TRUE, if the plot is situated in a maritime climatic region. cf. Ångström 1958. e.g. [forester::local_climate_Sweden()]. Otherwise FALSE.
#' @param county County name.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param fertilised_plot  1 for fertilised plots, 0 for others (default).
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_form_quotient_southern_Sweden_Birch <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  maritime,
  Basal_area_Birch_m2_ha,
  Basal_area_Pine_m2_ha,
  Basal_area_plot_m2_ha,
  SI_species,
  SI100,
  vegetation,
  age_at_breast_height,
  latitude,
  altitude,
  divided_plot=0,
  fertilised_plot=0,
  county
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Pine <- Basal_area_Pine_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Birch <- Basal_area_Birch_m2_ha/Basal_area_plot_m2_ha
  herb <- ifelse(vegetation<7,1,0)
  empetrum_calluna <- ifelse(vegetation%in%c(15,16),1,0)
  dry <- ifelse(soil_moisture==1,1,0)

  region5 <- ifelse(county %in% c("Blekinge","Kristianstad","Malmöhus","Västra Götaland","Halland","Gotland"),1,0)

  value_constant <- ifelse(county!="Gotland",0.12148E+01, ((0.12148E+01)-0.067))

  return(
    exp(
      -0.14327E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.39558E+02*(1/(age_at_breast_height+10))+
        -0.17589E-02*age_at_breast_height+
        -0.34887E-01*(diameter_cm*10/age_at_breast_height)+ #diameter in mm
        +0.45798E-03*spruce*SI100*10+#m to dm.
        +0.50812E-03*pine*SI100*10+ #m to dm.
        +0.11981E-01*Basal_area_plot_m2_ha+
        -0.13693E-03*(Basal_area_plot_m2_ha^2)+
        +0.13814E+00*diameter_quotient+
        -0.25030E+00*(diameter_quotient^2)+
        +0.22195E-01*latitude+
        -0.26037E-05*latitude*altitude+
        -0.63942E-01*BA_quotient_Pine+
        +0.40774E-01*BA_quotient_Birch+
        -0.56749E-01*maritime+
        -0.73603E-01*divided_plot+
        +0.12825E-01*herb+
        -0.57647E-01*dry+
        -0.17425E+00*empetrum_calluna+
        -0.40860E-01*region5+
        +0.56214E-01*fertilised_plot+
        +value_constant
    )
  )


}
