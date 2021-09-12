#' 5 year Basal area increment over bark, Spruce, southern Sweden, under 65 years at breast height from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 186.
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
#'
#' @details
#' Multiple correlation coefficient R = 0.85
#'
#' Spread about the function sf = 0.526
#'
#' sf/Spread about the mean = 0.52
#'
#' Number of observations = 3564
#'
#' Sum of weights: 2997.7
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
#' @param Basal_area_Spruce_m2_ha Basal area Spruce on the plot, m^2 / ha.
#' @param Basal_area_Birch_m2_ha Basal area Birch on the plot, m^2 / ha.
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param thinned TRUE if the stand has been thinned, otherwise FALSE.
#' @param last_thinned Number of growing seasons since last thinning.
#' @param SI100 Site Index H100, m.
#' @param SI_species Species for which SIH100 was estimated. One of : 'Picea abies' or 'Pinus sylvestris'.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param aspect Aspect, one of: "north", "south" or 0.
#' @param peatland 1 if plot is Peatland, 0 for others (default).
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#' @param fertilised_plot 1 for fertilised plots, 0 for others (default).
#' @param plot_inventoried_76_77 1 for plots measured in the years 1976-77, 0 for others (default).
#'
#' @return Basal area increment during 5 years, m2.
#' @export
#'
#' @examples
Soderberg_1986_BA_increment_southern_Sweden_under_65_Spruce <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  Basal_area_of_tree_m2,
  Basal_area_Pine_m2_ha,
  Basal_area_Spruce_m2_ha,
  Basal_area_Birch_m2_ha,
  Basal_area_plot_m2_ha,
  age_at_breast_height,
  thinned,
  last_thinned,
  SI100,
  SI_species,
  aspect,
  soil_moisture,
  county,
  peatland=0,
  divided_plot=0,
  fertilised_plot=0,
  plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- Basal_area_of_tree_m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  dry <- ifelse(soil_moisture==1,1,0)
  north <- ifelse(aspect=="north",1,0)
  south <- ifelse(aspect=="south",1,0)
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Pine <- Basal_area_Pine_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Spruce <- Basal_area_Spruce_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Birch <- Basal_area_Birch_m2_ha/Basal_area_plot_m2_ha
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.61903E-01*Basal_area_plot_m2_ha+
                         +0.67406E-03*(Basal_area_plot_m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.51359E-01*Basal_area_plot_m2_ha+
                                +0.45834E-03*(Basal_area_plot_m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.60125E-01*Basal_area_plot_m2_ha+
                                +0.65807E-03*(Basal_area_plot_m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.12995E+01*log(basal_area_of_tree_cm2)+
        -0.32205E-03*basal_area_of_tree_cm2+
        -0.42745E+01*(log(basal_area_of_tree_cm2)/(age_at_breast_height+10))+
        +0.13002E+03*(1/(age_at_breast_height+10))+
        -0.88771E+03*((1/(age_at_breast_height+10))^2)+
        +thinning+
        -0.44756E+00*diameter_quotient+
        -0.19273E+00*BA_quotient_Pine+
        -0.42807E+00*BA_quotient_Spruce+
        -0.30893E+00*BA_quotient_Birch+
        +0.21660E-03*spruce*SI100*10+#m to dm.
        +0.70422E-03*pine*SI100*10+ #m to dm.
        +0.55118E-01*peatland+
        -0.10323E+00*dry+
        +0.45389E-01*moist+
        -0.11319E+00*north+
        -0.79993E-01*south+
        +0.98700E-01*region5+
        +0.63629E-01*divided_plot+
        +0.37888E-01*fertilised_plot+
        -0.34626E+01

    )/10000 #cm2 to m2
  )


}
