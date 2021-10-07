#' 5 year Basal area increment over bark, other Broadleaves, southern Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 200.
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
#' Multiple correlation coefficient R = 0.79
#'
#' Spread about the function sf = 0.630
#'
#' sf/Spread about the mean = 0.62
#'
#' Number of observations = 643
#'
#' Sum of weights: 541.1
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
#' @param aspect Aspect, one of: "north", "south" or 0.
#' @param latitude Latitude, degrees.
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#'
#' @return Basal area increment during 5 years, m2.
#' @export
#'
#' @examples
Soderberg_1986_BA_increment_southern_Sweden_Broadleaves <- function(
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
  aspect,
  latitude,
  divided_plot=0
){
  basal_area_of_tree_cm2 <- Basal_area_of_tree_m2*10000
  north <- ifelse(aspect=="north",1,0)
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Pine <- Basal_area_Pine_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Spruce <- Basal_area_Spruce_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Birch <- Basal_area_Birch_m2_ha/Basal_area_plot_m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.57341E-01*Basal_area_plot_m2_ha+
                         +0.72742E-03*(Basal_area_plot_m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.34258E-01*Basal_area_plot_m2_ha+
                                +0.15713E-03*(Basal_area_plot_m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.74100E-01*Basal_area_plot_m2_ha+
                                +0.14893E-02*(Basal_area_plot_m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.12426E+01*log(basal_area_of_tree_cm2)+
        -0.38067E-03*Basal_area_of_tree_m2+
        +0.86923E+02*(1/(age_at_breast_height+10))+
        -0.49647E+03*((1/(age_at_breast_height+10))^2)+
        +thinning+
        -0.14061E+01*diameter_quotient+
        +0.85103E+00*(diameter_quotient^2)+
        +0.32525E+00*BA_quotient_Pine+
        +0.24568E+00*BA_quotient_Spruce+
        +0.20104E+00*BA_quotient_Birch+
        +0.90258E-03*spruce*SI100*10+#m to dm.
        +0.84649E-03*pine*SI100*10+ #m to dm.
        +0.49305E-01*latitude+
        -0.14761E+00*north+
        +0.10352E+00*divided_plot+
        -0.63258E+01

    )/10000 #cm2 to m2
  )


}
