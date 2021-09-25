#' 5 year Basal area increment over bark, Oak, Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 204.
#'
#'
#' @details
#' Multiple correlation coefficient R = 0.90
#'
#' Spread about the function sf = 0.417
#'
#' sf/Spread about the mean = 0.45
#'
#' Number of observations = 222
#'
#' Sum of weights: 187.9
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
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param thinned TRUE if the stand has been thinned, otherwise FALSE.
#' @param last_thinned Number of growing seasons since last thinning.
#' @param aspect Aspect, one of: "north", "south" or 0.
#' @param maritime TRUE, if the plot is situated in a maritime climatic region. cf. Ångstrom 1958. e.g. [forester::local_climate_sweden()] . Otherwise FALSE.
#'
#' @return Basal area increment during 5 years, m2.
#' @export
#'
#' @examples
Soderberg_1986_BA_increment_Sweden_Oak <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  Basal_area_of_tree_m2,
  Basal_area_plot_m2_ha,
  age_at_breast_height,
  thinned,
  last_thinned,
  aspect,
  maritime
){
  basal_area_of_tree_cm2 <- Basal_area_of_tree_m2*10000
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  north <- ifelse(aspect=="north",1,0)
  south <- ifelse(aspect=="south",1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.58270E-01*Basal_area_plot_m2_ha+
                         +0.77030E-03*(Basal_area_plot_m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.48067E-01*Basal_area_plot_m2_ha+
                                +0.67014E-03*(Basal_area_plot_m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.35960E-01*Basal_area_plot_m2_ha+
                                +0.34773E-03*(Basal_area_plot_m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10683E+01*log(basal_area_of_tree_cm2)+
        -0.42896E+01*(log(basal_area_of_tree_cm2)/(age_at_breast_height+10))+
        +0.16232E+03*(1/(age_at_breast_height+10))+
        -0.15683E+04*((1/(age_at_breast_height+10))^2)+
        +thinning+
        +0.29030E+00*diameter_quotient+
        +0.25426E+00*north+
        +0.22321E+00*south+
        +0.13856E+00*maritime+
        -0.37307E+01

    )/10000 #cm2 to m2
  )


}
