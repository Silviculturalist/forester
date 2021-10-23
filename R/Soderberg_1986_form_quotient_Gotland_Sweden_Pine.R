#' Form quotient for Scots Pine on the island of Gotland, Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 214.
#'
#' @description
#'
#' \strong{OBSERVE:} The following interpretation was made of the function - since only a factor was
#' given for the Site Index in the case that the indicating species was Picea abies, this was interpreted as mandatory.
#' Therefore, SI_species and SI100 have been replaced by SI100_Spruce.
#'
#' \strong{Applicable counties:}
#'
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
#' Multiple correlation coefficient R = 0.84
#'
#' Spread about the function sf = 0.150
#'
#' sf/Spread about the mean = 0.545
#'
#' Number of observations = 351
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
#' @param SI100_Spruce Site Index H100 for Spruce, m.
#' @param altitude Altitude, meters above sea level.
#' @param divided_plot 1 for plots described in different parts, which appears when the original plot consists of different land classes, density classes or cutting classes or belongs to different owners. 0 for full plots (default).
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_diameter_quotient_Gotland_Sweden_Pine <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  Basal_area_Pine_m2_ha,
  Basal_area_plot_m2_ha,
  SI100_Spruce,
  age_at_breast_height,
  altitude,
  divided_plot=0
){

  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  BA_quotient_Pine <- Basal_area_Pine_m2_ha/Basal_area_plot_m2_ha


  return(
    exp(
      -0.21146E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.71765E+04*((1/((diameter_cm*10) + 50))^2)+ #diameter in mm
        -0.62341E+01*(1/(age_at_breast_height+10))+
        +0.15382E+03*((1/(age_at_breast_height+10))^2)+
        +0.39119E-03*SI100_Spruce*10+#m to dm.
        +0.14818E-01*Basal_area_plot_m2_ha+
        -0.12168E-03*(Basal_area_plot_m2_ha^2)+
        -0.19266E+00*(diameter_quotient^2)+
        +0.14934E-02*(altitude)+
        -0.13057E+00*divided_plot+
        -0.14060E+00*BA_quotient_Pine+
        +0.23998E+01

    )
  )


}
