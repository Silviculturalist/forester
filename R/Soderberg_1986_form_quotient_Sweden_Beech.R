#' Form quotient for Beech, Sweden from Söderberg (1986)
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#'   Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#'   Functions for forecasting of timber yields - Increment and form height for
#'   individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 228.
#'
#' @description
#'
#' \strong{OBSERVE:} The function has a typo in the book. Although the variable
#' description states that it is Altitude^2, the symbol shows LAT^2. I have
#' interpreted this as that it should be Altitude^2, since the size of the
#' coefficient is most alike coefficients for ALT^2 in other related functions.
#'
#' \strong{OBSERVE:} The following interpretation was made of the function - since only
#' a factor was given for the Site Index in the case that the indicating species was
#' Picea abies, this was interpreted as mandatory. Therefore, SI_Species and SI100 have
#' been replaced by SI100_Spruce.
#'
#'
#'
#' @details Multiple correlation coefficient R = 0.90
#'
#' Spread about the function sf = 0.165
#'
#' sf/Spread about the mean = 0.442
#'
#' Number of observations = 219
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
#' @param Basal_area_Beech_m2_ha Basal area Beech on the plot, m^2 / ha.
#' @param Basal_area_Oak_m2_ha Basal area Oak on the plot, m^2 / ha.
#' @param Basal_area_plot_m2_ha Basal area of all tree species the plot, m^2 /
#'   ha.
#' @param age_at_breast_height Age at breast height of the tree.
#' @param SI100_Spruce Site Index H100 for Spruce, m.
#' @param altitude Altitude, meters above sea level.
#' @param divided_plot 1 for plots described in different parts, which appears
#'   when the original plot consists of different land classes, density classes
#'   or cutting classes or belongs to different owners. 0 for full plots
#'   (default).
#' @param aspect Aspect, one of: "north","south" or 0.
#' @return Form quotient, metres.
#' @export
#'
#' @examples
Soderberg_1986_form_quotient_Sweden_Beech <- function(
  diameter_cm,
  Basal_area_Beech_m2_ha,
  Basal_area_Oak_m2_ha,
  Basal_area_plot_m2_ha,
  SI100_Spruce,
  age_at_breast_height,
  aspect,
  altitude,
  divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  BA_quotient_Beech <- Basal_area_Beech_m2_ha/Basal_area_plot_m2_ha
  BA_quotient_Oak <- Basal_area_Oak_m2_ha/Basal_area_plot_m2_ha
  north <- ifelse(aspect=="north",1,0)

  return(
    exp(
      -0.21212E+03*(1/((diameter_cm*10) + 50))+ #diameter in mm
        +0.73868E+04*((1/((diameter_cm*10) + 50))^2)+ #diameter in mm
        -0.16097E+02*(1/(age_at_breast_height+10))+
        +0.27278E+03*((1/(age_at_breast_height+10))^2)+
        -0.30178E-01*(diameter_cm*10/age_at_breast_height)+ #diameter in mm
        +0.21615E-03*SI100_Spruce*10+#m to dm.
        +0.14469E-01*Basal_area_plot_m2_ha+
        -0.13999E-03*(Basal_area_plot_m2_ha^2)+
        -0.30893E-05*(altitude^2)+
        +0.17708E+00*BA_quotient_Beech+
        -0.14122E+00*BA_quotient_Oak+
        -0.13570E+00*divided_plot+
        -0.86474E-01*north+
        +0.27313E+01
    )
  )


}
