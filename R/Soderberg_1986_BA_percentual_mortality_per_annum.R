#' Mortality in self-thinned stands
#'
#' @source From Table 7.2, Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 86.
#'
#' @description
#'
#' Material from Unthinned permanent plots.
#'
#' \strong{N.B. As unthinned plots} present a relatively extreme case, this function is not suggested to be used other than in plots with very high basal area. (p. 86 & p. 90)
#'
#' @details
#'
#' Multiple correlation coefficient R = 0.40
#'
#' Spread about the function sf = 0.0059
#'
#' sf/Spread about the mean = 92.7
#'
#' Number of observations = 532.
#'
#' @param SI_species Species for which SIH100 was estimated. One of 'Picea abies' or 'Pinus sylvestris'.
#' @param SIH100 Site Index H100, metres.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_plot_m2_ha Basal area of all tree species on the plot, m^2 / ha.
#' @param Basal_area_Spruce_m2_ha Basal area Spruce on the plot, m^2 / ha.
#' @param Basal_area_Deciduous_m2_ha Basal area Deciduous trees on the plot, m^2 / ha.
#'
#' @return Mortality as annual proportion of basal area.
#' @export

Soderberg_1986_BA_percentual_mortality_per_annum <- function(SI_species,
                                                     SIH100,
                                                     total_age_stand,
                                                     Basal_area_plot_m2_ha,
                                                     Basal_area_Spruce_m2_ha,
                                                     Basal_area_Deciduous_m2_ha
                                                     ){

  BA_quotient_Spruce <- Basal_area_Spruce_m2_ha / Basal_area_plot_m2_ha
  BA_quotient_Deciduous <- Basal_area_Deciduous_m2_ha / Basal_area_plot_m2_ha

  spruce <- ifelse(SI_species=="Picea abies",1,0)
  pine <- ifelse(SI_species=="Pinus sylvestris",1,0)

  return(
      +0.60949*(1/(total_age_stand+10))+
      -12.5903*((1/(total_age_stand+10))^2)+
      +0.3317E-3*Basal_area_plot_m2_ha+
      -0.01006*(log(Basal_area_plot_m2_ha))+
      +0.173E-3*spruce*SIH100+
      +0.156E-3*pine*SIH100+
      -0.0130*BA_quotient_Spruce+
      +0.0119*(BA_quotient_Spruce^2)+
      +0.0189*(BA_quotient_Deciduous)+
      -0.0174*(BA_quotient_Deciduous^2)+
      +0.0232
  )

}
