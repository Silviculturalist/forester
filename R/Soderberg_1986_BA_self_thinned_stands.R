#' Basal area in self-thinned stands

#' @source From Table 7.1, Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 83.
#'
#' @description
#'
#' Material from Unthinned permanent plots.
#'
#' @details
#'
#' Multiple correlation coefficient R = 0.87
#'
#' Spread about the function sf = 0.15
#'
#' sf/Spread about the mean = 50.2
#'
#' Number of observations = 532.
#'
#' @param SI_species Species for which SIH100 was estimated. One of 'Picea abies' or 'Pinus sylvestris'.
#' @param SIH100 Site Index H100, metres.
#' @param total_age_stand Total age of the stand.
#' @param BA_quotient_Spruce Basal area Spruce / Basal area
#' @param BA_quotient_Deciduous Basal area Deciduous trees / Basal area
#' @param stems_per_ha Number of stems per hectare.
#'
#' @return Basal area m2/ha.
#' @export
#'
#' @examples
Soderberg_1986_BA_self_thinned_stands <- function(
  SI_species,
  SIH100,
  total_age_stand,
  BA_quotient_Spruce,
  BA_quotient_Deciduous,
  stems_per_ha
){

  spruce <- ifelse(SI_species=="Picea abies",1,0)
  pine <- ifelse(SI_species=="Pinus sylvestris",1,0)


  return(
    exp(

    -18.612*((1/(total_age_stand+10)))+
    -765.295*(((1/(total_age_stand+10)))^2)+
    +0.04798*SIH100*spruce+
    +0.05589*SIH100*pine+
    +0.06717E-4*stems_per_ha+
    -0.2864E-8*(stems_per_ha^2)+
    +0.7204*BA_quotient_Spruce+
    -0.4879*(BA_quotient_Spruce^2)+
    +0.1062*BA_quotient_Deciduous+
    -0.2073*(BA_quotient_Deciduous^2)+
    +2.5225

    )
  )



}
