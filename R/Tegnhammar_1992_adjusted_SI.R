#' Tegnhammars' correction for SI 1992
#' @details This function corrects height development functions by Hägglund
#'  (1972, 1973) for the 'apparent' trend, an age bias, wherein the functions
#'  would indicate higher SIH in young forests as compared to older forests,
#'  even if the site index is the same.
#' @description Tegnhammar's dissertation investigated a trend in the site index
#'  curves for spruce in Sweden, possibly caused by stem sectioning of elderly
#'  trees that were suppressed in their youth; young planted forests are
#'  composed of better plant material; young forests have recieved a better
#'  start as a result of methods introduced during the 1920-1950's; older stands
#'  which were not suitable for SI estimation by height were measured in such
#'  a manner nonetheless; The risk of including hidden top breakages is larger
#'  for older trees; & the productivity has increased with time due to nitrogen
#'  deposition - younger trees have been affected for a longer period of their
#'  lifetime.
#'
#' @source Tegnhammar, L. (1992) "Om skattningen av ståndortsindex för gran",
#' eng:"On the estimation of site index for Norway spruce". Report 53. Dept. of
#' Forest Survey. Swedish University of Agricultural Sciences. Umeå.p.14.
#' ISSN 0348-0496
#'
#' @param SIH Site index, metres.
#' @param dominant_age arithmetic mean of the age at breast height of the
#' dominant trees.
#' @param latitude degrees
#'
#' @return SIHjust, m.
#' @export
Tegnhammar_1992_adjusted_SI <- function(SIH, dominant_age, latitude) {
  return(((SIH*10) + (3.89 - 0.0498 * latitude) * (dominant_age - 15))/10)
}
