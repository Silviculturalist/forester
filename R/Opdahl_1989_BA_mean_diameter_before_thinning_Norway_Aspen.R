#' Basal area mean diameter before thinning for Aspen stands in Norway from Opdahl 1989.
#'
#' @source Opdahl, H. 1989. Avsmaling og volum hos osp (Populus tremula L.) i Sør-Norge. (Tapering and volume of Aspen (Populus tremula L.) in South Norway.) Medd. Nor. Inst. Skogforsk. 43.2.:42s. in
#' Opdahl, H. 1991. Bonitet, vekst og produksjon hos osp (Populus tremula L.) i Sør-Norge. (Site-index, growth and yield in Aspen (Populus tremula L.) stands in South Norway.) Medd. Skogforsk. 44(11):1-44. ISBN 82-7169-527-4. ISSN 0803-2866., page 21. (function 6).
#'
#'
#' @param SIH40 Site Index H40, e.g. [forester::Opdahl_1991_height_trajectory_Norway_Aspen()]
#' @param dominant_height Dominant height of stand, m. (arithmetic mean of 100 trees with largest diameter per hectare)
#' @param stems_per_ha_before_thinning Number of Stems per hectare before thinning.
#' @param correction Should the function use the correction used in the original simulation? E.g. return 96.7%. Default =TRUE
#'
#' @return Basal area mean diameter (cm).
#' @export
#'
#' @examples
Opdahl_1989_BA_mean_diameter_before_thinning_Norway_Aspen <- function(
  SIH40,
  dominant_height,
  stems_per_ha_before_thinning,
  correction=TRUE
){
  correction <- ifelse(isTRUE(correction),0.967,1)

  return(
    (68.69078 * (SIH40^0.05957) * (stems_per_ha_before_thinning^(-0.45314)) * (dominant_height^0.49641))*correction
  )
}
