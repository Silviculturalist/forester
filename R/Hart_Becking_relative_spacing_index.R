#' Calculate the Hart-Becking Relative Spacing Index.
#'
#' @param stems_per_ha Stems per hectare
#' @param dominant_height Dominant height of the stand.
#'
#' @return Hart - Becking Spacing Index (percent).
#' @export
#'
#' @examples
#' Hart_Becking_relative_spacing_index(stems_per_ha = 1600,dominant_height =8.6 )
Hart_Becking_relative_spacing_index <- function(
  stems_per_ha,
  dominant_height
){

  return(
    (sqrt(10000/stems_per_ha)/dominant_height)*100
  )

}
