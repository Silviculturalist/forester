#' Mean diameter for initial Oak stands from Carbonnier 1975
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 18.
#'
#' @details
#' s= 0.872
#'
#'
#'
#' @param stems Stems per hectare
#' @param dominant_height_m Dominant height, m.
#'
#' @return Mean stem diameter, cm.
#' @export
#'
#' @examples
Carbonnier_1975_initial_stand_mean_diameter <- function(
  stems,
  dominant_height_m
){
  return(
  2.3377+
  +0.6322*(10000/stems)+
  +0.3617*dominant_height_m
  )
}
