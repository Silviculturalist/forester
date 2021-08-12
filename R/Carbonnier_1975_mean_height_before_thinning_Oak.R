#' Calculate the mean height in an Oak stand before a thinning.
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 72.
#'
#' @param dominant_height_m Dominant height of stand, in metres.
#' @param stems Number of stems per hectare.
#'
#' @return Mean height of the Oak stand, in meters.
#' @export
#'
#' @examples
Carbonnier_1975_mean_height_before_thinning_Oak <- function(
  dominant_height_m,
  stems
){
  return(
    -1.3955+
    +1.0632*dominant_height_m+
    +0.1520*(stems/100)+
    -0.1574*((stems*dominant_height_m)/1000)
  )
}
