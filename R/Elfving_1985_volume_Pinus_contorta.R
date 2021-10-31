#' Volume of Lodgepole Pine trees, from Elfving 1985.
#'
#' @source Elfving, B. 1985. Nya data om contortatallens produktion. SLU, Inst. f. skogsskötsel. Arbetsrapporter, nr. 3.
#'
#' @param diameter_cm Diameter at breast height, cm.
#' @param height_m Height of tree, metres.
#'
#' @return Volume of tree, dm3.
#' @export
#'
#' @examples
Elfving_1985_volume_Pinus_contorta <- function(
  diameter_cm,
  height_m
){
  return(
    0.98*(0.1121*(diameter_cm^2) + 0.02870*(diameter_cm^2)*height_m - 0.000061*(diameter_cm^2)*(height_m^2) - 0.09176*diameter_cm*height_m + 0.01249*diameter_cm*(height_m^2))
  )
}
