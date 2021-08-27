#' Volume of Larch trees, from Carbonnier 1954.
#'
#' @source Carbonnier, C. 1954. Funktioner för kubering av europeisk, sibirisk och japansk lärk. MS.
#' @source PM for Heureka 2004-01-20 Björn Elfving. Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_%28Elfving_rapportutkast%29.pdf}
#'
#' @param diameter_cm Diameter at breast height, cm.
#' @param height_m Height of tree, metres.
#'
#' @return Volume of tree, dm3.
#' @export
#'
#' @examples
Carbonnier_1954_volume_Larch <- function(
  diameter_cm,
  height_m
){
  return(
  0.04801*(diameter_cm^2)*height_m + 0.08886*(diameter_cm^2) - 0.01012*(diameter_cm^3) - 0.08406*diameter_cm*height_m + 0.1972*height_m
  )
}
