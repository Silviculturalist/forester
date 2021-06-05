#' Height Development function for Norway Spruce in Norway, from Allen et al. 2020.
#'
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: <https://doi.org/10.1080/02827581.2020.1773525>
#'
#' @details N.B. Although the paper uses log10(), it should be ln()!
#' This is corrected in this function.
#'
#'
#'
#' N.B. The function fitted in the paper has several changes compared to the function from
#' Diéguez-Aranda, U.; Burkhart, H. E.; Rodríguez-Soalleiro, R. (2005). Modeling dominant
#' height growth of radiata pine (Pinus radiata D. Don) plantations in north-western Spain.
#' Forest Ecology and Management. Vol. 215. #1-3. pp. 271-284.
#' <https://doi.org/10.1016/j.foreco.2005.05.015>
#'
#' @param dominant_height numeric. Dominant height of stand in metres.
#' @param stand_age numeric. Stand age, years from planting.
#' @param ref_age numeric. Reference age. Age at output.
#'
#' @return numeric. Height at reference age in metres.
#' @export
#'
#' @examples
Allen_2020_height_development_Norway_spruce <- function(dominant_height,
                                                        stand_age,
                                                        ref_age){

  beta1 <- 0.01605
  beta2 <- 0.61208
  beta3 <- 4.43722

  L <- log(1 - exp(-beta1*stand_age))

  X0 <- 0.5*((log(dominant_height) + beta2*L) + sqrt((log(dominant_height) - beta2*L)^2 - (4*beta3*L)))

  H2 <- dominant_height*((1-exp(-beta1*ref_age))/(1-exp(-beta1*stand_age)))^((beta2+beta3)/X0)

  return(H2)

}
