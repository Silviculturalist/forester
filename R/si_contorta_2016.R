#' Site Index Estimations for P. contorta in Sweden, above 59th parallel.
#' @description Provides interface for the site index function from "\emph{A site index model for lodgepole pine (Pinus contorta Dougl. var. latifolia) in northern Sweden}" (2016),
#' by Liziniewicz, M., Nilsson, U., Agestam, E., Ekö, P-M & Elfving, B.
#'
#' "Lodgepole pines have only been cultivated in Sweden
#' for around 60 years. The experimental data set, therefore, did not contain
#' much information relating to stands older than 60 years, which may have affected the models'
#' estimates and means that it is impossible to be certain about the growth patterns
#' these stands will show as they age further. However, the available growth trajectories for
#' the oldest Swedish lodgepole pine stands provide no clear evidence of slowing height growth.
#' Stands aged between 15 and 40 years are well represented in the experimental  data set".
#'
#'     \strong{Suitable ages for use by species}: P. contorta (20 - 50 yrs)
#' @param refage Reference age to estimate height at.
#' @param age Average total age in years. For birch age at breast height.
#' @param height Average Height in metres of dominant trees.
#'
#' @return Estimate of the top height of the stand at the reference age.
#' @export
#'
#' @examples
#' si_contorta(species="NorwaySpruce",age=30, height=6.2)
