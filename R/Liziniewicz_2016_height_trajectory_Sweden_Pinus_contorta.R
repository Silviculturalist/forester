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
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return If output is "SIH100", the numeric value for the height (m) a stand
#' will reach at age 100.
#'
#'  If output is "Height", the numeric value for the height (m) a stand will
#'  reach at age_2.
#'
#'  If output is "Equation", a named list with 2 elements: I) "Equation": text response with the equation for that height
#'  curve.
Liziniewicz_2016_height_trajectory_Sweden_Pinus_contorta <- function(
  dominant_height,
  age,
  age2,
  output="Height"
)
{

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  message("Suitable for cultivated stands of Pinus contorta above 59 degrees N, between ages 20 and 50.")

paramasi <- 25
parambeta <- 8016.54
paramb2 <- -1.6986

d <- parambeta*(paramasi^paramb2)

r <- (((dominant_height-d)^2)+(4*parambeta*dominant_height*(age^paramb2)))^0.5


if(output=="SIH100"){
  return(
    (dominant_height+d+r)/ (2+(4*parambeta*(100^paramb2)) / (dominant_height-d+r))
  )
}

if(output=="Equation"){
  return(
    paste0("y ~",(dominant_height+d+r),"/(2+(4*",parambeta,"*(age^",paramb2,")) / ",(dominant_height-d+r),")")
  )
}

if(output=="Height"){
  return(
    ((dominant_height+d+r)/ (2+(4*parambeta*(age2^paramb2)) / (dominant_height-d+r)))
  )
}

}
