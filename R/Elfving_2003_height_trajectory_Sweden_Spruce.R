#' Height trajectory for Norway Spruce in Sweden from Elfving 2003.
#'
#' @source Elfving B. (2003) Övre höjdens utveckling i granplanteringar. Arbetsrapporter 185:1-8. Inst. f. skogsskötsel. Swedish University of Agricultural Sciences.
#'
#' @description
#'
#' 81 Norway Spruce stands.
#'
#' 293 measurement periods.
#'
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return
#' @export
Elfving_2003_height_trajectory_Sweden_Spruce <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  if(age<10|age2<10){
    warning(
      "Suitable for cultivated stands of Norway Spruce between total ages of 10 and 80."
    )
  }

  if(age>80|age2>80){
    warning(
      "Suitable for cultivated stands of Norway Spruce between total ages of 10 and 80."
    )
  }

  paramasi <- 10
  parambeta <- 1495.3
  paramb2 <- -1.5978
  age2 <- age2-3
  age <- age-3

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
