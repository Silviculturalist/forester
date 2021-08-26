#' Height trajectory for Grey Alder, Alnus incana, in Sweden from Johansson 1999.
#'
#' @source Johansson, T. (1999) Site index Curves for Common Alder and Grey Alder Growing on Different
#' Types of Forest Soil in Sweden. Scandinavian Journal of Forest Research. Vol 14:5. p.441-453. Available: \url{https://doi.org/10.1080/02827589950154140}
#'
#' @description
#'
#' Abstract:
#'
#' "Growth data were collected from 32 stands of common alder and 26 stands of
#'  grey alder growing on forest land in Sweden. The stands ranged in latitude
#'  from 56 to 60° N and from 58 to 63° N. The mean age of common and grey
#'  alder stands was 48 yrs (range 21 - 91) and 40 yrs (range 21 - 66),
#'  respectively, the mean stand density 1174 stems ha?1 (range 440 - 2994)
#'  and 2140 stems ha?1 (range 591 - 5120), and the mean diameter at breast
#'  height (on bark) 21 cm (range 13 - 27 cm) and 17 cm (range 9 - 25 cm).
#'  Site index curves were constructed for total age. Curves for H40
#'  (dominant height at 40 yrs total age) were constructed using data
#'  from all stands investigated. Curves fitted for H40 total age of
#'  common alder were found to have the same shape as curves developed
#'  for German conditions. Site index curves for grey alder show higher
#'  growth for middle - aged trees compared with curves from Norwegian
#'  stands. Older Finnish site index curves for grey alder were found
#'  to have the same shape as curves constructed in the present study.
#'  Soil types in the common alder stands were divided into four groups:
#'  moorland peat/light clay (n=5), moorland peat/sand (5),
#'  cambisol/sand (4) and fine sand (4). For grey alder stands
#'  the soils were divided into three groups: silt (4), sandy till (3)
#'  and cambisol/light clay (3). Soil types represented by only one or
#'  two stands were not included in the analysis. No statistically
#'  significant differences in site index were found between the four
#'  soil - type groups in common alder stands or between the three soil
#'  - type groups in grey alder stands. Some recommendations for the
#'  management of common and grey alder stands are given."
#'
#'
#'
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return
#' @export
#'
#' @examples
Johansson_1999_height_trajectory_Sweden_Alnus_incana <- function(
  dominant_height,
  age,
  age2,
  output
){
  if(missing(output)){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }


  if(age>70|age2>70){
    warning(
      "Suitable for stands of Grey Alder under age of 70."
    )
  }

  paramasi <- 7
  parambeta <- 278.9
  paramb2 <- -1.3152

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
