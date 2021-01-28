#' Height development in young stands NYSKOG
#'
#' @description A group of functions for mean height development in young stands of scots pine, norway spruce, birch (B. pubescens), birch (B. pendula), aspen, beech, oak or pinus contorta.
#'
#' @param species One of : Pinus sylvestris, Picea abies, Betula pendula, Betula pubescens, Populus tremula, Fagus sylvatica, Quercus robur, Pinus contorta.
#' @param si Site index H100.
#' \tabular{lr}{
#' \strong{Species} \tab \strong{SI species} \cr
#' Pine    \tab Pine \cr
#' Spruce \tab Spruce \cr
#' Birch \tab Pine \cr
#' Aspen \tab Pine \cr
#' Beech \tab Beech \cr
#' Oak \tab Oak
#'
#' }
#' @param total_age
#'
#' @return Estimated height, m.
#' @export
#'
#' @examples
#' young_stand_height_development_Elfving_NYSKOG(species=species, si=si, total_age=total_age)
young_stand_height_development_Elfving_NYSKOG <- function(species, si, total_age){

  X <- log(total_age)

  if(species=="Pinus sylvestris"){
    7 + (-0.57 -0.05*si)*X + (-0.28 + 0.0094*si)*(X^2)
  } else if (species=="Picea abies"){
    (6.27 + (12.1/si)) + (-0.262 - 0.0575*si + 0.00088*(si^2))*X + (-0.323 - 0.134*(-0.262 - 0.0575*si + 0.00088*(si^2)))*(X^2)
  } else if (species=="Betula pubescens"){
    (6.836 + 0.03165*si - 0.002757*(si^2)) + (-2.694+0.4937*(6.836 + 0.03165*si - 0.002757*(si^2)) - 0.05331*((6.836 + 0.03165*si - 0.002757*(si^2))^2))*X
  } else if (species=="Betula pendula"){
    si2 <- si+1.5
    (6.836 + 0.03165*si2 - 0.002757*(si2^2)) + (-2.694+0.4937*(6.836 + 0.03165*si2 - 0.002757*(si2^2)) - 0.05331*((6.836 + 0.03165*si2 - 0.002757*(si2^2))^2))*X
  } else if (species=="Populus tremula"){
    (10.024 - 0.1664*si) + (-4.093 + 0.1605*si - 0.0025*(si^2))
  } else if (species=="Fagus sylvatica"){
    si <- 7.4 + (0.755*si) - (0.00268*(si^2))
    (6.27 + (12.1/si)) + (-0.262 - 0.0575*si + 0.00088*(si^2))*X + (-0.323 - 0.134*(-0.262 - 0.0575*si + 0.00088*(si^2)))*(X^2)
  } else if (species=="Quercus robur"){
    si <- 6.5 + 0.5*si
    (6.836 + 0.03165*si - 0.002757*(si^2)) + (-2.694+0.4937*(6.836 + 0.03165*si - 0.002757*(si^2)) - 0.05331*((6.836 + 0.03165*si - 0.002757*(si^2))^2))*X
  } else if (species=="Pinus contorta"){
    si <- 0.888 + (1.336*si) - (0.0094*(si^2))
    7 + (-0.57 -0.05*si)*X + (-0.28 + 0.0094*si)*(X^2)
  }

}
