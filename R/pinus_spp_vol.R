#' Pinus spp volume
#' @description Volume of Pinus spp.
#'
#' Uses Brandel 1990 100-4 for all Pinus above 4.5 cm, for P. contorta 100-1.
#' For tree under 4.5 cm, Andersson 1954.
#'
#' @param species.latin Species name in latin, e.g. "Pinus sylvestris"
#' @param latitude Latitude.
#' @param dbh.cm Diameter breast height in centimetres.
#' @param height.m Height in metres.
#' @param double.bark.mm Double bark, in mm.
#' @param crown.base.height.m Height of crown base, in metres.
#' @return Tree volume
#' @export
#'
#' @examples
#' pinus_spp_volume(species.latin="Pinus sylvestris", dbh.cm=10, height.m=10,latitude=63)

pinus_spp_vol <- function(species.latin, dbh.cm, height.m, crown.base.height.m, double.bark.mm, latitude){
  if(!missing(crown.base.height.m)){
    if(!missing(double.bark.mm)){
      if(species.latin!="Pinus contorta"){
        if(dbh.cm >= 4.5){ #Brandel, 1990 function 100-4. for Pines above 4.5 cm dbh.
          if(latitude >= 60){
            (10^-1.12715)*(dbh.cm^2.13211)*((dbh.cm+20)^-0.13543)*(height.m^1.58121)*((height.m-1.3)^-0.73435)*(crown.base.height.m^0.06595)*(double.bark.mm^-0.10998)
          } else {
            (10^-1.20042)*(dbh.cm^2.10263)*((dbh.cm+20)^-0.07366)*(height.m^1.99751)*((height.m-1.3)^-1.11357)*(crown.base.height.m^0.06420)*(double.bark.mm^-0.14963)
          }
        } else { #Andersson,1954 for small trees.
          if(latitude >= 60){
            0.22 + 0.08786 * (dbh.cm^2) + 0.03045 * (dbh.cm^2) * height.m + 0.002809 * dbh.cm * (height.m^2)
          } else {
            0.22 + 0.1066 * (dbh.cm^2) + 0.02085 * (dbh.cm^2) * height.m + 0.008427 * dbh.cm * (height.m^2)
          }
        }
      }
    }
  } else {
    if(species.latin== "Pinus contorta"){ #Eriksson 1973 in Gardmo Examensarbete no limit expressed.
      0.1121*(dbh.cm^2) + 0.02870*(dbh.cm^2)*height.m - 0.000061*(dbh.cm^2)*(height.m^2) - 0.09176*dbh.cm*height.m + 0.01249*dbh.cm*(height.m^2)
    } else {
      if(dbh.cm >= 4.5){#Brandel 1990, 100-01
        if(latitude >= 60){
          (10^-1.20914)*(dbh.cm^1.94740)*((dbh.cm+20)^-0.05947)*(height.m^1.40958)*((height.m-1.3)^-0.45810)
        } else {
          (10^-1.38903)*(dbh.cm^1.84493)*((dbh.cm+20)^0.06563)*(height.m^2.02122)*((height.m-1.3)^-1.01095)
        }
      } else { #Andersson,1954 for small trees.
        if(latitude >= 60){
          0.22 + 0.08786 * (dbh.cm^2) + 0.03045 * (dbh.cm^2) * height.m + 0.002809 * dbh.cm * (height.m^2)
        } else {
          0.22 + 0.1066 * (dbh.cm^2) + 0.02085 * (dbh.cm^2) * height.m + 0.008427 * dbh.cm * (height.m^2)
        }
      }
    }
  }
}
