#' Picea volume
#' @description Background function for tree.vol.sweden.
#'
#' @param species.latin One of "Picea ...", "Pinus sylvestris", "Larix sibirica", "Abies ...".
#' @param dbh.cm Diameter breast height \strong{(1.3 m)}, in cm.
#' @param height.m Tree height in metres.
#' @param crown.base.height.m The lowest height of the living tree crown, in metres.
#' @param double.bark.mm The sum of bark thickness on polar ends of the diameter as measured at 1.3m height, in millimetres.
#' @param latitude The latitudinal placement of the tree, in degrees N.
#' @return Volume of the tree, in dm3
#' @export
#'
#' @examples
#' picea_spp_vol(height.m=3, dbh.cm=4, latitude=61)

picea_spp_vol <- function(height.m, dbh.cm, latitude, crown.base.height.m){
  if(missing(crown.base.height.m)==FALSE){
    if(dbh.cm >= 4.5){ #Brandel, 1990 function 100-02 for spruce above 4.5 cm dbh.
        if(latitude >= 60){
          (10^-0.66277) * (dbh.cm^2.16277) * ((dbh.cm+20)^-0.81628) * (height.m^2.92136) * ((height.m-1.3)^-1.71059) * (crown.base.height.m^0.04501)
        } else {
          (10^-0.93173) * (dbh.cm^2.06103) * ((dbh.cm+20)^-0.51644) * (height.m^2.66914) * ((height.m-1.3)^-1.51878) * (crown.base.height.m^0.04291)
        }
      } else {
        if(latitude >= 60){ #Andersson, 1954 for small trees.
          0.22 + 0.1150*(dbh.cm^2) + 0.01410*((dbh.cm^2)*height.m) + 0.01047*dbh.cm*(height.m^2)
        } else {
          0.22 + 0.1086*(dbh.cm^2) + 0.01712*((dbh.cm^2)*height.m) + 0.008905*dbh.cm*(height.m^2)
        }
      }

  } else if(missing(crown.base.height.m)==TRUE){
      if(dbh.cm >= 4.5){#Brandel, 1990, 100-01
        if(latitude >= 60){
          (10^-0.79783)*(dbh.cm^2.07157)*((dbh.cm+20)^-0.73882)*(height.m^3.16332)*((height.m-1.3)^-1.82622)
        } else {
          (10^-1.02039)*(dbh.cm^2.00128)*((dbh.cm+20)^-0.47473)*(height.m^2.87138)*((height.m-1.3)^-1.61803)
        }
      } else {
        if(latitude >= 60){ #Andersson, 1954 for small trees.
          0.22 + 0.1150*(dbh.cm^2) + 0.01410*((dbh.cm^2)*height.m) + 0.01047*dbh.cm*(height.m^2)
        } else {
          0.22 + 0.1086*(dbh.cm^2) + 0.01712*((dbh.cm^2)*height.m) + 0.008905*dbh.cm*(height.m^2)
      }
    }
  }
}
