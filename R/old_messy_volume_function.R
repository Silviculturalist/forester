#' Old messy volume function.
#' @description Provides interface for several functions from the collection of individual tree volume functions in  "\emph{Fältdatasystem för skogliga fältförsökö}" (2012),
#' by Karlsson, K., Ekö, Mossberg, M., Ulvcrona, T.
#'
#'     \strong{Supported species}: Picea spp. ; Abies spp. ;  Pinus sylvestris ; Larix sibirica; Pinus contorta; Picea abies
#'
#'     \strong{If diameter double bark is available}
#'     Will use a function following:
#'
#'     If Pinus sylvestris, larger than 4.5 cm dbh: \strong{Brandel, 1990: function 100-04}
#'
#'     If Pinus sylvestris, below 4.5 cm dbh :\strong{Andersson, S-O, 1954}
#'
#'     If Picea abies (Picea spp., Abies spp.), larger than 4.5 cm dbh : \strong{Brandel. 1990: function 100-02}
#'
#'     If Picea abies (Picea spp., Abies spp.), smaller than 4.5 cm dbh: \strong{Andersson, S-O, 1954}
#'
#'     If European/Siberian larch: \strong{Carbonnier, 1954}
#'
#'     If Pinus contorta: \strong{Eriksson, 1973}
#'
#'
#'     \strong{If diameter double bark is Not available}
#'     Will use a function following:
#'
#'     If Pinus...  larger than 4.5 cm dbh, \strong{Brandel 1990: 100-01}
#'
#'     If Pinus... below 4.5 cm dbh, \strong{Andersson, S-O, 1954}
#'
#'     If Picea/Abies... larger than 4.5 cm dhb, \strong{Brandel 1990: 100-01}
#'
#'     If Picea/Abies... below 4.5 cm dbh, \strong{Andersson, S-O, 1954}
#'
#'     If Larix sibirica: \strong{Carbonnier, 1954}
#'
#'
#' @param data Tidy data frame.
#' @param species.latin One of "Picea ...", "Pinus sylvestris", "Larix sibirica", "Abies ...".
#' @param dbh.cm Diameter breast height \strong{(1.3 m)}, in cm.
#' @param height.m Tree height in metres.
#' @param crown.base.height.m The lowest height of the living tree crown, in metres.
#' @param double.bark.mm The sum of bark thickness on polar ends of the diameter as measured at 1.3m height, in millimetres.
#' @param latitude The latitudinal placement of the tree, in degrees N.
#'
#' @return Volume of the tree, in \strong{dm^3}
#'
#' @examples
#' old.messy.volume.function(df=df, species.latin=SPP, height.m=height, dbh.cm=dbh, crown.base.height.m=cbhm, double.bark.mm=dbmm, latitude=latitude)



#Volume estimating function


old.messy.volume.function <- function(dbh.cm,height.m,crown.base.height.m,double.bark.mm,species.latin,latitude){
  if(missing(double.bark.mm)==FALSE){
  if(species.latin=="Pinus sylvestris"){
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
  } else if(grepl("^Picea|^Abies", species.latin)){ # For Spruce. Also for all picea and abies.
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
  } else if(species.latin== "Larix sibirica"){ #Carbonnier 1954
    0.04588*(dbh.cm^2)*height.m + 0.09006*(dbh.cm^2) - 0.008844*(dbh.cm^3) - 0.06460*dbh.cm*height.m + 0.1488*height.m - 0.001391*dbh.cm*height.m*double.bark.mm + 0.002654*(dbh.cm^2)*dbh.cm*crown.base.height.m
  } else if(species.latin== "Pinus contorta"){ #Eriksson 1973
    0.1121*(dbh.cm^2) + 0.02870*(dbh.cm^2)*height.m - 0.000061*(dbh.cm^2)*(height.m^2) - 0.09176*dbh.cm*height.m + 0.01249*dbh.cm*(height.m^2)
  }
  } else if (missing(double.bark.mm)==TRUE){
    if(grepl("^Pinus", species.latin)){
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
      } else if (grepl("^Picea|^Abies", species.latin)){ #For Spruce, Picea spp., Abies spp.
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
      } else if(species.latin=="Larix sibirica"){ #Carbonnier, 1954
      (0.04801*(dbh.cm^2)*height.m) + (0.08886*(dbh.cm^2)) - (0.01012*(dbh.cm^3)) - (0.08406*dbh.cm*height.m)+(0.1972*height.m)
    }
  }

}
