#' Volume estimation of trees
#'
#' @description Provides interface for several functions from the collection of individual tree volume functions in  "\emph{Fältdatasystem för skogliga fältförsökö}" (2012),
#' by Karlsson, K., Ekö, Mossberg, M., Ulvcrona, T.
#'
#'     \strong{If used on array, must use dplyr::rowwise()}
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
#'     If European/Siberian larch... >= 5 cm dbh: \strong{Carbonnier, 1954}
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
#'     If Larix sibirica... >=5 cm dbh: \strong{Carbonnier, 1954}.B
#'
#'
#' @param species.latin One of "Picea ...", "Pinus sylvestris", "Larix sibirica", "Abies ...".
#' @param dbh.cm Diameter breast height \strong{(1.3 m)}, in cm.
#' @param height.m Tree height in metres.
#' @param crown.base.height.m The lowest height of the living tree crown, in metres.
#' @param double.bark.mm The sum of bark thickness on polar ends of the diameter as measured at 1.3m height, in millimetres.
#' @param latitude The latitudinal placement of the tree, in degrees N.
#'
#' @return Volume of the tree, in \strong{dm^3}
#' @export
#'
#' @examples
#' tree.volume(species.latin=SPP, height.m=height, dbh.cm=dbh, crown.base.height.m=cbhm, double.bark.mm=dbmm, latitude=latitude)



#Volume estimating function


tree.volume <- function(dbh.cm,height.m,crown.base.height.m,double.bark.mm,species.latin,latitude){
  if(grepl("^Pinus", ignore.case=TRUE, species.latin)){
    pinus_spp_vol(species.latin = species.latin,
                  dbh.cm = dbh.cm,
                  height.m = height.m,
                  latitude = latitude,
                  double.bark.mm = double.bark.mm,
                  crown.base.height.m = crown.base.height.m)
  } else if(grepl("^Picea|^Abies",ignore.case = TRUE, species.latin)){
    picea_spp_vol(dbh.cm = dbh.cm,
                  height.m = height.m,
                  latitude = latitude,
                  crown.base.height.m = crown.base.height.m)
  } else if(grepl("^Larix", ignore.case = TRUE, species.latin)){
    larix_sibirica_vol_carbonnier_1954(dbh.cm = dbh.cm,
                                       height.m = height.m,
                                       double.bark.mm = double.bark.mm,
                                       crown.base.height.m = crown.base.height.m)
  }

}
