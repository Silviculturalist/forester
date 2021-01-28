#' Volume estimation of Larix sibirica
#' @description Interface for Carbonnier 1954 function for volume of Larix sibirica
#'
#' @param dbh.cm Diameter breast height (1.3 m) in centimetres.
#' @param height.m Height of tree, in metres.
#' @param double.bark.mm Double bark, in mm.
#' @param crown.base.height.m Height of crown base, in metres.
#' @return Tree volume
#'
#' @details \strong{If double.bark.mm & crown.base.height.m available, will use:}
#'
#'     0.04588*(dbh.cm^2)*height.m + 0.09006*(dbh.cm^2) - 0.008844*(dbh.cm^3) - 0.06460*dbh.cm*height.m + 0.1488*height.m - 0.001391*dbh.cm*height.m*double.bark.mm + 0.002654*(dbh.cm^2)*dbh.cm*crown.base.height.m
#'
#'     \strong{otherwise:}
#'
#'    (0.04801*(dbh.cm^2)*height.m) + (0.08886*(dbh.cm^2)) - (0.01012*(dbh.cm^3)) - (0.08406*dbh.cm*height.m)+(0.1972*height.m)
#' @export
#'
#' @examples
#' larix_sibirica_vol_carbonnier_1954(dbh.cm=10, height=10, double.bark.mm = 9, crown.base.height.m = 5)
#' larix_sibirica_vol_carbonnier_1954(dbh.cm=10, height=10)

larix_sibirica_vol_carbonnier_1954 <- function(dbh.cm, height.m, double.bark.mm, crown.base.height.m){
  if(!missing(double.bark.mm) & !missing(crown.base.height.m) & dbh.cm >= 5){
    0.04588*(dbh.cm^2)*height.m + 0.09006*(dbh.cm^2) - 0.008844*(dbh.cm^3) - 0.06460*dbh.cm*height.m + 0.1488*height.m - 0.001391*dbh.cm*height.m*double.bark.mm + 0.002654*(dbh.cm^2)*dbh.cm*crown.base.height.m
  } else if(dbh.cm >=5){
   (0.04801*(dbh.cm^2)*height.m) + (0.08886*(dbh.cm^2)) - (0.01012*(dbh.cm^3)) - (0.08406*dbh.cm*height.m)+(0.1972*height.m)

  } else{
    return(NA_real_)
  }


}
