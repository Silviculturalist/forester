#' Coordinate to county Sweden
#'
#' @description Returns the Swedish county which the coordinate is placed in.
#'  If there is no match, county will be 'Other', and a warning will be shown.
#'
#' @param latitude Latitude, decimal
#' @param longitude Longitude, decimal
#' @param espg Reference system. Default 'WGS84': 4326. For SWEREF99TM: '3006'.
#'
#' @return Text, Swedish County
#' @export
#'
county_sweden <- function(latitude, longitude,epsg=4326){

  if(missing(latitude) | missing(longitude)) stop("Cannot calculate county, requires either County or Latitude & Longitude.")

  #Stop if lat lon are wrong for SWEREF99TM.
  if(epsg==3006 & longitude>1E6){
    stop("Latitude and Longitude have been mixed up.")
  }

  point <- sf::st_sfc(sf::st_point(x=c(longitude,latitude)))
  sf::st_crs(point) <- epsg

  #Reproject to SWEREF99TM if epsg is not 3006.
  if(epsg!=3006){
    assign(x="point",value=sf::st_transform(point,crs=3006))
  }

  county_code <- forester:::swedish_counties_1998$DLANSKOD[sf::st_within(point, forester:::swedish_counties_1998)[[1]]]

    if(county_code == 1){
      county <- "Norrbottens lappmark"
    } else if(county_code == 2){
      county <- "Norrbottens kustland"
    } else if(county_code == 3){
      county <- "Västerbottens lappmark"
    } else if(county_code == 4){
      county <- "Västerbottens kustland"
    } else if(county_code == 5){
      county <- "Jämtland - Jämtlands landskap"
    } else if(county_code == 6){
      county <- "Jämtland - Härjedalens landskap"
    } else if(county_code == 7){
      county <- "Västernorrland - Ångermanlands landskap"
    } else if(county_code == 8){
      county <- "Västernorrland - Medelpads landskap"
    } else if(county_code == 9){
      county <- "Gävleborg - Hälsinglands landskap"
    } else if(county_code == 10){
      county <- "Gävleborg - övriga"
    } else if(county_code ==11){
      county <- "Kopparberg - Sälen-Idre"
    } else if(county_code ==12){
      county <- "Kopparberg - övriga"
    } else if(county_code ==13){
      county <- "Värmland"
    } else if(county_code ==14){
      county <- "Örebro"
    } else if(county_code ==15){
      county <- "Västmanland"
    } else if(county_code ==16){
      county <- "Uppsala"
    } else if(county_code ==17){
      county <- "Stockholm"
    } else if(county_code ==18){
      county <- "Södermanland"
    } else if(county_code ==19){
      county <- "Östergötland"
    } else if(county_code ==20){
      county <- "Skaraborg"
    } else if(county_code ==21){
      county <- "Älvsborg -  Dalslands landskap"
    } else if(county_code ==22){
      county <- "Älvsborg - Västergötlands landskap"
    } else if(county_code ==23){
      county <- "Jönköping"
    } else if(county_code == 24){
      county <- "Kronoberg"
    } else if(county_code ==25){
      county <- "Kalmar"
    } else if(county_code ==26){
      county <- "Västra Götaland"
    } else if(county_code ==27){
      county <- "Halland"
    } else if(county_code ==28){
      county <- "Kristianstad"
    } else if(county_code ==29){
      county <- "Malmöhus"
    } else if(county_code ==30){
      county <- "Blekinge"
    } else if(county_code ==31){
      county <- "Gotland"
    } else {
      warning("County not identified. Many functions will not work.")
      county <- "Other"
    }

  return(county)
  }

