#' Form height for spruce stands in Eriksson 1976
#'
#' @source From p. 78 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @param dominant_height_dm Dominant height, dm.
#' @param site_index Site index, metres.
#' @param diameter_of_mean_basal_area_over_bark_cm Diameter corresponding to mean basal area over bark, cm.
#'
#' @return Form Height
#' @export
#'
#' @examples
Eriksson_1976_form_height <- function(dominant_height_dm, site_index, diameter_of_mean_basal_area_over_bark_cm){

  if(site_index<=17.9){
    #G16
    b1 <- 0.835

  } else if(site_index<=21.9){
    #G20
    b1 <- 0.833

  } else if(site_index<=25.9){
    #G24
    b1 <- 0.838

  } else if(site_index<=29.9){
    #G28
    b1 <- 0.838

  } else if(site_index<=33.9){
    #G32
    b1 <-  0.839

  } else if(site_index>=34){
  #G36
  b1 <- 0.844
  }

  return(
    (10^(-1.141))*(dominant_height_dm^b1)*(diameter_of_mean_basal_area_over_bark_cm^0.123)
  )



}
