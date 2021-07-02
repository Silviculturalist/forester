#' Find Site Index H100 for Norway Spruce or Scots Pine in Sweden by Intercept method
#'
#' @description Demands:
#'
#' - The stand shall be a Scots Pine or Norway Spruce stand with maximum 20 \% of basal area composed of other species.
#'
#' - The stand must be even-aged.
#'
#' - The stand must not have been hindered in its' development.
#'
#' - The trees used to determine the intercept must not have had serious injuries, for example stem breakage.
#'
#' - The stand must not have been high-graded.
#'
#' - The stand must not have been fertilised before the five shoots included in the intercept.
#'
#' - It must be possible to clearly distinguish the five shoots of interest. This can be tricky for Norway spruce.
#'
#' - Obviously, the stand must have reached a level of development where five shoots exceeding 2.5 metres must exist.
#'
#' - The stand must not be too old - naturally hindered by self-pruning.
#'
#' - The stand must not be too old - it will at a certain point no longer be certain that the current dominant trees were previously dominant.
#'
#' - By age 35 at breast height or higher, determining site index by height functions will perform better.
#'
#' @details Standard deviation for the prognosis error was estimated to 16 dm for Scots Pine, and 18 dm for Norway Spruce.
#' In general the method is clearly less precise in Spruce stands than in Pine stands (p.54).
#'
#' @source Hägglund, B. (1976) "Skattning av höjdboniteten i unga tall- och granbestånd:
#' Estimating site index in young stands of Scots pine and Norway spruce in Sweden. Report #39. Royal College of Forestry. Stockholm.
#' p.36
#'
#' @param species "Picea abies" or "Pinus sylvestris"
#' @param intercept_2.5_5.m Measured distance, in metres. from the first branch whorl above 2.5 metres height to the fifth branch whorl above (height increment from five growing seasons)
#' @param vegetation Variable indicating vegetation type (NFI vegetation code) scaled from -5 to +4 as follows:
#'
#' \tabular{llr}{
#' Field Layer Code (NFI) \tab Description \tab Index \cr
#' 1\tab Tall herbs w/o dwarf shrubs \tab 4 \cr
#' 2\tab Tall herbs with bilberry \tab 2.5 \cr
#' 3\tab Tall herbs with cowberry \tab 2 \cr
#' 4\tab Low herbs w/o dwarf shrubs \tab 3 \cr
#' 5\tab Low herbs with bilberry \tab 2.5 \cr
#' 6\tab Low herbs with cowberry \tab 2 \cr
#' 7\tab No field layer \tab 3 \cr
#' 8\tab broad-leafed grasses \tab 2.5 \cr
#' 9\tab narrow-leaved grasses \tab 1.5 \cr
#' 10\tab Sedge, tall \tab -3 \cr
#' 11\tab Sedge, low \tab -3 \cr
#' 12\tab Horsetail \tab 1 \cr
#' 13\tab Bilberry \tab 0 \cr
#' 14\tab Cowberry \tab -0.5 \cr
#' 15\tab Crowberry \tab -3 \cr
#' 16\tab Poor shrub \tab -5 \cr
#' 17\tab Lichen-rich \tab -0.5 \cr
#' 18\tab Lichen-dominated \tab -1 \cr
#' }
#' @param lateral_water Type 1="Missing", 2="Seldom",3="shorter periods",4="longer periods",5="slope".
#' @param latitude Degrees. Only required when species is "Picea abies".
#' @param altitude Metres above sea level.
#'
#' @return SI H100
#' @export
#'
#' @examples
Hagglund_1976_intercept_SI <- function(
  species,
  intercept_2.5_5.m,
  vegetation,
  lateral_water,
  latitude,
  altitude
){

  intercept_2.5_5.m <- intercept_2.5_5.m*10

  altitude <- altitude/100

  if(vegetation>=13 && lateral_water%in%c(1,2)){
    class_5_7 <- 1
  } else {
    class_5_7 <- 0
  }

  if(species=="Pinus sylvestris"){
    return(
      round(
      (exp(
        3.89460+ #including correction for logarithmic bias.
          0.51353*log(intercept_2.5_5.m)+
          -0.00119*(altitude^2)+
          -0.04723*class_5_7
      )/10)
      ,1)
    )
  } else if(species=="Picea abies"){
    return(
      round(
      (exp(
        4.37165+ #including correction for logarithmic bias.
          -0.00818*intercept_2.5_5.m+
          +0.68641*log(intercept_2.5_5.m)+
          -0.01150*latitude+
          -0.00613*(altitude^2)+
          -0.03808*class_5_7
      )/10)
      ,1)
    )
  }

}
