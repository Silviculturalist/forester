#' Individual tree growth models of Norway spruce, Scots Pine, Birch and other broadleaves in Norway.
#' @source Andreassen, K., Tomter, S.M. (2003) Basal area growth models for
#' individual trees of Norway Spruce, Scots pine, birch and other broadleaves
#' in Norway. For. Ecol. Manage. 180:1-3. 11-24. ISSN 0378-1127. DOI:
#' \url{https://doi.org/10.1016/S0378-1127(02)00560-1}
#' @description Models based on 5.64 m radius plots (100 m^2).
#' Mean inventory period of 5.4 years.
#' @param basal_area_m2 Basal area of the tree, in m^2.
#' @param diameter_cm Diameter at breast height 1.3 m in cm.
#' @param BA_m2_ha Basal area of the plot, in m^2 / ha
#' @param BA_m2_ha_Spruce Basal area Spruce, in m^2 / ha
#' @param BA_m2_ha_Pine Basal area Pine, in m^2 / ha
#' @param BA_m2_ha_Broadleaves Basal area Broadleaves, in m^2 / ha
#' @param mean_height_Lorey_m Lorey's Mean Height for the plot.
#' @param DGV_cm Basal area weighted mean diameter, in cm. (To do: Check with authors)
#' @param stems_ha Number of stems per hectare.
#' @param SI40_Spruce Site Index H40 for Spruce. (will override SI40_Pine if not NULL (default))
#' @param SI40_Pine Site Index H40 for Scots Pine.
#' @param latitude Latitude, decimal.
#' @param altitude Meters above sea level.
#' @param even_aged TRUE if even-aged stand (one-layered). FALSE if uneven-aged stand (multi-layered)
#'@export
#'@name andreassen_2003
#'

#' @rdname andreassen_2003
#' @inheritParams andreassen_2003
#' @details
#' @export

Andreasssen_2003_BA_increment_Norway_Norway_Spruce_SI <- function(
  basal_area_m2,
  diameter_cm,
  BA_m2_ha,
  BA_m2_ha_Spruce,
  #BA_m2_ha_Pine,
  BA_m2_ha_Broadleaves,
  mean_height_Lorey_m,
  DGV_cm,
  stems_ha,
  SI40_Spruce=NULL,
  SI40_Pine=NULL,
  #latitude,
  #altitude,
  even_aged=TRUE
  #spruce_presence,
  #pine_presence,
  #birch_presence
){

  if(!is.null(SI40_Spruce)) SI40_Pine <- NULL
  if(!is.null(SI40_Pine)) SI40_Spruce <- NULL
  #if(is.null(SI40_Spruce) & is.null(SI40_Pine)) stop("Either SI40_Spruce or SI40_Pine must be given")
  #stopifnot(is.logical(c(spruce_presence,pine_presence,birch_presence)))
  stopifnot(is.logical(even_aged))


  #Multiply all proportions by 10 to ensure sum of proportions == 10.
  BA_quotient_Spruce <- (BA_m2_ha_Spruce / BA_m2_ha)*10
  #BA_quotient_Pine <- (BA_m2_ha_Pine / BA_m2_ha)*10
  BA_quotient_Broadleaves <- (BA_m2_ha_Broadleaves / BA_m2_ha)*10

  #Set missing vars to zero for simplicity with model components.
  latitude <- 0
  altitude <- 0
  BA_quotient_Pine <- 0
  pine_presence <- 0
  spruce_presence <- 0
  birch_presence <- 0


  #Follows order of table 4.
  a <- -0.8706
  b1 <- 0.7069
  b2 <- -0.0975 * 10^-3
  b3 <- 0.0405 * 10^-6
  c5 <- 0.2232
  c3 <- 0.302 * 10^-2
  c2 <- -0.750 * 10^-2
  c1 <- -0.0106
  c4 <- -0.123* 10^-3
  d1 <- 0.09337
  d2 <- 0.08841
  d3 <- 0
  d4 <- 0
  e4 <- -0.0122
  e5 <- 0
  e6 <- 0.0280
  e1 <- 0
  e2 <- 0
  e3 <- 0
  e7 <- 0.0043


  size <- b1 * log(basal_area_m2*10^4) + b2 * (basal_area_m2*10^4) + b3 * ((basal_area_m2*10^4)^2)
  competition <- c1 * BA_m2_ha + c2 * (mean_height_Lorey_m * 10) + c3 * (DGV_cm*10) + c4 * stems_ha + c5*(diameter_cm/DGV_cm)
  #Latitude decimal, as opposed to in paper.
  site <- d1 * SI40_Spruce + d2 * SI40_Pine + d3 * (latitude*10) + d4 * altitude
  stand <- e1*spruce_presence + e2*pine_presence + e3*birch_presence + e4*BA_quotient_Spruce + e5*BA_quotient_Pine + e6*BA_quotient_Broadleaves + e7*even_aged

  return(
    exp(a + size + competition + site + stand)/10^4 # cm^2 to m^2
  )


}
