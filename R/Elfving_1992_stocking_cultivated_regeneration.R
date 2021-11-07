#' Stocking (0-1) in natural regenerations from Elfving 1992.
#'
#' @description OBSERVE! Plausible retransformation error?
#' Distance from equator calculated as 111 km * latitude.
#' Jämtland county has been interpreted as both Jämtland - Härjedalens landskap and Jämtland - Jämtlands landskap.
#'
#'OBSERVE! This function includes the modifications to coefficients as defined on p. 6 in the source:
#' \emph{Increased efficiency in provenance selection and scarification has also been considered by assumed influence on the cofficients. For cultivations the negative effect by increasing height above see level has been reduced from -0.0514 to -0.0257 and the positive effect by scarification has been increased from 0.0757 to 0.2.}
#'
#'
#' @details
#'
#' n = 3528
#'
#' R^2 = 0.492
#'
#' s-res = 0.391
#'
#' Mean of dependents (?): 2.3059
#'
#' @source Elfving, B. (1992). Återväxtens etablering och utveckling till röjningstidspunkten. SLU, Institutionen för Skogsskötsel. Arbetsrapport nr. 67.
#' Available, p.5 : \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param latitude Latitude north, decimal degrees.
#' @param altitude Meters above sea level.
#' @param demanded_seedlings_per_ha Number of seedlings demanded for full stocking. Default 2500 as per implementation.
#' @param spacing Average square spacing used at sowing or planting.
#' @param growth_periods_since_cut Number of growth periods since the cut. Default 12 as per implementation.
#' @param county County, e.g. [forester::county_sweden()]
#' @param sown TRUE, if the site has been sown. Otherwise FALSE.
#' @param spruce TRUE, if the site has been cultivated with Spruce. Otherwise FALSE.
#' @param scarification TRUE if site has been scarified, otherwise FALSE.
#' @param prescribed_burning TRUE if a prescribed burn has been done after the cut, otherwise FALSE.
#' @param Jonson_site_class Site class according to Jonson 1914. e.g. [forester::Hagglund_1981_si_to_bonitet()] and  [forester::Jonson_1914_MAI_to_class()]
#'
#' @return Stocking, varying between 0-1.
#' @export
#'
#' @examples
Elfving_1992_stocking_cultivated_regeneration <- function(
  latitude,
  altitude,
  demanded_seedlings_per_ha=2500,
  spacing,
  growth_periods_since_cut=12,
  county,
  sown,
  spruce,
  scarification,
  prescribed_burning,
  Jonson_site_class
){
  stopifnot(sown%in%c(TRUE,FALSE,1,0))
  stopifnot(spruce%in%c(TRUE,FALSE,1,0))
  stopifnot(scarification%in%c(TRUE,FALSE,1,0))
  stopifnot(prescribed_burning%in%c(TRUE,FALSE,1,0))
  stopifnot(Jonson_site_class%in%1:9)
  stopifnot(spacing>=0)
  stopifnot(demanded_seedlings_per_ha>=0)
  if(altitude<0){warning("Altitude is less than 0 metres above sea level.")}

  demanded_seedlings_per_ha <- demanded_seedlings_per_ha/1000

  orebro <- ifelse(county=="Örebro",1,0)
  northern <- ifelse(latitude>60,1,0)

  temp <- 3.0707+
    +0.4358*(1/growth_periods_since_cut)+
    -0.0614*Jonson_site_class+
    #-0.0514*((altitude/100)^2)*northern+
    -0.0257*((altitude/100)^2)*northern+
    -0.3591*spacing+
    +0.0760*spruce+
    +0.1141*prescribed_burning+
    #+0.0757*scarification+
    +0.2*scarification+
    -0.0675*sown+
    +0.2597*northern+
    +4.7901*(1/((((111*latitude)-6050)/50)))+
    +0.2178*orebro+
    -0.1500*demanded_seedlings_per_ha

  SLH <- (0.037 + 0.926*(sin(temp/2)*sin(temp/2))) #Bias corrected and retransformed value.

  W <- sin(-0.058 + 1.380*temp - 0.315*temp^2 - 0.031*northern)*sin(-0.058 + 1.380*temp - 0.315*temp^2 - 0.031*northern)

  return(
  list(
    "Stocking Level"=SLH,
    "Young stand quality W"=W
  )
  )

  #Note: Is retransformation really correct?



}
