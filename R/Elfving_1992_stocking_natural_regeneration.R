#' Stocking (0-1) in natural regenerations from Elfving 1992.
#'
#' @description OBSERVE! Plausible retransformation error?
#' Distance from equator calculated as 111 km * latitude.
#' Jämtland county has been interpreted as both Jämtland - Härjedalens landskap and Jämtland - Jämtlands landskap.
#'
#'OBSERVE! This function includes the modifications to coefficients as defined on p. 6 in the source:
#' \emph{For natural regenerations the effect by scarification has been modified to 0.30 on mesic sites and 0.15 on other sites.}
#'
#'
#' @details
#'
#' n = 2462
#'
#' R^2 = 0.492
#'
#' s-res = 0.475
#'
#' Mean of dependents (?): 1.8585
#'
#' @source Elfving, B. (1992). Återväxtens etablering och utveckling till röjningstidspunkten. SLU, Institutionen för Skogsskötsel. Arbetsrapport nr. 67.
#' Available, p.5 : \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param latitude Latitude north, decimal degrees.
#' @param altitude Meters above sea level.
#' @param proportion_cultivated Proportion of cultivated seedlings according to inventory.
#' @param demanded_seedlings_per_ha Number of seedlings demanded for full stocking. Default 2500 as per implementation.
#' @param growth_periods_since_cut Number of growth periods since the cut. Default 12 as per implementation.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param county County, e.g. [forester::county_sweden()]
#' @param scarification TRUE if site has been scarified, otherwise FALSE.
#' @param prescribed_burning TRUE if a prescribed burn has been done after the cut, otherwise FALSE.
#' @param untreated TRUE if there were no measures done to promote regeneration after the final felling, otherwise FALSE.
#' @param uncleaned TRUE if bushes and small trees remain after clear-felling, otherwise FALSE.
#' @param seed_trees_per_ha Number of seed trees per hectare.
#' @param Jonson_site_class Site class according to Jonson 1914. e.g. [forester::Hagglund_1981_si_to_bonitet()] and  [forester::Jonson_1914_MAI_to_class()]
#' @param area Area of site in hectares.
#'
#' @return A list with elements (1) : Stocking, varying between 0-1, and (2): Young stand quality W.
#' @export
Elfving_1992_stocking_natural_regeneration <- function(
  latitude,
  altitude,
  proportion_cultivated,
  demanded_seedlings_per_ha=2500,
  growth_periods_since_cut=12,
  soil_moisture,
  county,
  scarification,
  prescribed_burning,
  untreated,
  uncleaned,
  seed_trees_per_ha,
  Jonson_site_class,
  area
){
  stopifnot(scarification%in%c(TRUE,FALSE,1,0))
  stopifnot(prescribed_burning%in%c(TRUE,FALSE,1,0))
  stopifnot(untreated%in%c(TRUE,FALSE,1,0))
  stopifnot(uncleaned%in%c(TRUE,FALSE,1,0))
  stopifnot(Jonson_site_class%in%1:9)
  stopifnot(area>=0)
  stopifnot(demanded_seedlings_per_ha>=0)
  stopifnot(seed_trees_per_ha>=0)
  stopifnot(proportion_cultivated>=0 & proportion_cultivated<=1)
  if(altitude<0){warning("Altitude is less than 0 metres above sea level.")}

  if(untreated & prescribed_burning){
    stop("Argument 'untreated' cannot be TRUE at the same time as 'prescribed_burning' or 'scarification'.")
  }
  if(untreated & scarification){
    stop("Argument 'untreated' cannot be TRUE at the same time as 'prescribed_burning' or 'scarification'.")
  }



  demanded_seedlings_per_ha <- demanded_seedlings_per_ha/1000

  moist <- ifelse(soil_moisture>3,1,0)
  dry <- ifelse(soil_moisture==1,1,0)
  mesic <- ifelse(soil_moisture==2,1,0)
  not_mesic <- ifelse(soil_moisture!=2,1,0)
  gotland <- ifelse(county=="Gotland",1,0)
  orebro <- ifelse(county=="Örebro",1,0)
  SYZ <- ifelse(county%in%c("Värmland","Västernorrland - Medelpads landskap","Jämtland - Jämtlands landskap", "Jämtland - Härjedalens landskap"),1,0)
  northern <- ifelse(latitude>60,1,0)
  eq_distance <- 111*latitude

  temp <- 1.7413+
    -0.0163*(((altitude/100)^2)*northern)+
    +0.6863*(2*(1/((1+exp(-0.3*growth_periods_since_cut))^-0.5)))+
    +0.6663*proportion_cultivated+
    -0.1500*demanded_seedlings_per_ha+
    +0.0218*(((111*latitude)-6050)/50)*dry+
    +0.2702*moist+
    -0.7552*gotland+
    +0.3310*orebro+
    -0.1275*SYZ+
    #+0.2692*scarification+ This variable changed.
    +0.30*scarification*mesic+ #modification p. 6
    +0.15*scarification*not_mesic+ # modification p. 6
    +0.2030*prescribed_burning+
    -0.1484*untreated+
    -0.0947*uncleaned+
    +0.1596*seed_trees_per_ha+
    +0.0175*(((111*latitude)-6050)/50)*seed_trees_per_ha+
    -0.0379*Jonson_site_class+
    +0.1888*(1/area)+
    +0.1075*northern+
    -0.00619*(((111*latitude)-6050)/50)

  SLH <- (0.056 + 0.887*(sin(temp/2)*sin(temp/2))) #Bias corrected and retransformed value.

  W <- sin(-0.11 + 1.671*temp - 0.583*temp^2)*sin(-0.11 + 1.671*temp - 0.583*temp^2)

  return(
  list(
    "Stocking Level"= SLH,
    "Young stand quality W" = W
  )
  )

  #Note: Is retransformation really correct?



}

