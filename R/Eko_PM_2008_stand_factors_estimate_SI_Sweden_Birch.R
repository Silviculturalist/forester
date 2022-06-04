#' Estimate SIH50 with stand factors for Birch from Ekö et al 2008.
#'
#' @source Ekö, P-M, Johansson, U., Petersson, N. Bergqvist, J., Elfving, B., Frisk, J. (2008) Current growth differences of Norway spruce (Picea abies), Scots Pine (Pinus sylvestris) and birch (Betula pendula and Betula pubescens) in different regions in Sweden. Scandinavian Journal of Forest Research Vol. 23:4. p. 307-318. DOI: \url{https://doi.org/10.1080/02827580802249126}
#'
#' @details
#' Northern Sweden, mesic (SD=2.86,R^2=61\%)
#'
#' Northern Sweden, moist (SD=2.94,R^2=48\%)
#'
#' Southern Sweden, mesic (SD=2.95,R^2=18\%)
#'
#' Southern Sweden, moist (SD=2.88,R^2=28\%)
#'
#' @param altitude Altitude in metres above sea level.
#' @param latitude Latitude, degrees.
#' @param vegetation Vegetation type according to follows Swedish National forest inventory FALTSKIKT:
#' \tabular{ll}{
#' Code \tab Vegetation \cr
#' 1 \tab  Rich-herb without shrubs \cr
#' 2 \tab Rich-herb with shrubs/bilberry \cr
#' 3 \tab Rich-herb with shrubs/lingonberry \cr
#' 4 \tab Low-herb without shrubs \cr
#' 5 \tab Low-herb with shrubs/bilberry \cr
#' 6 \tab Low-herb with shrubs/lingonberry \cr
#' 7 \tab No field layer \cr
#' 8 \tab Broadleaved grass \cr
#' 9 \tab Thinleaved grass \cr
#' 10 \tab Sedge, high \cr
#' 11 \tab Sedge, low \cr
#' 12 \tab Horsetail, Equisetum ssp. \cr
#' 13 \tab Bilberry \cr
#' 14 \tab Lingonberry \cr
#' 15 \tab Crowberry \cr
#' 16 \tab Poor shrub \cr
#' 17 \tab Lichen, frequent occurrence \cr
#' 18 \tab Lichen, dominating \cr
#' }
#' @param ground_layer According to Swedish NFI, roughly:
#'
#'   \tabular{ll}{
#' Code \tab Ground layer \cr
#' 1 \tab Lichen type (>50\% of existing ground layer)\cr
#' 2 \tab Lichen-rich bogmoss type (>25\% lichen + >50\% Sphagnum of existing ground layer)\cr
#' 3 \tab Lichen rich (>25\% lichen, & not >50\% Sphagnum of existing ground layer) \cr
#' 4 \tab Bogmoss type (\emph{Sphagnum} > 50\% of existing ground layer) \cr
#' 5 \tab Swamp moss type (\emph{Polytrichum commune, P. gracile, P. strictum, Sphagnum, Depranocladus, Scorpidium, Paludella, Calliergon, Tomentypnum, Campylium.}) \cr
#' 6 \tab Fresh moss type (\emph{Hylocomium splendens, Ptilium crista-castrensis, Dicranum ssp.}) \cr
#' }
#' @param lateral_water Type 1="Missing",Type 2="Seldom",3="shorter periods",4="longer periods", 5="slope".
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#'
#' @return Site Index for Birch H50
#' @export
Eko_PM_2008_stand_factors_estimate_SI_Sweden_Birch <- function(
  altitude,
  latitude,
  vegetation,
  ground_layer,
  lateral_water,
  soil_moisture
){
  grasses <- ifelse(vegetation==8|vegetation==9,1,0)
  low_herbs <- ifelse(vegetation>3 & vegetation<7,1,0)
  tall_herbs <- ifelse(vegetation<4,1,0)
  herb <- ifelse(vegetation<7,1,0)
  polytrichum <- ifelse(ground_layer==5,1,0)
  mesic_soil_mosses <- ifelse(ground_layer==6,1,0)
  mesic <- ifelse(soil_moisture==2,1,0)
  moist <- ifelse(soil_moisture>3,1,0)
  lateral_frequent <- ifelse(lateral_water==4,1,0)
  northern <- ifelse(latitude>60,1,0)

  ifelse(northern == 1 & mesic == 1,
         return(
           88.469 - (0.00869 * altitude) - (1.112 * latitude) + (1.397 * herb) + (2.216 *
                                                                                    lateral_frequent)
         ),
         ifelse(
           northern == 1 & moist == 1,
           return(
             77.862 - (0.0103 * altitude) - (0.976 * latitude) + (0.732 * herb) + (1.372 *
                                                                                     polytrichum) + (1.914 * mesic_soil_mosses) + (1.304 * lateral_frequent)
           ),
           ifelse(
             northern == 0 & mesic == 1,
             return(
               -9.910 + (0.467 * latitude) + (2.956 * grasses) + (4.671 * low_herbs) +
                 (5.526 * tall_herbs) + (1.711 * lateral_frequent)
             ),
             ifelse(northern == 0 & moist == 1,
                    return(
                      0.259 - (0.00729 * altitude) + (0.287 * latitude) + (1.601 * grasses) +
                        (2.991 * low_herbs) + (2.173 * tall_herbs) + (1.224 * polytrichum) + (2.213 *
                                                                                                mesic_soil_mosses) + (1.978 * lateral_frequent)
                    ),
                    stop("Can only choose between 'moist' (soil_moisture==4) or 'mesic' (soil_moisture==2) soil moistures."))
           )
         ))

}
