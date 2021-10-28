#' Calculate Tegnhammar's adjusted Site Index from stand variables
#'
#' @source Tegnhammar, L. (1992) "Om skattningen av ståndortsindex för gran",
#' eng:"On the estimation of site index for Norway spruce". Report 53. Dept. of
#' Forest Survey. Swedish University of Agricultural Sciences. Umeå. p.80.
#' ISSN 0348-0496
#'
#' @description Stand variables were regressed to estimate Tegnhammar's adjusted site index SIHjust.
#'
#'  \strong{SIHjust} corrects height development functions by Hägglund
#'  (1972, 1973) for the 'apparent' trend, an age bias, wherein the functions
#'  would indicate higher SIH in young forests as compared to older forests,
#'  even if the site index is the same.
#'
#'  Tegnhammar's dissertation investigated a trend in the site index
#'  curves for spruce in Sweden, possibly caused by stem sectioning of elderly
#'  trees that were suppressed in their youth; young planted forests are
#'  composed of better plant material; young forests have recieved a better
#'  start as a result of methods introduced during the 1920-1950's; older stands
#'  which were not suitable for SI estimation by height were measured in such
#'  a manner nonetheless; The risk of including hidden top breakages is larger
#'  for older trees; & the productivity has increased with time due to nitrogen
#'  deposition - younger trees have been affected for a longer period of their
#'  lifetime.
#'
#' @details
#'
#' N.B. Not for use on Öland / Gotland, or soil moisture types 'Wet'/'Blöt'.
#' No underlying material for these sites. The material was too small to determine the exact size of the error, but indicate that estimates are much too high for these sites.
#'
#' s = 30.71662
#' R^2  = 0.7045
#'
#' @param latitude decimal degrees. By default WGS84, see parameter 'epsg'.
#' @param longitude decimal degrees. By default WGS84, see parameter 'epsg'.
#' @param altitude metres above sea level
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
#' @param aspect If more than 1:20 (5\%), one of the following, otherwise 0.
#'
#'  \tabular{cl}{
#' 1 \tab North \cr
#' 2 \tab North-East \cr
#' 3 \tab East \cr
#' 4 \tab South-East \cr
#' 5 \tab South \cr
#' 6 \tab South-West \cr
#' 7 \tab West \cr
#' 8 \tab North-West \cr
#' }
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param soil_depth Type 1="Deep, >70cm",2="Rather shallow, 20-70cm",3="Shallow <20cm", 4= "Varying".
#' @param soil_texture
#' \tabular{ll}{
#' Code \tab Short \cr
#' 1 \tab Boulder \cr
#' 2 \tab Gravel \cr
#' 3 \tab Coarse sand \cr
#' 4 \tab Medium sand \cr
#' 5 \tab Fine sand \cr
#' 6 \tab Coarse silt \cr
#' 7 \tab Fine silt \cr
#' 8 \tab Clay \cr
#' 9 \tab Peat \cr
#' }
#' @param humidity Humidity during the \emph{vegetation period}. mm. If not provided, will be calculated from latitude, longitude with [forester::Eriksson_1986_humidity()] from Eriksson, B. (1986) fig.34. Available online (26/10/2021) \url{https://www.smhi.se/polopoly_fs/1.166663!/RMK_46\%20Nederbörds-\%20och\%20humiditetsklimat\%20i\%20Sverige\%20under\%20vegetationsperioden..pdf}
#' @param ditched TRUE/FALSE if affected by ditching.
#' @param lateral_water Type 1="Missing",Type 2="Seldom",3="shorter periods",4="longer periods", 5="slope".
#' @param peat_humification "Low", if organic remnants clearly visible: squeezing sample gives somewhat cloudy water; "Medium", if with some difficulty organic remnants can be distinguished, if sample is squeezed the water is cloudy; or "High", if no organic remnants can be distinguished. Water and humus cannot be separated by squeezing. Humus is porridge-like.
#' @param epsg Default is WGS84 'EPSG:4326'. For SWEREF-99TM - 'EPSG:3006'.
#'
#' @return Tegnhammars adjusted SI, meters.
#' @export
#'
#' @examples
Tegnhammar_1992_adjusted_SI_by_stand_variables <- function(species,latitude,longitude, altitude,vegetation, ground_layer,aspect_main,incline_percent,soil_moisture,soil_depth,soil_texture,humidity,ditched,lateral_water,peat_humification="Medium",epsg='EPSG:4326'){

  #If missing humidity.
  if(missing(humidity)){
     assign("humidity",forester::Eriksson_1986_humidity(latitude = latitutde,longitude = longitude,epsg=epsg))
  }

  #TRUE if site is located north of Limes Norrlandicus
  limes_N <- terra::relate(terra::vect(matrix(ncol=2,nrow=1,dimnames = list(c(),c("lon","lat")),data=c(longitude,latitude)),crs=epsg),as(Limes_norrlandicus,"SpatVector"),"within")[1]


  if(limesN==TRUE){
    LatN <- TRUE
    LatS <- FALSE
  } else {
    LatN <- FALSE
    LatS <- TRUE
  }

  if(LatN==TRUE){
    Altitude_S_Sweden <- FALSE
    Altitude_N_Sweden <- altitude
  } else {
    Altitude_N_Sweden <- FALSE
    Altitude_S_Sweden <- altitude
  }


  humidity_south_sweden_dry <- if(LatS==TRUE && soil_moisture%in%c(1,2)){
    humidity
  } else {
    0
  }

  humidity_north_sweden <- if(LatN==TRUE){
    humidity
  } else {
    0
  }

  humidity_north_sweden_LatN <- humidity_north_sweden*LatN

  shallow <- if(soil_depth!=1){1} else {0}

  dry <- if(soil_moisture==1){1} else {0}

  moist <- if(soil_moisture==4){1} else {0}

  peat <- if(soil_texture==9){1} else {0}

  if(ditched==1 && peat==0 && moist==0){
    message("Ditched only defined for peat or moist soils! Setting ditched to 0.")
    ditched <- 0
  }



  lateral_water_longer_periods <- if(lateral_water==4){1} else {0}

  lateral_water_shorter_periods <- if(lateral_water==3){1} else {0}

  coarse <- if(soil_texture%in%c(1,2,3)){1} else {0}

  fine <- if(soil_texture%in%c(7,8)){1} else {0}



  if(peat==0){
    peat_humification_low <- 0
    peat_humification_medium <- 0
    peat_humification_high <- 0
  } else if(peat==1){
    if(peat_humification=="Low"){
      peat_humification_low <- 1
      peat_humification_medium <- 0
      peat_humification_high <- 0
    } else if(peat_humification=="Medium"){
      peat_humification_low <- 0
      peat_humification_medium <- 1
      peat_humification_high <- 0
    } else if(peat_humification=="High"){
      peat_humification_low <- 0
      peat_humification_medium <- 0
      peat_humification_high <- 1
    }
  }


  #incline, if above 1:20 == 5% AND aspect east or southeast.
  aspect_east_south_east_incline <- ifelse(aspect%in%c(6,7),1,0)


  #Extremely cold site
  extremely_cold <-
    ifelse(
      ((altitude + (130 * latitude) - 8900) > 0),
           (altitude + (130 * latitude) - 8900),
           0
           )

  #distance to closest SWEDISH coast in km
  distance_to_swedish_coast <- forester::coast_distance(latitude = latitude,longitude = longitude,epsg = epsg)

  distance_to_swedish_coast_2 <- if(soil_moisture%in%c(1,2)){
    exp((-distance_to_swedish_coast/5))
  } else {
    0 # param KUSTT is always 0 if soil is moist, page 83. page 154.
  }

  #Vegetation type
  rich_herb_no_shrub <- if(vegetation==1){1} else {0}
  low_herb_no_shrub <- if(vegetation==3){4} else {0}
  herb_with_shrub <- if(vegetation%in%c(2,3,5,6)){1} else {0}
  herb <- if(vegetation%in%c(1:6)){1} else {0}
  no_field_layer <- if(vegetation==7){1} else {0}
  broadleaved_grass <- if(vegetation==8){1} else {0}
  thinleaved_grass <- if(vegetation==9){1} else {0}
  Carex_or_Equisetum <- if(vegetation==c(10:12)){1} else {0}
  bilberry <- if(vegetation==13){1} else {0}
  lingonberry <- if(vegetation==14){1} else {0}
  crowberry_or_worse <- if(vegetation>=15){1} else {0}

  #Ground layer
  sphagnum <- if(ground_layer==4){1} else {0}
  polytrichum <- if(ground_layer==5){1} else {0}

  #Calculation
  SIHjust <- 1210.813605+
    -14.969124*LatN+
    -15.444848*LatS+
    -1.308729*Altitude_N_Sweden+
    -0.000062709*(Altitude_N_Sweden^2)+
    -0.048787*Altitude_N_Sweden*moist+
    +0.000099984*(Altitude_N_Sweden^2)*moist+
    -2.396754*Altitude_S_Sweden+
    -0.000326*(Altitude_S_Sweden^2)+
    +0.019672*LatN*Altitude_N_Sweden+
    +0.042405*LatS*Altitude_S_Sweden+
    +4.084904*humidity_north_sweden+
    -0.064359*humidity_north_sweden_LatN+
    -0.085390*humidity_north_sweden*moist+
    +0.026421*humidity_south_sweden_dry+
    -22.492215*distance_to_swedish_coast_2+
    -6.494468*dry+
    +3.284251*moist+
    +8.004164*lateral_water_shorter_periods+
    +12.557257*lateral_water_longer_periods+
    +7.162013*ditched+
    -2.241642*fine+
    -3.944443*coarse+
    -22.450707*peat_humification_low+
    -10.735284*peat_humification_medium+
    -2.872332*peat_humification_high+
    -12.206813*shallow+
    +5.792854*shallow*moist+
    +36.421502*rich_herb_no_shrub+
    +31.110677*low_herb_no_shrub+
    +20.906555*herb_with_shrub+
    +22.879371*no_field_layer+
    +11.623366*broadleaved_grass+
    +13.542164*thinleaved_grass+
    -6.635013*Carex_or_Equisetum+
    -10.954260*lingonberry+
    -19.853882*crowberry_or_worse+
    -15.615067*sphagnum+
    -5.950684*polytrichum+
    -0.062692*extremely_cold+
    +5.299670*aspect_east_south_east_incline

  return(SIHjust/10) #return metres.


}
