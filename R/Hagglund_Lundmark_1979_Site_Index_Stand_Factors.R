#' Estimate Site Index H100 for Spruce or Pine in Sweden by stand factors.
#'
#' @source Corrected functions from Appendix II for estimation of site index
#' by site factors according to Hägglund and Lundmark (1977), in
#' Hägglund, B. (1979) Ett system för bonitering av skogsmark - analys,
#' kontroll och diskussion inför praktisk tillämpning. Rapport 14, Projekt
#' HUGIN. Skogsvetenskapliga fakulteten, Sveriges Lantbruksuniversitet.
#' Umeå. Sweden.
#'
#' Hägglund, B., Lundmark, J-E. (1977) Skattning av höjdboniteten med
#' ståndortsfaktorer: Tall och gran i Sverige. Research Notes nr. 28. Dept.
#' Forest Ecology & Forest Soils. Royal College of Forestry. Stockholm. Sweden.
#'
#' @details Uses corrected functions from appendix II in Hägglund & Lundmark
#' 1979, except for when used on peat soils, in which case functions f. 8.1
#' & f. 8.2. pages 138-139 from Hägglund & Lundmark (1977) is used.
#'
#'
#' @param species One of "Picea abies" (default) or "Pinus sylvestris".
#' @param vegetation integer 1-18 see
#' [forester::Sweden_vegetation_types(layer="field)]
#' @param groundLayer integer 1-6 see
#' [forester::Sweden_vegetation_types(layer="ground")]
#' @param latitude decimal WGS84.
#' @param longitude decimal WGS84.
#' @param altitude Meters above sea level.
#' @param aspectMain Main aspect of the site. One of: NA (default), "North",
#' "East,"South","West".
#' @param inclinePercent Incline of slope, in percent. Default == 0.
#' @param soilMoisture integer 1-5 see
#' [forester::Sweden_soil_types(type="moisture")]
#' @param soilTexture integer 1-9 see
#' [forester::Sweden_soil_types(type="texture")]
#' @param soilDepth integer 1-4 see
#' [forester::Sweden_soil_types(type="depth")]
#' @param lateralWater integer 1-3 see
#' [forester::Sweden_soil_types(type="water")]
#' @param ditched boolean TRUE/FALSE (default) if plot affected by ditching.
#' @param climateCode One of: NA (default),"M1","M2","K1","K2","K3". see
#' [forester::Angstrom_1958_local_climate_Sweden()]
#' @param close2Coast boolean TRUE/FALSE (default) if plot is closer to coast than 50km. e.g. [forester::coast_distance()]
#' @param LimesNorrlandicus boolean TRUE/FALSE (default) if plot is north of Sernanders Limes Norrlandicus (used for Peat soils only.)
#'
#' @return Site Index H100 (meters).
#' Will return NA & issue a warning message if no method was found.
#' @export
#'
#' @examples
Hagglund_Lundmark_1979_Site_Index_Stand_Factors <- function(
  species,
  vegetation,
  groundLayer,
  latitude,
  longitude,
  altitude,
  aspectMain=NA,
  inclinePercent=0,
  soilMoisture,
  soilTexture,
  soilDepth,
  lateralWater,
  ditched,
  climateCode=NA,
  close2Coast=FALSE,
  LimesNorrlandicus=FALSE
  ){
  #Ensure that aspect is correctly provided.
  if(!aspectMain %in% c("North","South","East","West",NA)) stop("aspectMain must be 'North', 'South', 'East', 'West' or NA")

  #Make sure vegetation,soilMoisture,soilTexture, groundLayer,lateralWater are integers.
  if(any((c(vegetation,soilMoisture,soilTexture,groundLayer,lateralWater,soilDepth)%%1)!=0)) stop ("vegetation, soilMoisture & soilTexture must be integers.")

  #Make sure climateCode is OK
  if(!(climateCode%in%c(NA,"M1","M2","K1","K2","K3"))) stop ("climateCode must
                                                             be one of NA, M1,
                                                             M2, K1, K2, K3")
  #Set climate booleans.

  #OBS! Correction according to note by B. Elfving (?). K2 was earlier K3 for
  #function 1, p. 101.
  #This corresponds to LOC 515 in SiteIndexCalculator.cs for Heureka.

  #Observe! If ClimateCode is NA ifelse comparison returns NA. check first.
  k2 <- ifelse(!is.na(climateCode) & climateCode=="K2",1,0)

  m2 <- ifelse(!is.na(climateCode) & climateCode=="M2",1,0)


  #Ensure lateralWater & soilMoisture can be combined.

  #Lateral water was earlier 5 categories (now 3). It separated:
  #1 Seldom/never into Seldom and Never.;
  #3 Longer Periods into Longer Periods and 'Lid'. p. 35 (1977)

  #In 1977 dry or very dry soil were separated.
  #soilMoisture 1 was also separated into seldom and missing.
  if(soilMoisture==1 & lateralWater!=1) stop("Dry soil can only have missing
                                             or seldom lateral water
                                             (lateralWater==1")

  #TODO: shortWater_1 is .. shorter periods only ? Out of old vars missing,
  #seldom, shorter, longer, lid..
  #shortWater_1 <- ifelse(lateralWater==3, TRUE, FALSE)
  shortWater_1 <- ifelse(lateralWater==2,1,0)

  #Ensure species and soilMoisture can be combined.
  if(species=="Picea abies" & soilMoisture==1) stop("No data available for
                                                    spruce on dry to very
                                                    dry soils.")


  #Incline calculation
  #NB if above 2:20, 5.71deg, 10%.
  #Applicable if Pine on North or East aspect above 350 MASL.
  NE_incline <- ifelse(inclinePercent>10 & aspectMain %in% c("North","East"),
                       1,
                       0)
  zeroIncline <- ifelse(inclinePercent<=10,1,0)

  if(missing(altitude)) stop("Altitude must be provided. e.g.
                             googleway::google_elevation()")
  if(altitude<0) warning("Altitude less than 0, setting altitude to 0.")
  if(altitude>1000) warning("Altitude above 1000m MASL.")

  #Set soilTexture index. From Björn Elfvings notes. (?).
  #NB! This will hinder soilTexture==9 for Peat soils if run before.
  Peat=ifelse(soilTexture==9,TRUE,FALSE)
  soilTexture <- 2.265-0.204*soilTexture+2.94/soilTexture

  #Set Global Parameters
  #altitude parameters
  alt_above_350 <- ifelse(altitude>350,TRUE,FALSE)

  #soil parameters
  deepSoil <- ifelse(soilDepth==1,1,0)


  ##Lateral water
  shortWater <- ifelse(lateralWater %in% c(1,2),1,0)
  longWater <- ifelse(lateralWater==3,1,0)

  ##Vegetation combinations
  herbType <- ifelse(vegetation<7,1,0)
  bilberry <- ifelse(vegetation==13,1,0)
  grassesCowberryNoFieldLayer <- ifelse(vegetation%in%c(7,8,9,14),1,0)
  poorShrubsHorsetailSedges <- ifelse(vegetation%in%c(10,11,12,16),1,0)
  herbTypesnobrush <- ifelse(vegetation%in%c(1,4,7),1,0)
  herbTypesbrush <- ifelse(vegetation%in%c(2,3,5,6,13,14),1,0)
  poor <- ifelse(vegetation%in%c(10,11,12,16),1,0)

  #Ground layer parameters
  swampMosses <- ifelse(groundLayer==5,1,0)
  freshMosses <- ifelse(groundLayer==6,1,0)
  sphagnumLichen <- ifelse(groundLayer%in%c(2,3,4,5),1,0) #Sphagnum-Lavtyper:
  #Lichenrich bogmoss type, Lichen-rich type, Bogmoss (sphagnum) type,
  #Swamp moss type.

  #D correction value.
  D <- altitude + 130*altitude - 8900

  #G1, G2 correction coefficients. 1979.
  g1 <- if(D<=-60){
    1.05
  } else if(-60<=D & D<0){
    (1-(D/1200))
  } else if(D>=0){
    1
  }

  g2 <- if(D<0){
    1
  } else if(D >= 0 & D < 60){
    (1-(D/600))
  } else if(D>=60){
    0.9
  }

  #Function choice

  #Peat choice first.
  if(Peat){
    if(species=="Pinus sylvestris"){

      #Note, must return h100dm, since no further transformation done.

      h100dm <- exp(5.03077+
        -0.03627*(latitude-60)+ #-0.03626?
        #NB in Hägglund 1977 variable AK (closer than 50km to coast) is only
        #applied if site is north of Limes norrlandicus, page. 68.
        -0.00901*LimesNorrlandicus*close2Coast*((altitude^2)/10000)+
        +0.09195*longWater+
        +0.13622*ditched+
        -0.14970*sphagnumLichen+
        +0.22273*herbType+
        +0.13997*bilberry+
        +0.13377*grassesCowberryNoFieldLayer)
    }

    if(species=="Picea abies"){
      #Sphagnum type in book not same as sphagnumLichen type.
      #Here interpreted so to follow main routine from Heureka until further
      #notice.

      #Note, must return h100dm, since no further transformation done.

     h100dm <- exp(5.51735+
       -0.03958*((latitude-60)+abs(latitude-60))+
       -0.02466*((latitude-60)-abs(latitude-60))+
       -0.01174*close2Coast*LimesNorrlandicus*shortWater*((altitude^2)/10000)+
       -0.00899*close2Coast*LimesNorrlandicus*longWater*((altitude^2)/10000)+
       +0.04577*shortWater+
       +0.063*longWater+
       +0.04428*ditched+
       -0.20935*sphagnumLichen+
       +0.09517*herbTypesnobrush+
       -0.04868*herbTypesbrush+
       -0.12545*poor)
          }

  } else if(species=="Pinus sylvestris"){
    if(soilMoisture==1){ #Pine on dry and very dry soil.

      #OBS K2 was erronously given as K3 in report!
      lnh100dm <- 5.44789+
        -0.01566*(latitude-60+abs(latitude-60))+
        -0.01020*((altitude^2)/10000)+
        0.09162*deepSoil+
        -0.0417*soilTexture+
        0.12*k2

      if(vegetation==18){
        h100dm <- g2*exp(lnh100dm + -0.19805)

      } else if(vegetation==17){
        h100dm <- g2*exp(lnh100dm+ -0.13810)

      } else if(vegetation %in% c(1,2,3,4,5,6,8)){
        h100dm <- g1*exp(lnh100dm+ 0.0953)

      } else if(vegetation %in% c(9,7,13)){
        h100dm <- g1*exp(lnh100dm+ 0.0488)

      } else if(vegetation %in% c(14,15,16)){
        h100dm <- g2*exp(lnh100dm)
      }

    } else if(soilMoisture==2 & vegetation %in% c(1,2,3,4,5,6,7,8,9)){


      lnh100dm <- 5.34912+
        -0.02037*(latitude-60+abs(latitude-60))+
        -0.00481*((altitude^2)/10000)+
        0.11574*deepSoil+
        -0.16403*m2

      if(vegetation %in% c(4,5,8,9)){
        h100dm <- g1 * exp(lnh100dm + 0.08376)
      } else if(vegetation %in% c(6,7,3)){
        h100dm <- g1*exp(lnh100dm)
      } else if(vegetation %in% c(1,2)){
        h100dm <- g1*(exp(lnh100dm + 0.12296))
      }

    } else if(soilMoisture==2 & vegetation %in% c(13,14,15,16)){
      #Check groups for s. 103, Hägglund 1979. Pine on mesic soil, shrub types.

      lnh100dm <- 5.30943+
        -0.01716*(latitude-60+abs(latitude-60))+
        -0.00390*(latitude-60-abs(latitude-60))+
        -0.00678*((altitude^2)/10000)+
        longWater*0.0488+
        deepSoil*0.11580+
        -0.01243*(soilTexture^2)

      if(vegetation==13){
        h100dm <- g1*exp(lnh100dm + 0.09429)

      } else if(vegetation==14){
        h100dm <- g1*exp(lnh100dm + 0.06167)

      } else if(vegetation==15){ #Kråkbär, Ljung (Crowberry, Heath?)
        h100dm <- g2*exp(lnh100dm)

      } else if(vegetation==16){ #Odon-skvattram == fattigris?
        h100dm <- g1*exp(lnh100dm + -0.07775)

      }


    } else if(soilMoisture==2 & vegetation %in% c(17,18)){
      #Pine mesic soil with lichen types.

      lnh100dm <- 5.21803+
        -0.01193*((latitude-60)+abs(latitude-60))+
        -0.00593*shortWater*((altitude^2)/10000)+
        -0.00355*longWater*((altitude^2)/10000)+
        0.12454*deepSoil+
        -0.06329*zeroIncline*shortWater+
        -0.07189*alt_above_350*NE_incline

      if(vegetation==18){
        h100dm <- g2*exp(lnh100dm + -0.06842)
      } else if(vegetation==17){
        h100dm <- g2*exp(lnh100dm)
      }

    } else if(soilMoisture %in% c(3,4)){
      #Pine on mesic-moist to moist ground.

      lnh100dm <- 5.46782+
        -0.02013*((latitude-60)+abs(latitude-60))+
        -0.01517*shortWater*((altitude^2)/10000)+
        -0.00747*longWater*((altitude^2)/10000)+
        -0.01074*(soilTexture^2)+
        -0.073*swampMosses+
        0*freshMosses

      if(vegetation%in%c(1,2,3,4,5,6)){
        h100dm <- g1*exp(lnh100dm + 0.11585)

      } else if(vegetation %in% c(10,11,12)){
        h100dm <- g2*exp(lnh100dm + -0.22358)

      } else if(vegetation %in% c(13,14,8,9,7)){
        h100dm <- g1*exp(lnh100dm + 0.0770)

      } else if(vegetation %in% c(15,16)){
        h100dm <- g2*exp(lnh100dm + -0.0726)

      }
    }

  } else if(species=="Picea abies"){

    if(soilMoisture==2 & vegetation %in% c(1:9)){


      lnh100dm <- 5.68205+
        -0.03423*((latitude-60)+abs(latitude-60))+
        -0.02122*((latitude-60)-abs(latitude-60))+
        -0.00691*((altitude^2)/10000)+
        shortWater*0.03247+
        longWater*0.05097+
        -0.10806*m2+
        -0.073*swampMosses

      if(vegetation %in% c(2,3,5,6,7)){
        h100dm <- exp(lnh100dm + -0.02991)

      } else if(vegetation %in% c(8,9)){
        h100dm <- exp(lnh100dm + -0.06787)

      } else if(vegetation %in% c(4)){
        h100dm <- exp(lnh100dm)

      } else if(vegetation %in% c(1)){
        h100dm <- exp(lnh100dm + 0.039)
      }

    } else if(soilMoisture==2 & vegetation %in% c(10:18)){
      #Carex low and high, Lichen and lichen rich are
      #lingonberry or worse group. Also Equisetum group.

      lnh100dm <- 5.51876+
        -0.04342*((latitude-60)+abs(latitude-60))+
        -0.01837*((latitude-60)-abs(latitude-60))+
        -0.01095*shortWater*((altitude^2)/10000)+
        -0.00716*longWater*((altitude^2)/10000)+
        -0.073*swampMosses+
        shortWater_1*0.03361+
        0.04605*longWater

      if(vegetation==13){ # if blueberry
        h100dm <- exp(lnh100dm + 0.07842)
      } else if(vegetation!=13){ # change from lingon and worse to not blueberry
        h100dm <- exp(lnh100dm)
      }

    } else if(soilMoisture %in% c(3,4)){

      lnh100dm <- 5.59884 +
        -0.03722*((latitude-60)+abs(latitude-60))+
        -0.02499*((latitude-60)-abs(latitude-60))+
        -0.01206*shortWater*((altitude^2)/10000)+
        -0.00937*longWater*((altitude^2)/10000)+
        0.04766*shortWater_1+
        0.05939*longWater+
        0.02383*ditched+
        -0.073*swampMosses

      if(vegetation ==1 ){
        h100dm <- exp(lnh100dm + 0.12)

      } else if (vegetation==7){
        h100dm <- exp(lnh100dm + 0.08075)

      } else if (vegetation==4){
        h100dm <- exp(lnh100dm + 0.05342)

      } else if (vegetation %in% c(13,14)){
        h100dm <- exp(lnh100dm - 0.05889)

      } else if(vegetation %in% c(10,11,12,15,16)){
        h100dm <- exp(lnh100dm - 0.1643)

      } else if(vegetation %in% c(2,3,5,6,8,9)){
        h100dm <- exp(lnh100dm)

      }


    }

  }

  if(exists("h100dm")){
   SIS <- h100dm/10
  } else {
   warning(paste0("No method was found for your plot."))
   SIS <- NA
  }

  return(
    SIS
  )
}

