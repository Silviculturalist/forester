#' Site index estimation by site factors
#'
#' @description Corrected functions from Appendix II for estimation of site index by site factors according to Hägglund and Lundmark (1977), in
#' Hägglund, B. (1979) Ett system för bonitering av skogsmark - analys, kontroll och diskussion inför praktisk tillämpning. Rapport 14, Projekt HUGIN. Skogsvetenskapliga fakulteten, Sveriges Lantbruksuniversitet. Umeå. Sweden.
#'
#' @param plotid  PlotID
#' @param species "Picea abies" or "Pinus sylvestris"
#' @param vegetation Type 1-18.
#' @param ground_layer Type 1="Lichen type", 2="Lichen-rich bogmoss type", 3="Lichen-rich",4="Bogmoss type" Sphagnum,5="Swamp moss type",6="Fresh moss type"
#' @param latitude Decimal degrees.
#' @param longitude Decimal degrees.
#' @param altitude Metres above sea level
#' @param aspect_main Main aspect of site, one of :"North","South","East","West" or 0.
#' @param incline_percent Incline of slope, in percent.
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param soil_texture Type 1-9
#' @param soil_depth Type 1="Deep, >70cm", 2 = "Rather shallow, 20-70 cm", 3 = "Shallow <20", 4 = "Varying"
#' @param lateral_water Type 1="Missing", 2="Seldom",3="shorter periods",4="longer periods",5="slope".
#' @param ditched TRUE/FALSE if affected by ditching.
#' @param climate_code Optional, else from lat, lon.
#' @param county Optional, else from lat/lon

#'
#' @return Site index, in metres.
#' @examples
#' SIS_estimate(plotid=12332, species="Pinus sylvestris",vegetation=13, ground_layer=6, latitude=63.8,longitude=12.32, altitude=200,incline_percent=0,soil_moisture=2,soil_texture=2,soil_depth=1,lateral_water=1,ditched=FALSE)


SIS_estimate <- function(plotid,species, vegetation, ground_layer, latitude, longitude, altitude, aspect_main, incline_percent, soil_moisture, soil_texture, soil_depth, lateral_water, ditched, local_climate, county){

assertive.numbers::assert_all_are_whole_numbers(c(vegetation, soil_moisture,soil_texture),tol = 0)

if(!aspect_main %in% c("North","South","East","West",0)) stop("aspect_main must be 'North', 'South', 'East', 'West' or 0")



#County calc
if(missing(county)){
  county <- forester::county_sweden(latitude=latitude,longitude = longitude)
}

#incline calculation
#NB IF ABOVE 2:20 , 5.71° , 10%.
#Applicable if Pine on North or East aspect above 350 MASL
if(incline_percent>10 & aspect_main %in% c("North","East")){
  ne_incline <- TRUE
} else {
  ne_incline <- FALSE
}

if(incline_percent<=10){
  no_incline <- TRUE
} else {
  no_incline <- FALSE
}

#Climate Code calc.
if(missing(local_climate)){
  local_climate <- forester::local_climate_sweden(latitude=latitude,longitude = longitude)
}

#use googleway for altitude if missing
if(missing(altitude)){
  altresults <- googleway::google_elevation(data.frame(lon=longitude,lat=latitude),location_type = "individual",key=key)
  altitude <- as.numeric(altresults$results[1])
  rm(altresults)
}



#D correction value.
D <-  altitude + 130*latitude - 8900

#G1, G2 correction coefficient
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
if(species=="Pinus sylvestris"){
  if(soil_moisture==1){ #Pine on dry and very dry soil.
    deep_soil <- ifelse(soil_depth==1,TRUE,FALSE)
    k3 <- ifelse(local_climate=="K3",TRUE,FALSE)
    lnh100dm <- 5.44789 + -0.01566*(latitude-60+abs(latitude-60)) + -0.01020*((altitude^2)/10000) + 0.09162*deep_soil + -0.0417*soil_texture + 0.12*k3

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

  } else if(soil_moisture==2 & vegetation %in% c(1,2,3,4,5,6,7,8,9)){
    deep_soil <- ifelse(soil_depth==1,TRUE,FALSE)
    m2 <- ifelse(local_climate=="M2",TRUE,FALSE)

    lnh100dm <- 5.34912 + -0.02037*(latitude-60+abs(latitude-60)) + -0.00481*((altitude^2)/10000) + 0.11574*deep_soil + -0.16403*m2

    if(vegetation %in% c(4,5,8,9)){
      h100dm <- g1 * exp(lnh100dm + 0.08376)
    } else if(vegetation %in% c(6,7,3)){
      h100dm <- g1*exp(lnh100dm)
    } else if(vegetation %in% c(1,2)){
      h100dm <- g1*(exp(lnh100dm + 0.12296))
    }

  } else if(soil_moisture==2 & vegetation %in% c(13,14,15,16)){ #Check groups for s. 103, Hägglund 1979. Pine on mesic soil, shrub types.
    long_water <- ifelse(lateral_water%in%c(4,5),TRUE,FALSE)
    deep_soil <- ifelse(soil_depth==1,TRUE,FALSE)

    lnh100dm <- 5.30943 + -0.01716*(latitude-60+abs(latitude-60))+ -0.00390*(latitude-60-abs(latitude-60)) + -0.00678*((altitude^2)/10000) + long_water*0.0488 + deep_soil*0.11580 + -0.01243*(soil_texture^2)

    if(vegetation==13){
      h100dm <- g1*exp(lnh100dm + 0.09429)

    } else if(vegetation==14){
      h100dm <- g1*exp(lnh100dm + 0.06167)

    } else if(vegetation==15){ #Kråkbär, Ljung (Crowberry, Heath?)
      h100dm <- g2*exp(lnh100dm)

    } else if(vegetation==16){ #Odon-skvattram == fattigris?
      h100dm <- g1*exp(lnh100dm + -0.07775)

    }


  } else if(soil_moisture==2 & vegetation %in% c(17,18)){ #Pine mesic soil with lichen types.
    long_water <- ifelse(lateral_water%in%c(4,5),TRUE,FALSE)
    short_water <- ifelse(lateral_water %in% c(1,2,3),TRUE,FALSE)
    deep_soil <- ifelse(soil_depth==1,TRUE,FALSE)
    alt_above_350 <- ifelse(altitude>350,TRUE,FALSE)

    lnh100dm <- 5.21803 + -0.01193*((latitude-60)+abs(latitude-60)) + -0.00593*short_water*((altitude^2)/10000) + -0.00355*long_water*((altitude^2)/10000) + 0.12454*deep_soil + -0.06329*no_incline*short_water + -0.07189*alt_above_350*ne_incline

    if(vegetation==18){
      h100dm <- g2*exp(lnh100dm + -0.06842)
    } else if(vegetation==17){
      h100dm <- g2*exp(lnh100dm)
    }

  } else if(soil_moisture %in% c(3,4)){ #Pine on mesic-moist to moist ground.
    long_water <- ifelse(lateral_water%in%c(4,5),TRUE,FALSE)
    short_water <- ifelse(lateral_water %in% c(1,2,3),TRUE,FALSE)
    swamp_moss <- ifelse(ground_layer==5,TRUE,FALSE)
    fresh_moss <- ifelse(ground_layer==6,TRUE,FALSE)

    lnh100dm <- 5.46782 + -0.02013*((latitude-60)+abs(latitude-60)) + -0.01517*short_water*((altitude^2)/10000) + -0.00747*long_water*((altitude^2)/10000) + -0.01074*(soil_texture^2) + -0.073*swamp_moss + 0*fresh_moss

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

  if(soil_moisture==2 & vegetation %in% c(1,2,3,4,5,6,7,8,9)){
    short_water <- ifelse(lateral_water%in%c(3),TRUE,FALSE)
    long_water <- ifelse(lateral_water%in%c(4,5),TRUE,FALSE)
    m2 <- ifelse(local_climate=="M2",TRUE,FALSE)
    swamp_moss <- ifelse(ground_layer==5,TRUE,FALSE)

    lnh100dm <- 5.68205 + -0.03423*((latitude-60)+abs(latitude-60)) + -0.02122*((latitude-60)-abs(latitude-60)) + -0.00691*((altitude^2)/10000) + short_water*0.03247 + long_water*+0.05097 + -0.10806*m2 + -0.073*swamp_moss

    if(vegetation %in% c(2,3,5,6,7)){
      h100dm <- exp(lnh100dm + -0.02991)

    } else if(vegetation %in% c(8,9)){
      h100dm <- exp(lnh100dm + -0.06787)

    } else if(vegetation %in% c(4)){
      h100dm <- exp(lnh100dm)

    } else if(vegetation %in% c(1)){
      h100dm <- exp(lnh100dm + 0.039)
    }

  } else if(soil_moisture==2 & vegetation %in% c(10,11,12,13,14,15,16,17,18)){ #Carex low and high, Lichen and lichen rich are lingonberry or worse group. Also Equisetum group.
    swamp_moss <- ifelse(ground_layer==5,TRUE,FALSE)
    short_water_1 <- ifelse(lateral_water==3, TRUE, FALSE)
    short_water <- ifelse(lateral_water%in%c(1,2,3), TRUE, FALSE)
    long_water <- ifelse(lateral_water%in%c(4,5), TRUE, FALSE)

    lnh100dm <- 5.51876 + -0.04342*((latitude-60)+abs(latitude-60)) + -0.01837*((latitude-60)-abs(latitude-60)) + -0.01095*short_water*((altitude^2)/10000) + -0.00716*long_water*((altitude^2)/10000) + -0.073*swamp_moss + short_water_1*0.03361 + 0.04605*long_water

    if(vegetation==13){ # if blueberry
      h100dm <- exp(lnh100dm + 0.07842)
    } else if(vegetation!=13){ # change from lingon and worse to not blueberry
      h100dm <- exp(lnh100dm)
    }

  } else if(soil_moisture %in% c(3,4)){
    swamp_moss <- ifelse(ground_layer==5,TRUE,FALSE)
    short_water_1 <- ifelse(lateral_water==3, TRUE, FALSE)
    short_water <- ifelse(lateral_water%in%c(1,2,3), TRUE, FALSE)
    long_water <- ifelse(lateral_water%in%c(4,5), TRUE, FALSE)


    lnh100dm <- 5.59884 + -0.03722*((latitude-60)+abs(latitude-60)) + -0.02499*((latitude-60)-abs(latitude-60)) + -0.01206*short_water*((altitude^2)/10000) + -0.00937*long_water*((altitude^2)/10000) + 0.04766*short_water_1 + 0.05939*long_water + 0.02383*ditched + -0.073*swamp_moss

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
  return(h100dm/10)
} else {
  if(!missing(plotid)){
    warning(paste0("No method was found for plot: ",plotid))
  } else {
    warning(paste0("No method was found for your plot. "))
  }

  return(NA)

  }

}
