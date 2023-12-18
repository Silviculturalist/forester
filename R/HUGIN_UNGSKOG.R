#' Stocking level in cultivated or natural regenerations
#'
#' @source Elfving, B. (1992). Återväxtens etablering och utveckling till röjningstidspunkten. SLU, Institutionen för Skogsskötsel. Arbetsrapport nr. 67.
#' Available, p.5 : \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#' @source Elfving, B. 1982. Hugins ungskogstaxering 1976-1979. SLU, Projekt HUGIN. Rapport 27.
#' @source Nyström, K. & Söderberg, U. 1987. Tillväxtberäkningen för ungskog i Hugin-systemet. SLU. Inst. f. skogsskötsel. Arbetsrapport 18.
#'
#' @description OBSERVE! Plausible retransformation error?
#' Distance from equator calculated as 111 km * latitude.
#' Jämtland county has been interpreted as both Jämtland - Härjedalens landskap and Jämtland - Jämtlands landskap.
#'
#' OBSERVE! This function includes the modifications to coefficients as defined on p. 6 in the source:
#' \emph{Increased efficiency in provenance selection and scarification has also been considered by assumed influence on the cofficients. For cultivations the negative effect by increasing height above see level has been reduced from -0.0514 to -0.0257 and the positive effect by scarification has been increased from 0.0757 to 0.2.}
#'
#'
#' @details
#'
#' \strong{Summary statistics for natural regenerations}
#' \itemize{
#' \item n = 2462.
#' \item R^2 = 0.492
#' \item s-res = 0.475
#' \item mean of the dependents: 1.8585
#' }
#'
#' \strong{Summary statistics for cultivated or sown stands}
#' \itemize{
#' \item n = 3528
#' \item R^2 = 0.492
#' \item s-res = 0.391
#' \item mean of the dependents: 2.3059
#' }
#'
#'
#' @param altitude Meters above sea level.
#' @param latitude Decimal latitude.
#' @param main_species Main species "Picea abies" or "Pinus sylvestris".
#' @param regeneration_method "natural" or "cultivated-sown" or "cultivated-planted"
#' @param age age, number of vegetation periods between cut and inventory.
#' @param proportion_cultivated percent cultivated, for partly planted sites.
#' @param N_full number seedlings demanded for full stocking. In thousands?
#' @param soil_moisture Soil moisture code [forester::Sweden_soil_types('moisture')]
#' @param scarification 1 if scarified, 0 if no treatment.
#' @param burnt 1 if prescribed burning, 0 if no treatment.
#' @param no_treatment 1 if no measures were taken to promote regeneration.
#' @param uncleaned 1 if bushes and small trees remain after clear-cut.
#' @param number_seed_trees number of seed trees per hectare
#' @param H100_Spruce e.g. [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce],
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce],
#' [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param area size of regeneration area in hectares.
#' @param square_spacing Average square spacing used for sowing/planting
#'
#' @return A vector of SLH: Stocking quality, between 0 and 1, and W, a measure of the young stand quality between 0 and 1.
#' @export
Elfving_2010_regeneration_stocking <- function(altitude, latitude,age, regeneration_method, proportion_cultivated, N_full, ground_water, county="county", scarification, burnt, no_treatment, uncleaned, number_seed_trees, H100_Spruce, area,square_spacing,soil_moisture){
  warning('TODO: This function not appropriately checked.')
  #calculate Jonsson class.
  jonsbon <- forester::MAI_to_jonsson(forester::Hagglund_1981_si_to_bonitet_integrated(H100=H100_Spruce, altitude=altitude,county=county,vegetation=vegetation,main_species=main_species))

  #Indicator variable
  if(latitude >= 60){
    northern_sweden <- 1
  } else {
    northern_sweden <- 0
  }

  #calculating the corresponding swedish map..? Distance from the equator.. ?
  #Should this be RT90?
  swe_map <- (((geosphere::distHaversine(c(1,latitude),c(1,0))/1000)-6050)/50)

  #is there less than 1 m to the ground water level?
  if(soil_moisture%in%c(3,4,5)){
    ground_water_shallow <- 1
  } else {
    ground_water_shallow <- 0
  }

  # is there more than 2 metres to ground water level?
  if(soil_moisture %in%c(1)){
    ground_water_deep <- 1
  } else{
    ground_water_deep <- 0
  }

  #indicator for Värmland, Medelpad, Jämtland counties. S, Y, Z counties.
  if(county%in%c("Västernorrland - Medelpads landskap",
                 "Jämtland - Jämtlands landskap",
                 "Jämtland - Härjedalens landskap")){
    SYZ_county_indicator <- 1
  } else {
    SYZ_county_indicator <- 0
  }

  #indicator for Gotland.
  if(county=="Gotland"){
    gotland <- 1
  } else {
    gotland <- 0
  }

  #indicator for Örebro county
  if(county=="Örebro"){
    orebro <- 1
  } else {
    orebro <- 0
  }

  #Indicator for natural regeneration without seed-trees.
  if(number_seed_trees==0){
    no_seed_trees <- 1
  } else {
    no_seed_trees <- 0
  }


  #age function
  agef <- 2*(1/((1+exp(-0.3*age))^-0.5))

  if(regeneration_method=="natural"){
    #arcsin transformed dependent variable.
    arcsinSLH <- 1.7413 +
      -0.0163*(((altitude/100)^2)*northern_sweden)+
      0.6863*agef+
      0.6663*proportion_cultivated+
      -0.1500*N_full+
      0.0218*ground_water_deep*swe_map+
      0.2702*ground_water_shallow+
      -0.7552*gotland+
      0.3310*orebro+
      -0.1275*SYZ_county_indicator+
      0.2692*scarification+
      0.2030*burnt+
      -0.1484*no_treatment+
      -0.0947*uncleaned+
      0.1596*number_seed_trees+
      0.0175*swe_map*no_seed_trees+
      -0.0379*jonsbon+
      0.1888*(1/area)+
      0.1075*northern_sweden+
      -0.00619*swe_map

    #Re-transformation for Arc-sin Bias corrected SLH.
    SLH <- 0.056 + 0.887*(sin(arcsinSLH/2)^2)
    W <- sin(sin(-0.11 + 1.671*asin(SLH^0.5) - 0.583*asin(SLH^0.5)^2))

    return(c(SLH,W))
  } else if(method%in%c("cultivated-sown","cultivated-planted")){

    if(main_species=="Picea abies"){
      main_species_is_spruce <- 1
    } else {
      main_species_is_spruce <- 0
    }

    if(method=="cultivated-sown"){
      sown <- 1
    } else {
      sown <- 0
    }

    arcsinSLH <- 3.0707+
      +0.4358*(1/age)+
      -0.0614*jonsbon+
      -0.0514*(((altitude/100))*northern_sweden)+ #OBSERVE not squared!
      -0.3591*square_spacing+ #spacing squared or squared spacing?
      0.0760*main_species_is_spruce+
      0.1141*burnt+
      0.0757*scarification+
      -0.0675*sown+
      0.2597*northern_sweden+
      4.7901*(1/swe_map)+
      0.2178*(orebro)+
      -0.1500*(N_full)

    #Re-transformation for Arc-sin Bias corrected SLH.

    SLH <- 0.037 + 0.926*(sin(arcsinSLH/2)^2)
    W = sin(sin(-0.058 + 1.380*asin(SLH^0.5) - 0.315*asin(SLH^0.5)^2 - 0.031*northern_sweden))
    return(c(SLH,W))
  }

}




#' Height development in young stands NYSKOG
#'
#' @description A group of functions for mean height development in young stands of scots pine, norway spruce, birch (B. pubescens), birch (B. pendula), aspen, beech, oak or pinus contorta.
#'
#' @param species One of : Pinus sylvestris, Picea abies, Betula pendula, Betula pubescens, Populus tremula, Fagus sylvatica, Quercus robur, Pinus contorta.
#' @param H100_Spruce Site index H100 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()],[forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#' @param H100_Pine Site index H100 [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param total_age Total age of stand.
#'
#' @return Estimated height, m.
#' @export
Elfving_2010_NYSKOG_young_stand_height_development <- function(species, H100_Spruce, H100_Pine, total_age){
  warning('TODO: H100 Beech, H100 Oak? Interm solution with H100_Spruce for Beech and H100_Pine for Oak.')
  X <- log(total_age)

  if(species=="Pinus sylvestris"){
    mean_height = 7 + (-0.57 -0.05*H100_Pine)*X + (-0.28 + 0.0094*H100_Pine)*(X^2)
  } else if (species=="Picea abies"){
    mean_height = (6.27 + (12.1/si)) + (-0.262 - 0.0575*H100_Spruce + 0.00088*(H100_Spruce^2))*X + (-0.323 - 0.134*(-0.262 - 0.0575*H100_Spruce + 0.00088*(H100_Spruce^2)))*(X^2)
  } else if (species=="Betula pubescens"){
    mean_height = (6.836 + 0.03165*H100_Pine - 0.002757*(H100_Pine^2)) + (-2.694+0.4937*(6.836 + 0.03165*H100_Pine - 0.002757*(H100_Pine^2)) - 0.05331*((6.836 + 0.03165*H100_Pine - 0.002757*(H100_Pine^2))^2))*X
  } else if (species=="Betula pendula"){
    si2 <- H100_Pine+1.5
    mean_height = (6.836 + 0.03165*si2 - 0.002757*(si2^2)) + (-2.694+0.4937*(6.836 + 0.03165*si2 - 0.002757*(si2^2)) - 0.05331*((6.836 + 0.03165*si2 - 0.002757*(si2^2))^2))*X
  } else if (species=="Populus tremula"){
    mean_height = (10.024 - 0.1664*H100_Pine) + (-4.093 + 0.1605*H100_Pine - 0.0025*(H100_Pine^2))
  } else if (species=="Fagus sylvatica"){
    si <- 7.4 + (0.755*H100_Spruce) - (0.00268*(H100_Spruce^2)) #H100 Beech?
    mean_height = (6.27 + (12.1/si)) + (-0.262 - 0.0575*si + 0.00088*(si^2))*X + (-0.323 - 0.134*(-0.262 - 0.0575*si + 0.00088*(si^2)))*(X^2)
  } else if (species=="Quercus robur"){
    si <- 6.5 + 0.5*H100_Pine #H100 Oak?
    mean_height = (6.836 + 0.03165*si - 0.002757*(si^2)) + (-2.694+0.4937*(6.836 + 0.03165*si - 0.002757*(si^2)) - 0.05331*((6.836 + 0.03165*si - 0.002757*(si^2))^2))*X
  } else if (species=="Pinus contorta"){
    si <- 0.888 + (1.336*H100_Pine) - (0.0094*(H100_Pine^2))
    mean_height = 7 + (-0.57 -0.05*si)*X + (-0.28 + 0.0094*si)*(X^2)
  }

  return(
    mean_height
  )

}



#' Determine age at breast height for young trees
#'
#' @source Nyström, K. & Söderberg, U. 1987. Tillväxtberäkningen för ungskog i Hugin-systemet. SLU, Inst. f. skogsskötsel. Arbetsrapport 18. URL: \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @description
#' Elfving 2010: Functions for determination of age at breast height for young trees were presented by
#' Nyström & Söderberg (1987). They were based on data from un-damaged sample-trees with
#' h>1.3 m from the Hugin young stand survey, in total about 10 000 trees. Sampling
#' probabilities were used as weights in the regressions
#'
#'
#' \strong{Summary Statistics, Pine}
#' \itemize{
#' \item R^2 : 0.96
#' \item Sres : 2.1
#' }
#'
#' \strong{Summary Statistics, Spruce}
#' \itemize{
#' \item R^2 : 0.92
#' \item Sres : 3.0
#' }
#'
#' \strong{Summary Statistics, Birch}
#' \itemize{
#' \item R^2 : 0.91
#' \item Sres : 3.1
#' }
#'
#' @param height Total tree height in meters
#' @param species One of 'Pinus sylvestris', 'Picea abies', 'Betula pendula', 'Betula pubescens'
#' @param mean_height Arithmetic mean height (m) of future crop trees (1600 stems per ha)
#' @param H100_Spruce Meters. for Spruce [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()],[forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#' @param H100_Pine Meters. for Pine and Birch [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param source_sprout 1 for sprout, else 0 (seed).
#' @export
#'
#' @return age at breast height of young trees


Nystrom_1987_age_of_individual_young_trees <- function(height, species, mean_height, H100_Spruce, H100_Pine, source_sprout=0){

  #Total tree height should be in dm.
  height_dm <- height*10
  #Unclear, but assumed that also mean height should be so.
  mean_height <- mean_height*10

  H100_Spruce = H100_Spruce*10
  H100_Pine = H100_Pine*100


  if(species=="Pinus sylvestris"){
    age = 2.548*log(height_dm-12) + (68.23 * ((height_dm/H100_Pine)^2)) + (-0.003*(height_dm*H100_Pine/10)) + 24.54*((mean_height/H100_Pine)^2) + 0.572*((height_dm-mean_height)/mean_height)
    } else if (species=="Picea abies"){
      age = 2.643*log(height_dm-12) + (78.64 * ((height_dm/H100_Spruce)^2)) + (-0.002*(height_dm*H100_Spruce/10)) + 28.86*((mean_height/H100_Spruce)^2) + 0.510*((height_dm-mean_height)/mean_height)
      } else if (startsWith(species,"Betula")){
        peb <- ifelse(species=="Betula pendula",1,0)
        age = 1.988*log(height_dm-12) + (61.89 * ((height_dm/H100_Pine)^2)) + (-1.490*(mean_height/H100_Pine)) + (-15.72*peb*((height_dm/H100_Pine)^2)) + 0.014*source_sprout*(height_dm-50)
        }
  return(age)
}


#' Estimate DBH from height, stand density, etc.
#' @source Nyström, K. & Söderberg, U. 1987. Tillväxtberäkningen för ungskog i Hugin-systemet. SLU, Inst. f. skogsskötsel. Arbetsrapport 18. URL: \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @description Based on plots from the HUGIN young stand survey re-measured 1981-1983, validated with plots from 1984.
#'
#'
#' Summary Statistics: Pine
#' \itemize{
#' \item n = 9524
#' \item R = 0.91
#' \item sf = 0.42
#' }
#'
#' Summary Statistics: Spruce
#' \itemize{
#' \item n = 11706
#' \item R = 0.95
#' \item sf = 0.36
#' }
#'
#' Summary Statistics Birch
#' \itemize{
#' \item n=5476
#' \item R = 0.93
#' \item sf = 0.47
#' }
#'
#' @details To calculate sum of squared heights the number of stems has been included.
#'
#' @param species One of: 'Pinus sylvestris','Picea abies' or 'Betula'
#' @param height Tree height, meters
#' @param dominant_height Mean height, m, of the three highest trees on the plot.
#' @param stems Number of stems per ha.
#' @param proportion_broadleaves Proportion (of stems?) which are Broadleaf.
#' @param sprouts number of sprouts exceeding 1/2 of dominant_height.
#' @param natural_regeneration Boolean.
#' @param PCT Boolean. Pre-Commercial thinning performed < 10 years ago?
#' @param PCT_years Numeric. Number of years since PCT was performed.
#' @param latitude Latitude, degrees N.
#' @param altitude Meters above sea level.
#' @param distance_to_coast Numeric. (If site is more than 5 (sic?) (TODO: check 50 km) from coast. else 0. E.g. [forester::coast_distance()])
#' @param H100_Pine Site index for Scots Pine, e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param vegetation Integer. Code from Swedish NFI Field Layer classification: [forester::Sweden_vegetation_types()]
#'
#' @return DBH, cm of individual tree.
#' @export

Nystrom_1987_DBH_of_individual_young_trees <- function(
    species='Picea abies',
    height,
    mean_height,
    dominant_height,
    stems,
    proportion_broadleaves,
    sprouts,
    natural_regeneration,
    PCT,
    PCT_years,
    latitude,
    altitude,
    distance_to_coast,
    H100_Pine,
    vegetation){

  distance_to_coast = ifelse(distance_to_coast<50,1,0)

  shrub = ifelse(vegetation%in%c(2,3,5,6,13,14,15,16),1,0) #Including high and low herbs? Empetrum? Poor shrubs?
  grass = ifelse(vegetation%in%c(8,9),1,0)
  dateBool = 0 #Always set to 0 in implementation.
  main_species = ifelse(startsWith(main_species,prefix = 'Betula'),'Betula',main_species)
  k = switch(main_species,
             'Picea abies' = 1.0,
             'Pinus sylvestris' = 1.0,
             'Betula'=1.1)

  heightDiff = ifelse((dominant_height - height)>0.1,dominant_height - height, 0.1)
  squaredHeightsSum = stems * mean_height^2
  BroadleafProp = squaredHeightsSum*proportion_broadleaves*10^-5
  CF = 1+10*heightDiff*(BroadleafProp+0.1)
  #1.5708 as radians is 0.02741563 (units::set_units(units::as_units(1.5708,'degrees'),'radians'))
  Idel = BroadleafProp*0.02741563

  DBH = switch(main_species,
               'Pinus sylvestris' = {
                 sqrt(exp(
                  2.098 * log(height-k) +
                  3.474*dateBool*(1-height)+
                 -0.1473*squaredHeightsSum+
                 -0.176*log(squaredHeightsSum+0.1)+
                 -0.098*log(CF)+
                 +0.136*sin(Idel)+
                 -0.312*natural_regeneration+
                  0.0759*natural_regeneration*sqrt(height)+
                 -0.171*PCT/(1+PCT_years)+
                  0.022*PCT*log(10+PCT_years)+
                  0.014*latitude+
                 -0.0466*altitude/100+
                 +0.0126*(altitude/100)^2+
                  0.096*distance_to_coast+
                  0.088*grass+
                  0.261+
                  0.068 #corr log bias
                 ))
               },
               'Picea abies' = {
                 sqrt(exp(
                   1.868*log(height-k)+
                   0.0147*(height-k)^2+
                   1.582*dateBool*(1-height)+
                  -0.0525*squaredHeightsSum+
                  -0.109*log(squaredHeightsSum+0.1)+
                  -0.058*log(CF)+
                   0.081*natural_regeneration+
                  -0.093*PCT/(1+PCT_years)+
                   0.013*PCT*log(10+PCT_years)+
                   0.014*latitude+
                   0.068*(altitude/100)+
                   0.0045*(altitude/100)^2+
                   0.103*distance_to_coast
                  -0.053*shrub+
                   0.186+
                   0.061 #corr log bias
                 ))

               },
               'Betula' = {
                 sqrt(exp(
                   2.25*log(height-k)+
                   0.0119*height+
                  -0.0166*squaredHeightsSum+
                  -0.119*log(CF)+
                  -0.071*PCT/(1+PCT_years)+
                   0.033*PCT*log(10+PCT_years)+
                  -0.00029*sprouts*height^2+
                   0.120*distance_to_coast+
                  -0.0096*H100_Pine+
                   0.867+
                   0.090
                 ))

               })

  return(DBH)

}

