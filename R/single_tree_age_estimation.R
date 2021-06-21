#' Functions for estimation of single tree ages at breast height
#'
#' @description Presented in Elfving, B. (2003) Ålderstilldelning till enskilda träd i skogliga tillväxtprognoser. SLU, inst. f. skogsskötsel. Arbetsrapport 182.
#'
#'
#' @param diameter diameter at breast height, cm
#' @param diameter_ba diameter of basal area weighted mean tree, cm.
#' @param latitude latitude
#' @param altitude altitude, metres above sea level.
#' @param lnd log mean diameter of co-dominant trees. Output of mean_diam_codominant.
#' @param total_stand_age total stand age
#' @param SIS site index according site factors, given as top height at age 100, m.
#' @param total_ba As estimated with relascope in field. m^2 / ha
#' @param species Tree species
#' @param field_vegetation type, FsKod.
#' @param even_aged TRUE / FALSE,  above 80 percent of volume within 20 yr span.
#' @param county Swedish county
#' @param ditch TRUE / FALSE
#'
#' @return age at breast height
#' @export
#'

singleTreeAgeEstimation <- function(diameter, diameter_ba, latitude, altitude, lnd, total_stand_age, SIS, total_ba, species, field_vegetation, even_aged, county, peat, ditch){
  #Standard or Undergrowth tree?
  if(diameter > 1.8*exp((lnd+8))){
    standard_tree <-  TRUE
  } else{
    standard_tree <- FALSE
  }

  if(diameter < 0.4*(exp(lnd))){
    undergrowth_tree <- TRUE
  } else{
    undergrowth_tree <- FALSE
  }

  if(diameter>=0.4*(exp(lnd)) & diameter <= 1.8*exp(lnd+8)){
    undergrowth_tree <- FALSE
    standard_tree <- FALSE
  }

  #Plot in gotland?
  if(county=="Gotland"){
    gotland <- TRUE
  } else {
    gotland <- FALSE
  }

  #Rich is field veg. is herbs or grass. Poor if lichens, heather, etc.
  if(field_vegetation<=9){
    rich <- TRUE
    poor <- FALSE} else {
      rich <- FALSE
      poor <- TRUE
    }


  temperature_sum <- forester::odin_tsum_1983(lat_degree = latitude, masl=altitude)

  #Species coding
  if(species=="Picea abies"){
    piceaabies <- TRUE
  } else if(species=="Pinus sylvestris"){
    pinussylvestris <- TRUE
  } else if(species=="Fagus sylvatica" | species=="Quercus robur"){
    fagusQuercus <- TRUE
  } else if(species %in% c("Ulmus glabra","Tilia cordata","Acer platanoides","Prunus avium") ==TRUE | grepl("^Salix",species)==TRUE){
    trivial <- TRUE
  } else if(species %in% c("Carpinus betulus","Sorbus aucuparia")){
    broadleaves <- TRUE
  }

  #reld
  reld <- diameter / diameter_ba

  # ln d mean diameter of codominant trees
  #lnd <- -0.9231+(1.0032*log(total_stand_age))-(0.00701*total_stand_age)-(4.005/SIS)+(0.0186*SIS)-(1.882/total_ba) + 0.036
  lnd <- Elfving_2003_mean_diameter_codominant_trees(total_stand_age=total_stand_age, SIS=SIS, total_ba=total_ba)


if(is.na(total_stand_age)){
    #Without stand age
    age_13 <- 2.2552+(1.2108*lnd)-(1.5115*reld)+(0.3962*reld^2)-(0.1822*pinussylvestris)+(0.1416*fagusQuercus)-(0.1276*trivial)-(0.0278*SIS)-(0.00471*SIS*piceaabies)-(0.00000304*(SIS^2))-(0.3921*temperature_sum)+(0.1621*standard_tree)+(0.2950*undergrowth_tree)+(0.1908*log(total_ba+1))+(0.1994*gotland)-(0.2071*rich)+(0.0320*poor)-(0.0918*ditch)+(0.1410*peat) +0.062
    return(exp(age_13))

  } else if(standard_tree == TRUE){
    #If tree belongs to standards.
    age_13 <- 1.7993+ (1.1489*lnd) -(0.01841*diameter) +(0.00634*total_stand_age)-(0.0346*SIS)-(0.2254*trivial) +0.045
    return(exp(age_13))

  } else if(even_aged == FALSE){
    #If uneven aged stand
    age_13 <- 0.4181+(0.5572*lnd)-(0.01803*diameter)+(0.1713*diameter/exp(lnd))+(0.3923*log(total_stand_age))+(0.00367*total_stand_age)+(0.1051*fagusQuercus)-(0.2228*trivial)-(0.01171*SIS)+(0.0002851*SIS*diameter)-(0.00135*SIS*piceaabies)+(0.8362*temperature_sum)-(0.4469*(temperature_sum^2))+(0.0469*log(total_ba))-(0.0668*rich)+(0.0313*poor)-(0.0379*ditch)+(0.1449*gotland) +0.042
    return(exp(age_13))

  } else if(species=="Pinus sylvestris" & even_aged == TRUE){
    #If Pine
    age_13 <- -1.6957+0.2982*log(diameter)-0.00801*diameter+0.0726*diameter/exp(lnd)+1.1587*log(total_stand_age)-0.0007699*(total_stand_age)+0.0310*log(total_ba) +0.012
    return(exp(age_13))

  } else if(species=="Picea abies" & even_aged == TRUE){
    #If Spruce

    age_13 <- -1.3834+0.3790*log(diameter)-0.01003*diameter+0.1283*reld+1.0201*log(total_stand_age)+0.0175*log(total_ba) +0.018
    return(exp(age_13))

  } else if(grepl("^Betula",species)==TRUE & even_aged == TRUE){
    #If Birch

    age_13=0.1006+0.5207*log(diameter)-0.00793*diameter-0.0814*reld+0.6701*log(total_stand_age)-0.01053*SIS-0.0990*rich+0.0394*log(total_ba+1) +0.038
    return(exp(age_13))

  } else if(trivial == TRUE & even_aged == TRUE | broadleaves == TRUE & even_aged == TRUE){
    #If Trivial or Broadleaves

    age_13=0.138+0.6469*log(diameter)-0.265*diameter/exp(lnd)+0.5053*log(total_stand_age)+0.1939*broadleaves-0.1787*trivial +0.034

  } else if(!is.na(total_stand_age)){
    #All species with stand age
    age_13 <- -1.4692+(0.2989*lnd)-(0.01132*diameter)+(0.2594*diameter/(exp(lnd)))+(1.019*ln(total_stand_age))-(0.0948*pinussylvestris)-(0.0980*trivial)+(0.00465*SIS)-(0.00372*SIS*piceaabies)+(0.3350*temperature_sum)-(0.1551*(temperature_sum^2))+(0.5684*standard_tree)-(0.2103*undergrowth_tree)-(0.0312*even_aged)+0.0449*log(total_ba)+(0.0344*gotland)-(0.1388*rich)+(0.0614*poor) +0.028
    return(exp(age_13))

  }
}


