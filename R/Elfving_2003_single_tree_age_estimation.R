#' Functions for estimation of single tree ages at breast height
#'
#' @description Presented in Elfving, B. (2003) Ålderstilldelning till enskilda träd i skogliga tillväxtprognoser. SLU, inst. f. skogsskötsel. Arbetsrapport 182.
#'
#'
#' @param diameter_cm diameter at breast height, cm
#' @param diameter_ba diameter of basal area weighted mean tree, cm.
#' @param mean_diameter_codominant_trees Mean Diameter of the co-dominant trees, e.g. [forester::Elfving_2003_mean_diameter_codominant_trees()]
#' @param latitude latitude
#' @param altitude altitude, metres above sea level.
#' @param total_stand_age total stand age
#' @param SIS_100 Site index from stand variables, metres. e.g. [forester::SIS_estimate()]
#' @param Basal_area_plot As estimated with relascope in field. m^2 / ha
#' @param species One of: "Pinus sylvestris", "Picea abies", "Betula pendula", "Betula pubescens", "Fagus sylvatica", "Quercus robur","Ulmus glabra","Tilia cordata","Acer platanoides","Prunus avium","Salix ..","Carpinus betulus" or "Sorbus aucuparia"
#' @param vegetation vegetation Vegetation type according to follows Swedish National forest inventory FALTSKIKT:
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
#' @param even_aged TRUE / FALSE,  above 80 percent of volume within 20 yr span.
#' @param county Swedish county
#' @param ditch TRUE / FALSE
#'
#' @return A list, with element 1 : Age at breast height, 2: Boolean if tree is a standard, 3: Boolean if tree is suppressed.
#' @export
#'

Elfving_2003_single_tree_age_estimation <- function(diameter_cm, diameter_ba, mean_diameter_codominant_trees, latitude, altitude, lnd, total_stand_age, SIS, Basal_area_plot, species, vegetation, even_aged, county, peat, ditch){

  #lnd
  lnd <- log(mean_diameter_codominant_trees)

  #Standard or Undergrowth tree?
  standard_tree <- ifelse(diameter_cm > 1.8*(exp(lnd)+8),TRUE,FALSE)

  undergrowth_tree <- ifelse(diameter_cm < 0.4*(exp(lnd)),TRUE,FALSE)

  #Plot in gotland?
  gotland <- ifelse(county=="Gotland",TRUE,FALSE)

  #Rich if vegetation is herbs or grass. Poor if lichens, heather, etc.
  rich <- ifelse(vegetation<=9,TRUE,FALSE)
  poor <- ifelse(vegetation>9,TRUE,FALSE)

  temperature_sum <- forester::odin_tsum_1983(lat_degree = latitude, masl=altitude)

  #Species coding

  ifelse(
    species == "Picea abies",
    assign("piceaabies", TRUE),
    ifelse(
      species == "Pinus sylvestris",
      assign("pinussylvestris", TRUE),
      ifelse(
        species %in% c("Fagus sylvatica", "Quercus robur"),
        assign("fagusQuercus", TRUE),
        ifelse(
          species %in% c(
            "Ulmus glabra",
            "Tilia cordata",
            "Acer platanoides",
            "Prunus avium"
          ) | grepl("^Salix", species),
          assign("trivial", TRUE),
          if (species %in% c("Carpinus betulus", "Sorbus aucuparia")) {
            assign("broadleaves", TRUE)
          }
        )
      )
    )
  )

  #Relative diameter
  reld <- diameter_cm / diameter_ba



if(is.na(total_stand_age)){
    #Without stand age
    age_13 <- 2.2552+(1.2108*lnd)-(1.5115*reld)+(0.3962*reld^2)-(0.1822*pinussylvestris)+(0.1416*fagusQuercus)-(0.1276*trivial)-(0.0278*SIS)-(0.00471*SIS*piceaabies)-(0.00000304*(SIS^2))-(0.3921*temperature_sum)+(0.1621*standard_tree)+(0.2950*undergrowth_tree)+(0.1908*log(Basal_area_plot+1))+(0.1994*gotland)-(0.2071*rich)+(0.0320*poor)-(0.0918*ditch)+(0.1410*peat) +0.062
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(standard_tree == TRUE){
    #If tree belongs to standards.
    age_13 <- 1.7993+ (1.1489*lnd) -(0.01841*diameter_cm) +(0.00634*total_stand_age)-(0.0346*SIS)-(0.2254*trivial) +0.045
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(even_aged == FALSE){
    #If uneven aged stand
    age_13 <- 0.4181+(0.5572*lnd)-(0.01803*diameter_cm)+(0.1713*diameter_cm/exp(lnd))+(0.3923*log(total_stand_age))+(0.00367*total_stand_age)+(0.1051*fagusQuercus)-(0.2228*trivial)-(0.01171*SIS)+(0.0002851*SIS*diameter_cm)-(0.00135*SIS*piceaabies)+(0.8362*temperature_sum)-(0.4469*(temperature_sum^2))+(0.0469*log(Basal_area_plot))-(0.0668*rich)+(0.0313*poor)-(0.0379*ditch)+(0.1449*gotland) +0.042
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(species=="Pinus sylvestris" & even_aged == TRUE){
    #If Pine
    age_13 <- -1.6957+0.2982*log(diameter_cm)-0.00801*diameter_cm+0.0726*diameter_cm/exp(lnd)+1.1587*log(total_stand_age)-0.0007699*(total_stand_age)+0.0310*log(Basal_area_plot) +0.012
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(species=="Picea abies" & even_aged == TRUE){
    #If Spruce

    age_13 <- -1.3834+0.3790*log(diameter_cm)-0.01003*diameter_cm+0.1283*reld+1.0201*log(total_stand_age)+0.0175*log(Basal_area_plot) +0.018
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(grepl("^Betula",species)==TRUE & even_aged == TRUE){
    #If Birch

    age_13=0.1006+0.5207*log(diameter_cm)-0.00793*diameter_cm-0.0814*reld+0.6701*log(total_stand_age)-0.01053*SIS-0.0990*rich+0.0394*log(Basal_area_plot+1) +0.038
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(trivial == TRUE & even_aged == TRUE | broadleaves == TRUE & even_aged == TRUE){
    #If Trivial or Broadleaves

    age_13=0.138+0.6469*log(diameter_cm)-0.265*diameter_cm/exp(lnd)+0.5053*log(total_stand_age)+0.1939*broadleaves-0.1787*trivial +0.034
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  } else if(!is.na(total_stand_age)){
    #All species with stand age
    age_13 <- -1.4692+(0.2989*lnd)-(0.01132*diameter_cm)+(0.2594*diameter_cm/(exp(lnd)))+(1.019*ln(total_stand_age))-(0.0948*pinussylvestris)-(0.0980*trivial)+(0.00465*SIS)-(0.00372*SIS*piceaabies)+(0.3350*temperature_sum)-(0.1551*(temperature_sum^2))+(0.5684*standard_tree)-(0.2103*undergrowth_tree)-(0.0312*even_aged)+0.0449*log(Basal_area_plot)+(0.0344*gotland)-(0.1388*rich)+(0.0614*poor) +0.028
    return(list("Age at breast height"=exp(age_13),"Standard tree"=standard_tree, "Suppressed tree"=undergrowth_tree))

  }
}


