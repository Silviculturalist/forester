#' Natural mortality, Bengtsson 1978
#'
#' @description Natural mortality. Functions based on National Forest Inventory
#'  surveys of dead trees during 1973-1975. Dead or wind-felled trees above 5 cm
#'  diameter at breast height. Avoided harsh storm damages during 1969. Largest
#'  insecurity pertains to which season the substrate in question died.
#'
#'  A 'roof' was set at a maximum of 0.9% per annum and never let the loss be less than 0.1% per annum.
#'
#'  \href{https://lagen.nu/sou/1978:7#sid293-img}{Available Online (Swedish)}
#'
#' @param standData
#'
#' @return Proportion of the raw standing volume lost per annum.
#' @export
#'
#' @examples
ba_mortality_thinned_Bengtsson_1978 <- function(standData){
  if(!class(standData)%in%c("standData")){
    stop("Input standData is not a standData object. Please make sure it is the correct class.")
  }

  #Sites as SI Pine
  si_pine <- forester::SIS_estimate(species = "Pinus sylvestris",
                                    vegetation = standData$siteData$vegetation,
                                    ground_layer = standData$siteData$ground_layer,
                                    latitude = standData$siteData$latitude,
                                    longitude = standData$siteData$longitude,
                                    altitude = standData$siteData$altitude,
                                    aspect_main = standData$siteData$aspect_main,
                                    incline_percent = standData$siteData$incline_percent,
                                    soil_moisture = standData$siteData$soil_moisture,
                                    soil_texture = standData$siteData$soil_texture,
                                    soil_depth = standData$siteData$soil_depth,
                                    lateral_water = standData$siteData$lateral_water,
                                    ditched = standData$siteData$ditched,
                                    climate_code = standData$siteData$climate_code,
                                    county = standData$siteData$county
  )

  #Main cohort age.
  stand_age <-  standData$standSummary$Main_cohort_age

  #Recommendation due to grouped regression, according to Heureka manual.
  if(stand_age<30){
    stand_age <- 30
  } else if(stand_age>120){
    stand_age <- 120
  }

  #mean diameter in stand
  mean_diameter <- standData$standSummary$Mean_diameter

  #Recommendation due to grouped regression, according to Heureka manual.
  if(mean_diameter<7){
    mean_diameter <- 7
  } else if(mean_diameter>30){
    mean_diameter <- 30
  }

  #Northern Sweden here "Norrland och Dalarna", p. 270.
  #Counties interpreted to be included see list below.
  northern_sweden <- ifelse(standData$siteData$county %in%  c("Norrbottens lappmark",
                                                              "Norrbottens kustland",
                                                              "Västerbottens lappmark",
                                                              "Västerbottens kustland",
                                                              "Västernorrland - Ångermanlands landskap",
                                                              "Västernorrland - Medelpads landskap",
                                                              "Jämtland - Jämtlands landskap",
                                                              "Jämtland - Härjedalens landskap",
                                                              "Kopparberg - Sälen-Idre",
                                                              "Kopparberg - övriga",
                                                              "Gävleborg - Hälsinglands landskap",
                                                              "Gävleborg - övriga"), 1,0)


  if (northern_sweden==1){
    if(standData$standSummary$main_species == "Pinus sylvestris"){
      a <- 0.00437
      b1 <- -0.000847
      b2 <- 0.0000214
      b3 <- 0.000104
      b4 <- -0.000000317
      b5 <- 0
      mai_pine_s_sweden <- 0

    } else if(standData$standSummary$main_species == "Picea abies"){
      a <- 0.00390
      b <- -0.000610
      b2 <- 0.0000160
      b3 <- 0.000050
      b4 <- 0
      b5 <- 0
      mai_pine_s_sweden <- 0

    } else if(tree_type(standData$standSummary$main_species)=="Deciduous"){
      a <- 0.01005
      b1 <- -0.000843
      b2 <- 0.0000176
      b3 <- -0.00000174
      b4 <- 0.000000591
      b5 <- 0
      mai_pine_s_sweden <- 0
    }
  } else if(northern_sweden==0){
    if(standData$standSummary$main_species == "Pinus sylvestris"){
      a <- 0.00760
      b1 <- -0.000430
      b2 <- 0
      b3 <- 0.000047
      b4 <- 0
      b5 <- 0.00048

      mai_pine_s_sweden <- si_to_bonitet_integrated(standData$siteData$SIS_Pinus_sylvestris,
                                                    main_species="Pinus sylvestris",
                                                    vegetation=standData$siteData$vegetation,
                                                    altitude=standData$siteData$vegetation,
                                                    county=standData$siteData$county)

    } else if(standData$standSummary$main_species == "Picea abies"){
      a <- 0.00330
      b1 <- -0.000130
      b2 <- 0
      b3 <- 0.000043
      b4 <- 0
      b5 <- 0
      mai_pine_s_sweden <- 0

    } else if(tree_type(standData$standSummary$main_species)=="Deciduous"){
      a <- 0.00460
      b1 <- -0.000200
      b2 <- 0
      b3 <- 0.000046
      b4 <- 0
      b5 <- 0
      mai_pine_s_sweden <-0

  }

  }

  return(
    a+(b1*mean_diameter) + (b2*(mean_diameter^2)) + (b3*stand_age) + (b4*(stand_age^2)) + (b5*mai_pine_s_sweden)
  )

}
