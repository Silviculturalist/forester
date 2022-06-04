#' Calculate biomass of tree according to Petersson (1999)
#'
#' @source Petersson, H. (1999). Biomassafunktioner för trädfaktorer av tall,
#'  gran och björk i Sverige. Eng.: Biomass functions for tree factors of
#'  Pine, Norway Spruce and Birch in Sweden. Report #59. Skogliga konsekvensanalyser 1999.
#'  Dept. of Forest Resources and Geomatics. Swedish University of Agricultural Sciences.
#'  ISSN 1401-1204, ISRN SLU-SRG-AR-59-SE. Umeå, Sweden.
#'
#'  <https://pub.epsilon.slu.se/8782/1/Petersson_H_120502.pdf>
#'
#' @description Forests at high altitudes and close to coast are not represented.
#'
#' @param species One of "Picea abies","Pinus sylvestris","Betula pendula" or
#' "Betula pubescens".
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, in metres.
#' @param age_at_breast_height Age at breast height.
#' @param SI Site index of dominating tree species according to Hägglund &
#' Lundmark (1977).
#' @param dominant_species For which the SI was calculated. "Picea abies" or
#' "Pinus sylvestris".
#' @param five_years_radial_increment.mm five years radial increment, in mm.
#' @param peat TRUE if soil type is peat, otherwise FALSE. Soil type is peat
#' if the tree plot is more than 30 cm deep peat.
#' @param latitude latitude
#' @param longitude longitude
#' @param espg_in EPSG of Latitude and Longitude, default=4326.
#' @param altitude Metres above sea level.
#'
#' @return List of weights, in g.
#' @export
Petersson_1999_biomass <- function(species,
                                   diameter.cm,
                                   height.m,
                                   age_at_breast_height,
                                   SI,
                                   dominant_species,
                                   five_years_radial_increment.mm,
                                   peat,
                                   latitude,
                                   longitude,
                                   epsg_in=4326,
                                   altitude
){
  #Check tree species is supported.
  if(!(species%in%c("Picea abies",
                    "Pinus sylvestris",
                    "Betula pendula",
                    "Betula pubescens"))){
    stop("Tree species not supported.")
  }

  #Check age requirement.
  if(age_at_breast_height<5){
    stop("Tree must be at least 5 years at breast height.")
  }

  if(!(dominant_species%in%c("Picea abies","Pinus sylvestris"))){
    stop("SI must be calculated for either 'Picea abies' or 'Pinus sylvestris'")
  }

  #Convert input crs to RT90.

  if(epsg_in!=3021){

    message("Input is reprojected to RT90 2.5 gon V,  EPSG:3021")

    geom_point <- as.matrix(data.frame(x=longitude,y=latitude))

    geom_point <- terra::vect(geom_point, crs=paste0("epsg:",epsg_in))

    geom_point <- terra::project(geom_point, paste0("epsg:",3021)) #RT90

    longitude <- terra::geom(geom_point)[3]
    latitude <- terra::geom(geom_point)[4]

  }








  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }

  if(species=="Pinus sylvestris"){
    pine <- TRUE
  } else {
    pine <- FALSE
  }

  #cm to mm.
  diameter.cm <- diameter.cm*10

  #height to dm.
  height.m <- height.m*10

  #radial increment from mm to 0.1 mm
  five_years_radial_increment.mm <- five_years_radial_increment.mm*10



  if(species=="Picea abies"){
    dry_weight_of_stem_above_bark <-
      exp(
      -6.839310+
      +3.578450*log(diameter.cm + 25)+
      -0.003042*diameter.cm+
      +0.093033*log(five_years_radial_increment.mm)+
      -0.002763*five_years_radial_increment.mm+
      +0.111347*log(age_at_breast_height)+
      +0.012148*SI*picea+
      +0.011586*SI*pinus+
      -0.000020194*latitude+
      +((0.17069^2)/2) #Baskerville 1972 log. bias correction.
      )



    dry_weight_of_bark <-
      exp(
      -4.084706+
      +2.397166*log(diameter.cm + 10)+
      -0.066053*log(five_years_radial_increment.mm)+
      +0.151696*log(age_at_breast_height)+
      +((0.22759^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_needles <-
      exp(
      -1.130238+
      +1.485407*log(diameter.cm)+
      +0.514648*log(five_years_radial_increment.mm)+
      +0.206901*log(age_at_breast_height)+
      +((0.30852^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_branches_including_needles_leaves_cones <-
      exp(
      -0.718621+
      +1.740810*log(diameter.cm)+
      +0.348379*log(five_years_radial_increment.mm)+
      +0.180503*log(age_at_breast_height)+
      +((0.28825^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_dead_branches <-
      exp(
      -1.763738+
      +2.616200*log(diameter.cm)+
      -0.745459*log(five_years_radial_increment.mm)+
      -0.359509*log(age_at_breast_height)+
      +((0.79373^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_stump_and_root_fraction_larger_than_5_cm_diameter <-
      exp(
      -1.980469+
      +2.339527*log(diameter.cm)+
      +0.342786*peat+
      -0.224812*pine+
      +((0.28283^2)/2) #Baskerville 1972 log. bias correction.
      )


    dry_weight_above_stump <-
      exp(
      -0.437361+
      +2.446692*log(diameter.cm+9)+
      +0.065779*log(five_years_radial_increment.mm)+
      +0.102290*log(age_at_breast_height)+
      -0.000021633*latitude+
      +((0.15492^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_total <-
      exp(
      -0.614093+
      +2.425997*log(diameter.cm+8)+
      +0.081636*log(five_years_radial_increment.mm)+
      +0.128027*log(age_at_breast_height)+
      -0.000015810*latitude+
      +((0.15985^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_excluding_root_fraction_less_than_5_cm_diameter <-
      exp(
      -0.766217+
      +2.491024*log(diameter.cm+8)+
      +0.070526*log(five_years_radial_increment.mm)+
      +0.113514*log(age_at_breast_height)+
      -0.000017876*latitude+
      +((0.16274^2)/2) #Baskerville 1972 log. bias correction.
      )

    returnlist <- list(
      "Dry weight of stem above bark" = dry_weight_of_stem_above_bark,
      "Dry weight of bark" = dry_weight_of_bark,
      "Dry weight of needles"=dry_weight_of_needles,
      "Dry weight of living branches, needles and cones"=dry_weight_of_branches_including_needles_leaves_cones,
      "Dry weight of dead branches" = dry_weight_of_dead_branches,
      "Dry weight of stump and root fraction larger than 5 cm in diameter" =dry_weight_of_stump_and_root_fraction_larger_than_5_cm_diameter,
      "Dry weight above stump"=dry_weight_above_stump,
      "Dry weight total"= dry_weight_total,
      "Dry weight excluding root fraction less than 5 cm in diameter"=dry_weight_excluding_root_fraction_less_than_5_cm_diameter

    )

    return(returnlist)



  } else if(species=="Pinus sylvestris"){

    dry_weight_of_stem_above_bark <-
      exp(
      -7.674621+
      +3.155671*log(diameter.cm+25)+
      -0.002197*diameter.cm+
      +0.084427*log(five_years_radial_increment.mm)+
      -0.002665*five_years_radial_increment.mm+
      +0.253227*log(age_at_breast_height)+
      +0.028478*SI*picea+
      +0.031435*SI*pinus+
      +0.000008342*latitude+
      +((0.17803^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_bark <-
      exp(
      -1.340149+
      +2.209719*log(diameter.cm+13)+
      -0.001986*log(five_years_radial_increment.mm)+
      -0.000024146*latitude+
      +((0.26942^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_needles <-
      exp(
      -2.597267+
      +1.506121*log(diameter.cm)+
      +0.571366*log(five_years_radial_increment.mm)+
      +0.208130*log(age_at_breast_height)+
      +0.000870*altitude+
      +((0.35753^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_branches_including_needles_leaves_cones <-
      exp(
      -2.533220+
      +1.989129*log(diameter.cm)+
      +0.387203*log(five_years_radial_increment.mm)+
      +0.105215*log(age_at_breast_height)+
      +((0.34938^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_dead_branches <-
      exp(
      +1.596001+
      +2.441173*log(diameter.cm)+
      -0.437497*log(five_years_radial_increment.mm)+
      -0.711616*log(age_at_breast_height)+
      -0.001358*altitude+
      -0.000129*longitude+
      +((0.76730^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_above_stump <-
      exp(
      -2.032666+
      +2.413856*log(diameter.cm+6)+
      +0.130304*log(age_at_breast_height)+
      +0.011834*SI*picea+
      +0.013668*SI*pinus+
      +((0.15651^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_total <-
      exp(
      -1.507568+
      +2.449121*log(diameter.cm+5)+
      +0.104243*log(age_at_breast_height)+
      -0.000321*altitude+
      +((0.16332^2)/2) #Baskerville 1972 log. bias correction.
      )


    dry_weight_excluding_root_fraction_less_than_5_cm_diameter <-
      exp(
      -1.756201+
      +2.483808*log(diameter.cm+5)+
      +0.107190*log(age_at_breast_height)+
      -0.000325*altitude+
      +((0.16086^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_stump_and_root_fraction_larger_than_5_cm_diameter <-
      exp(
      -1.980469+
      +2.339527*log(diameter.cm)+
      +0.342786*peat+
      -0.224812*pine+
      +((0.28283^2)/2) #Baskerville 1972 log. bias correction.
      )

    returnlist <- list(
      "Dry weight of stem above bark" = dry_weight_of_stem_above_bark,
      "Dry weight of bark" = dry_weight_of_bark,
      "Dry weight of needles" = dry_weight_of_needles,
      "Dry weight of living branches, needles and cones" = dry_weight_of_branches_including_needles_leaves_cones,
      "Dry weight of dead branches" = dry_weight_of_dead_branches,
      "Dry weight above stump" = dry_weight_above_stump,
      "Dry weight total" = dry_weight_total,
      "Dry weight excluding root fraction less than 5 cm in diameter" = dry_weight_excluding_root_fraction_less_than_5_cm_diameter,
      "Dry weight of stump and root fraction larger than 5 cm in diameter" = dry_weight_of_stump_and_root_fraction_larger_than_5_cm_diameter
    )

    return(returnlist)

  } else if(species%in%c("Betula pendula","Betula pubescens")){

    dry_weight_of_stem_above_bark <-
      exp(
      -3.091932+
      +2.479648*log(diameter.cm+7)+
      +0.243747*log(age_at_breast_height)+
      +0.022185*SI*picea+
      +0.022955*SI*pinus+
      +((0.19827^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_bark <-
      exp(
      -3.244490+
      +2.525420*log(diameter.cm+18)+
      +0.329744*log(age_at_breast_height)+
      -0.000030180*latitude+
      +((0.25483^2)/2) #Baskerville 1972 log. bias correction.
      )


    dry_weight_living_branches_and_leaves <-
      exp(
      -2.782537+
      +2.276815*log(diameter.cm)+
      +0.228528*log(five_years_radial_increment.mm)+
      +((0.45153^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_of_dead_branches <-
      exp(
      -2.059091+
      +1.657683*log(diameter.cm)+
      +((1.12848^2)/2) #Baskerville 1972 log. bias correction.
      )

    dry_weight_above_stump <-
      exp(
      -0.423749+
      +2.574575*log(diameter.cm+8)+
      +0.090419*log(age_at_breast_height)+
      -0.000026862*latitude+
      +((0.17071^2)/2) #Baskerville 1972 log. bias correction.
      )

    returnlist <- list(
      "Dry weight of stem above bark" = dry_weight_of_stem_above_bark,
      "Dry weight of bark" = dry_weight_of_bark,
      "Dry weight of living branches and leaves"= dry_weight_living_branches_and_leaves,
      "Dry weight of dead branches" = dry_weight_of_dead_branches,
      "Dry weight above stump" = dry_weight_above_stump
    )

    return(returnlist)

  }



}
