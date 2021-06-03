Marklund_1988_biomass <- function(
  species,
  diameter.cm,
  diameter.cm_3m,
  diameter.cm_5m,
  height.m,
  age_at_breast_height,
  SI,
  dominant_species,
  five_years_diameter_increment.mm,
  double_bark.mm,
  peat,
  latitude,
  longitude,
  epsg_in,
  altitude,
  crown_base_height.m,
  crown_radius.m,
  Dmax_5_m_radius,
  Dry_soil,
  Moist_soil,
  mobile_ground_water_longer_periods,
  mobile_ground_water_shorter_periods,
  mobile_ground_water_present



){

  #Check tree species is supported.
  if(!(species%in%c("Picea abies",
                    "Pinus sylvestris",
                    "Betula pendula",
                    "Betula pubescens"))){
    stop("Tree species not supported.")
  }






  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }

  #Altitude to km above sea level.
  altitude <- altitude/1000

  if(!missing(diameter.cm_5m)){
    form_quotient5 <- diameter.cm_5m/diameter.cm
    form_quotient3 <- 0
  } else if(!missing(diameter.cm_3m)){
    form_quotient3 <- diameter.cm_3m/diameter.cm
    form_quotient5
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

  relative_bark_thickness <-  (double_bark.mm / (diameter.cm*10))*100


  if(species=="Pinus sylvestris"){
    dry_weight_of_stem_above_bark_1_var_T1 <-
      -2.3388+
      +11.3264*(diameter.cm/(diameter.cm+13))

    dry_weight_of_stem_above_bark_2_var_T2 <-
      -2.6768+
      +7.5939*(diameter.cm/(diameter.cm+13))+
      +0.0151*height.m+
      +0.8799*log(height.m)


    dry_weight_of_stem_above_bark_4_var_T3 <-
      -2.6232+
      +7.7318*(diameter.cm/(diameter.cm+13))+
      +0.0139*height.m+
      +0.8625*log(height.m)+
      -0.0704*log(double_bark.mm)+
      +0.00185*age_at_breast_height

    dry_weight_of_stem_above_bark_6_var_T4 <-
      -2.4826+
      +7.9039*(diameter.cm/(diameter.cm+13))+
      +0.0184*height.m+
      +0.6939*log(height.m)+
      -0.0731*log(double_bark.mm)+
      +0.00182*age_at_breast_height+
      +0.2382*form_quotient5+
      +0.2217*form_quotient3+
      -0.1596*altitude

    dry_weight_of_stem_wood_1_var_T5 <-
      -2.2184+
      11.4219*(diameter.cm/(diameter.cm+14))

    dry_weight_of_stem_wood_2_var_T6 <-
      -2.6864+
      +7.6066*(diameter.cm/(diameter.cm+14))+
      +0.0200*height.m+
      +0.8658*log(height.m)

    dry_weight_of_stem_wood_4_var_T7 <-
      -2.5325+
      +7.8936*(diameter.cm/(diameter.cm+14))+
      +0.0231*height.m+
      +0.7887*log(height.m)+
      -0.1065*log(double_bark.mm)+
      +0.00201*age_at_breast_height

    dry_weight_of_stem_wood_6_var_T8 <-
      -2.0028+
      +7.9455*(diameter.cm/(diameter.cm+14))+
      +0.0439*height.m+
      +0.2437*log(height.m)+
      -0.0875*log(double_bark.mm)+
      +0.00172*age_at_breast_height+
      +0.7778*form_quotient5+
      +0.4855*form_quotient3+
      -0.1557*altitude

    dry_weight_of_stem_bark_var_1_T9 <-
      -2.9748+
      +8.8489*(diameter.cm/(diameter.cm+16))

    dry_weight_of_stem_bark_var_2_T10 <-
      -3.2765+
      +7.2482*(diameter.cm/(diameter.cm+16))+
      +0.4487*log(height.m)

    dry_weight_of_stem_bark_var_3_T11 <-
      -3.6065+
      +7.0834*(diameter.cm/(diameter.cm+16))+
      +0.5086*log(height.m)+
      +0.0255*relative_bark_thickness

    dry_weight_of_stem_bark_var_4_T12 <-
      -3.5076+
      +7.5295*(diameter.cm/(diameter.cm+16))+
      +0.5629*log(height.m)+
      -0.2271*log(height.m-crown_base_height.m)+
      +0.0222*relative_bark_thickness

    dry_weight_living_branches_var_1_T13 <-
      -2.8604+
      +9.1015*(diameter.cm/(diameter.cm+10))

    dry_weight_living_branches_var_2_T14 <-
      -2.5413+
      +13.3955*(diameter.cm/(diameter.cm+10))+
      -1.1955*log(height.m)

    dry_weight_living_branches_var_4_T15 <-
      -0.9137+
      +11.4337*(diameter.cm/(diameter.cm+10))+
      -1.4815*log(height.m)+
      +0.9825*log(height.m-crown_base_height.m)+
      -0.0235*latitude

    dry_weight_living_branches_var_6_T16 <-
      -2.8445+
      +9.0891*(diameter.cm/(diameter.cm+10))+
      -1.1599*log(height.m)+
      +0.6197*log(height.m-crown_base_height.m)+
      +0.5372*log(crown_radius.m)+
      +0.2011*log(age_at_breast_height)+
      +0.2142*log(five_years_diameter_increment.mm)

    dry_weight_needles_var_1_T17 <-
      -3.7983+
      +7.7681*(diameter.cm/(diameter.cm+7))

    dry_weight_needles_var_2_T18 <-
      -3.4781+
      +12.1095*(diameter.cm/(diameter.cm+7))+
      +0.0413*height.m+
      -1.5650*log(height.m)

    dry_weight_needles_var_4_T19 <-
      -2.6024+
      +9.8471*(diameter.cm/(diameter.cm+7))+
      +0.0260*height.m+
      -1.6717*log(height.m)+
      +1.0419*log(height.m-crown_base_height.m)+
      -0.0123*latitude

    dry_weight_needles_var_6_T20 <-
      -4.6082+
      +7.7998*(diameter.cm/(diameter.cm+7))+
      -0.6978*log(height.m)+
      +0.4588*log(height.m-crown_base_height.m)+
      +0.2398*log(crown_radius.m)+
      +0.2632*log(age_at_breast_height)+
      +0.4040*log(five_years_diameter_increment.mm)+
      +0.5144*altitude

    dry_weight_dead_branches_var_1_T21 <-
      -5.3338+
      +9.5938*(diameter.cm/(diameter.cm+10))

    dry_weight_dead_branches_var_2_T22 <-
      -5.8926+
      +7.1270*(diameter.cm/(diameter.cm+10))+
      -0.0465*height.m+
      +1.1060*log(height.m)

    dry_weight_dead_branches_var_4_T23 <-
      -0.9305+
      +7.1889*(diameter.cm/(diameter.cm+10))+
      -0.0850*height.m+
      +1.3027*log(height.m)+
      -0.0702*latitude+
      -1.0568*altitude

    dry_weight_dead_branches_var_7_T24 <-
      -0.8931+
      +10.3377*(diameter.cm/(diameter.cm+10))+
      -0.0865*height.m+
      +0.8701*log(height.m)+
      -0.6209*log(age_at_breast_height)+
      -0.5100*log(five_years_diameter_increment.mm)+
      +0.5846*Dmax_5_m_radius+
      -0.0577*latitude+
      -1.1226*altitude

    dry_weight_stump_and_roots_var_1_T25 <-
      -3.3913+
      +11.1106*(diameter.cm/(diameter.cm+12))

    dry_weight_stump_and_roots_var_3_T26 <-
      -1.5530+
      +11.2246*(diameter.cm/(diameter.cm+12))+
      -0.0314*SI*pinus+
      -0.0268*SI*picea+
      -0.0192*latitude

    dry_weight_stump_and_roots_T27 <-
      -3.1628+
      +10.7181*(diameter.cm/(diameter.cm+12))+
      +0.0952*log(age_at_breast_height)+
      -0.0168*SI*pinus+
      -0.0136*SI*picea+
      -0.0808*Dry_soil+
      +0.2165*Moist_soil+
      +0.3088*peat+
      -0.1655*mobile_ground_water_longer_periods+
      -0.1070*mobile_ground_water_shorter_periods+
      -0.5221*altitude


    dry_weight_stump_var_1_T28 <-
      -3.9657+
      +11.0481*(diameter.cm/(diameter.cm+15))

    dry_weight_stump_var3_T29 <-
      -2.1762+
      +9.5137*(diameter.cm/(diameter.cm+15))+
      +0.3105*log(age_at_breast_height)+
      -0.0326*latitude

    dry_weight_stump_T30 <-
      -2.5087+
      +9.4014*(diameter.cm/(diameter.cm+15))+
      +0.3220*log(age_at_breast_height)+
      -0.1794*Dry_soil+
      +0.2047*Moist_soil+
      +0.1247*peat+
      -0.1031*mobile_ground_water_present+
      -0.0255*latitude+
      -0.6880*altitude

    dry_weight_roots_above_5_T31 <-
      -6.3413+
      +13.2902*(diameter.cm/(diameter.cm+9))

    dry_weight_roots_above_5_T32 <-
      -3.5882+
      +13.6524*(diameter.cm/(diameter.cm+9))+
      -0.0467*SI*pinus+
      -0.0448*SI*picea+
      -0.0306*latitude

    dry_weights_roots_above_5_T33 <-
      -5.9660+
      +13.7465*(diameter.cm/(diameter.cm+9))+
      -0.0352*SI*pinus+
      -0.0356*SI*picea+
      -0.1443*Dry_soil+
      +0.3052*Moist_soil+
      +0.5078*peat+
      -0.6359*altitude

    dry_weights_roots_below_5_T34 <-
      -3.8375+
      +8.8795*(diameter.cm/(diameter.cm+10))

    dry_weights_roots_below_5_T35 <-
      -3.5912+
      +8.9776*(diameter.cm/(diameter.cm+10))+
      -0.0162*SI*pinus+
      -0.0123*SI*picea

    dry_weights_roots_below_5_T36 <-
      -3.3979+
      +8.9668*(diameter.cm/(diameter.cm+10))+
      -0.0204*SI*pinus+
      -0.0168*SI*picea+
      -0.4501*altitude


  } else if(species=="Picea abies"){

    dry_weight_of_stem_above_bark_G1 <-
      -2.0571+
      +11.3341*(diameter.cm/(diameter.cm+14))

    dry_weight_of_stem_above_bark_G2 <-
      -2.1702+
      +7.4690*(diameter.cm/(diameter.cm+14))+
      +0.0289*height.m+
      +0.6828*log(height.m)

    dry_weight_of_stem_above_bark_G3 <-
      -2.1781+
      +7.2601*(diameter.cm/(diameter.cm+14))+
      +0.0371*height.m+
      +0.4803*log(height.m)+
      +0.0934*log(age_at_breast_height)+
      +0.2239*form_quotient5+
      +0.1265*form_quotient3

    dry_weight_of_stem_wood_G4 <-
      -2.2471+
      +11.4873*(diameter.cm/(diameter.cm+14))

    dry_weight_of_stem_wood_G5 <-
      -2.3032+
      +7.2309*(diameter.cm/(diameter.cm+14))+
      +0.0355*height.m+
      +0.7030*log(height.m)

    dry_weight_of_stem_wood_G6 <-
      -2.2029+
      +7.0615*(diameter.cm/(diameter.cm+14))+
      +0.0448*height.m+
      +0.4522*log(height.m)+
      +0.0727*log(age_at_breast_height)+
      +0.3154*form_quotient5+
      +0.1467*form_quotient3

    dry_weight_of_stem_bark_G7 <-
      -3.3912+
      +9.8364*(diameter.cm/(diameter.cm+15))

    dry_weight_of_stem_bark_G8 <-
      -3.4020+
      +8.3089*(diameter.cm/(diameter.cm+15))+
      +0.0147*height.m+
      +0.2295*log(height.m)

    dry_weight_of_stem_bark_G9 <-
      -2.9427+
      +7.2807*(diameter.cm/(diameter.cm+15))+
      +0.0341*height.m+
      +0.3363*log(height.m)+
      -0.0203*SI*pinus+
      -0.0208*SI*picea

    dry_weight_of_stem_bark_G10 <-
      -3.1923+
      +6.5893*(diameter.cm/(diameter.cm+15))+
      +0.0353*height.m+
      +0.2818*log(height.m)+
      +0.1662*log(double_bark.mm)+
      +0.1729*log(age_at_breast_height)+
      -0.1836*log(Dmax_5_m_radius)+
      -0.00725*SI*pinus+
      -0.00849*SI*picea

    dry_weight_of_living_branches_and_needles_G11 <-
      -1.2804+
      +8.5242*(diameter.cm/(diameter.cm+13))

    dry_weight_of_living_branches_and_needles_G12 <-
      -1.2063+
      +10.9708*(diameter.cm/(diameter.cm+13))+
      -0.0124*height.m+
      -0.4923*log(height.m)

    dry_weight_of_living_branches_and_needles_G13 <-
      -1.1209+
      +10.4621*(diameter.cm/(diameter.cm+13))+
      -1.5211*log(height.m)+
      +1.0179*log(height.m-crown_base_height.m)+
      +0.0121*SI*pinus+
      +0.0110*SI*picea

    dry_weight_of_living_branches_and_needles_G14 <-
      -1.3242+
      +8.0106*(diameter.cm/(diameter.cm+13))+
      -0.9993*log(height.m)+
      +0.6623*log(height.m-crown_base_height.m)+
      +0.5003*log(crown_radius.m)+
      +0.2248*log(age_at_breast_height)+
      +0.2518*log(five_years_diameter_increment.mm)+
      -0.1640*log(Dmax_5_m_radius)

    dry_weight_of_needles_G15 <-
      -1.9602+
      +7.8171*(diameter.cm/(diameter.cm+12))

    dry_weight_of_needles_G16 <-
      -1.8551+
      +9.7809*(diameter.cm/(diameter.cm+12))+
      -0.4873*log(height.m)

    dry_weight_of_needles_G17 <-
      -1.5732+
      +8.4127*(diameter.cm/(diameter.cm+12))+
      -1.5628*log(height.m)+
      +1.4032*log(height.m-crown_base_height.m)

    dry_weight_of_needles_G18 <-
      -2.6982+
      +6.6949*(diameter.cm/(diameter.cm+12))+
      -0.8733*log(height.m)+
      +0.7249*log(height.m-crown_base_height.m)+
      +0.2066*log(crown_radius.m)+
      +0.2820*log(age_at_breast_height)+
      +0.4526*log(five_years_diameter_increment.mm)+
      -0.1467*log(Dmax_5_m_radius)

    dry_weight_of_dead_branches_G19 <-
      -4.3308+
      +9.9550*(diameter.cm/(diameter.cm+18))

    dry_weight_of_dead_branches_G20 <-
      -4.6351+
      +3.6518*(diameter.cm/(diameter.cm+18))+
      +0.0493*height.m+
      +1.0129*log(height.m)

    dry_weight_of_dead_branches_G21 <-
      -5.3924+
      +5.6333*(diameter.cm/(diameter.cm+18))+
      +2.7826*log(height.m)+
      -1.7460*log(height.m-crown_base_height.m)

    dry_weight_of_dead_branches_G22 <-
      -5.0472+
      +5.7144*(diameter.cm/(diameter.cm+18))+
      +1.7185*log(height.m)+
      -0.5287*log(height.m-crown_base_height.m)+
      -0.5739*log(five_years_diameter_increment.mm)+
      +0.2804*log(Dmax_5_m_radius)

    dry_weight_of_stumps_and_roots_G23 <-
      -2.4447+
      +10.5381*(diameter.cm/(diameter.cm+14))

    dry_weight_of_stumps_and_roots_G24 <-
      -2.0810+
      +10.6680*(diameter.cm/(diameter.cm+14))+
      -0.0162*SI*pinus+
      -0.0190*SI*picea

    dry_weight_of_stumps_and_roots_G25 <-
      -2.2616+
      +10.6277*(diameter.cm/(diameter.cm+14))+
      -0.0102*SI*pinus+
      -0.0144*SI*picea+
      +0.2237*Moist_soil+
      +0.2693*peat+
      -0.1919*mobile_ground_water_longer_periods

    dry_weight_of_stumps_G26 <-
      -3.3645+
      +10.6686*(diameter.cm/(diameter.cm+17))

    dry_weight_of_stumps_G27 <-
      -0.8963+
      +10.6925*(diameter.cm/(diameter.cm+17))+
      -0.0196*SI*pinus+
      -0.0188*SI*picea+
      -0.0305*latitude

    dry_weight_of_roots_above_5_G28 <-
      -6.3851+
      +13.3703*(diameter.cm/(diameter.cm+8))

    dry_weight_of_roots_above_5_G29 <-
      -6.0559+
      +13.6140*(diameter.cm/(diameter.cm+8))+
      -0.0204*SI*pinus+
      -0.0211*SI*picea

    dry_weight_of_roots_above_5_G30 <-
      -5.9948+
      +12.5949*(diameter.cm/(diameter.cm+8))+
      +0.3864*log(height.m)+
      -0.1114*log(age_at_breast_height)+
      -0.0215*SI*pinus+
      -0.0246*SI*picea+
      +0.3267*Moist_soil+
      +0.4094*peat+
      -0.4444*mobile_ground_water_longer_periods

    dry_weight_of_roots_below_5_G31 <-
      -2.5706+
      +7.6283*(diameter.cm/(diameter.cm+12))

    dry_weight_of_roots_below_5_G32 <-
      -2.3177+
      +7.7441*(diameter.cm/(diameter.cm+12))+
      -0.0105*SI*pinus+
      -0.0148*SI*picea

    dry_weight_of_roots_below_5_G33 <-
      -2.4676+
      +7.7375*(diameter.cm/(diameter.cm+12))+
      -0.00675*SI*pinus+
      -0.0117*SI*picea+
      +0.1777*Moist_soil+
      +0.2461*peat


  } else if(species%in%c("Betula pendula","Betula pubescens")){

    dry_weight_of_stem_above_bark_B1 <-
      -3.0932+
      +11.0735*(diameter.cm/(diameter.cm+8))

    dry_weight_of_stem_above_bark_B2 <-
      -3.5686+
      +8.2827*(diameter.cm/(diameter.cm+7))+
      +0.0393*height.m+
      +0.5772*log(height.m)

    dry_weight_of_stem_above_bark_B3 <-
      -3.5194+
      +8.0420*(diameter.cm/(diameter.cm+7))+
      +0.0531*height.m+
      +0.3897*log(height.m)+
      +0.1018*log(age_at_breast_height)

    dry_weight_of_stem_wood_B4 <-
      -2.3327+
      +10.8109*(diameter.cm/(diameter.cm+11))

    dry_weight_of_stem_wood_B5 <-
      -3.3045+
      +8.1184*(diameter.cm/(diameter.cm+11))+
      +0.9783*log(height.m)

    dry_weight_of_stem_wood_B6 <-
      -3.0464+
      +8.3820*(diameter.cm/(diameter.cm+11))+
      +0.9113*log(height.m)+
      +0.1024*log(age_at_breast_height)+
      -0.1067*log(double_bark.mm)+
      -0.00552*latitude

    dry_weight_of_stem_bark_B7 <-
      -3.2518+
      +10.3876*(diameter.cm/(diameter.cm+14))

    dry_weight_of_stem_bark_B8 <-
      -4.0778+
      +8.3019*(diameter.cm/(diameter.cm+14))+
      +0.7433*log(height.m)

    dry_weight_of_stem_bark_B9 <-
      -3.6430+
      +6.9285*(diameter.cm/(diameter.cm+14))+
      +0.5898*log(height.m)+
      +0.2772*log(age_at_breast_height)+
      +0.2038*log(double_bark.mm)+
      -0.0137*latitude

    dry_weight_of_stem_bark_B10 <-
      -2.3569+
      +7.4965*(diameter.cm/(diameter.cm+14))+
      +0.5947*log(height.m)+
      +0.1820*log(double_bark.mm)+
      +0.1972*log(age_at_breast_height)+
      -0.1185*log(five_years_diameter_increment.mm)+
      -0.1974*log(Dmax_5_m_radius)+
      -0.0182*latitude

    dry_weight_of_living_branches_B11 <-
      -3.3633+
      +10.2806*(diameter.cm/(diameter.cm+10))

    dry_weight_of_living_branches_B12 <-
      +0.0432+
      +12.7821*(diameter.cm/(diameter.cm+10))+
      -0.8525*log(height.m)+
      -0.0409*latitude

    dry_weight_of_living_branches_B13 <-
      +0.0282+
      +10.7485*(diameter.cm/(diameter.cm+10))+
      -1.2066*log(height.m)+
      +1.0409*log(height.m-crown_base_height.m)+
      -0.0415*latitude

    dry_weight_of_living_branches_B14 <-
      -0.3916+
      +8.0492*(diameter.cm/(diameter.cm+10))+
      -1.1407*log(height.m)+
      +0.7207*log(height.m-crown_base_height.m)+
      +0.9133*log(crown_radius.m)+
      +0.1702*log(age_at_breast_height)+
      +0.1747*log(five_years_diameter_increment.mm)+
      -0.0320*latitude

    dry_weight_of_dead_branches_B15 <-
      -5.9507+
      +7.9266*(diameter.cm/(diameter.cm+5))

    dry_weight_of_dead_branches_B16 <-
      -6.6237+
      +11.2872*(diameter.cm/(diameter.cm+30))+
      -0.3081*height.m+
      +2.6821*log(height.m)

    dry_weight-of_dead_branches_B17 <-
      -0.6700+
      +12.0799*(diameter.cm/(diameter.cm+30))+
      -0.3448*height.m+
      +2.7062*log(height.m)+
      +1.5634*altitude+
      -0.0914*latitude











  }




}
