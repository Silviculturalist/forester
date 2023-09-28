#Marklund 1988

#Pine stem
Marklund_1988_T1 <- function(DBH){
  return(
    exp(
    -2.3388+
    11.3264*(DBH/(DBH+13))
    )
  )
}

Marklund_1988_T2 <- function(DBH,height.m){
  return(
    exp(
      -2.6768+
      +7.5939*(DBH/(DBH+13))+
      +0.0151*height.m+
      +0.8799*log(height.m)
    )
  )
}

Marklund_1988_T3 <- function(DBH, height.m, double_bark.mm, ageBH){
  return(
   exp(
     -2.6232+
      +7.7318*(DBH/(DBH+13))+
      +0.0139*height.m+
      +0.8625*log(height.m)+
      -0.0704*log(double_bark.mm)+
      +0.00185*ageBH
  )
  )
}

Marklund_1988_T4 <- function(DBH, height.m, double_bark.mm, ageBH, form_quotient5, form_quotient3, altitude){
  if(!(is.null(form_quotient5)|is.null(form_quotient3))){
    warning('Either form_quotient5 or form_quotient3 can be supplied. Not both.')
    warning('form_quotient5 taking precedence.')
    form_quotient3 = NULL
  }
  return(
    exp(
    -2.4826+
      +7.9039*(DBH/(DBH+13))+
      +0.0184*height.m+
      +0.6939*log(height.m)+
      -0.0731*log(double_bark.mm)+
      +0.00182*ageBH+
      +0.2382*form_quotient5+
      +0.2217*form_quotient3+
      -0.1596*altitude
    )
  )
}

MarklundPineStem = list(Marklund_1988_T1,Marklund_1988_T2,Marklund_1988_T3,Marklund_1988_T4)
arg_counts <- sapply(MarklundPineStem, function(f) length(formals(f)))
MarklundPineStem = MarklundPineStem[order(-arg_counts)]

#Stem wood
Marklund_1988_T5 <- function(DBH){
  return(
    exp(
      -2.2184+
        11.4219*(DBH/(DBH+14))
    )
  )
}

Marklund_1988_T6 <- function(DBH, height.m){
  return(
    exp(
      -2.6864+
        +7.6066*(DBH/(DBH+14))+
        +0.0200*height.m+
        +0.8658*log(height.m)
    )
  )
}

Marklund_1988_T7 <- function(DBH, height.m, double_bark.mm, ageBH){
  return(
    exp(
      -2.5325+
        +7.8936*(DBH/(DBH+14))+
        +0.0231*height.m+
        +0.7887*log(height.m)+
        -0.1065*log(double_bark.mm)+
        +0.00201*ageBH
    )
  )
}

Marklund_1988_T8 <- function(DBH, height.m, double_bark.mm, ageBH, form_quotient5, form_quotient3, altitude){
  if(!(is.null(form_quotient5)|is.null(form_quotient3))){
    warning('Either form_quotient5 or form_quotient3 can be supplied. Not both.')
    warning('form_quotient5 taking precedence.')
    form_quotient3 = NULL
  }
  return(
    exp(
      -2.0028+
        +7.9455*(DBH/(DBH+14))+
        +0.0439*height.m+
        +0.2437*log(height.m)+
        -0.0875*log(double_bark.mm)+
        +0.00172*ageBH+
        +0.7778*form_quotient5+
        +0.4855*form_quotient3+
        -0.1557*altitude
    )
  )
}

MarklundPineStemWood = list(Marklund_1988_T5,Marklund_1988_T6,Marklund_1988_T7,Marklund_1988_T8)
arg_counts <- sapply(MarklundPineStemWood, function(f) length(formals(f)))
MarklundPineStemWood = MarklundPineStemWood[order(-arg_counts)]

#Stem bark
Marklund_1988_T9 <- function(DBH){
  return(
    exp(
      -2.9748+
        +8.8489*(DBH/(DBH+16))
    )
  )
}

Marklund_1988_T10 <- function(DBH, height.m){
  return(
    exp(
      -3.2765+
        +7.2482*(DBH/(DBH+16))+
        +0.4487*log(height.m)
    )
  )
}

Marklund_1988_T11 <- function(DBH, height.m, relative_bark_thickness){
  return(
    exp(
      -3.6065+
        +7.0834*(DBH/(DBH+16))+
        +0.5086*log(height.m)+
        +0.0255*relative_bark_thickness
    )
  )
}


Marklund_1988_T12 <- function(DBH, height.m, crown_base_height.m, relative_bark_thickness){
  return(
    exp(
      -3.5076+
        +7.5295*(DBH/(DBH+16))+
        +0.5629*log(height.m)+
        -0.2271*log(height.m-crown_base_height.m)+
        +0.0222*relative_bark_thickness
    )
  )
}

MarklundPineBark = list(Marklund_1988_T9,Marklund_1988_T10,Marklund_1988_T11,Marklund_1988_T12)
arg_counts <- sapply(MarklundPineBark, function(f) length(formals(f)))
MarklundPineBark = MarklundPineBark[order(-arg_counts)]

#Pine living branches
Marklund_1988_T13 <- function(DBH){
  return(
    exp(
      -2.8604+
        +9.1015*(DBH/(DBH+10))
    )
  )
}

Marklund_1988_T14 <- function(DBH, height.m){
  return(
    exp(
      -2.5413+
        +13.3955*(DBH/(DBH+10))+
        -1.1955*log(height.m)
    )
  )
}

Marklund_1988_T15 <- function(DBH, height.m, crown_base_height.m, latitude){
  return(
    exp(
      -0.9137+
        +11.4337*(DBH/(DBH+10))+
        -1.4815*log(height.m)+
        +0.9825*log(height.m-crown_base_height.m)+
        -0.0235*latitude

    )
  )
}

Marklund_1988_T16 <- function(DBH, height.m, crown_base_height.m, crown_radius.m, ageBH, DBH_i5.mm){
  return(
    exp(
      -2.8445+
        +9.0891*(DBH/(DBH+10))+
        -1.1599*log(height.m)+
        +0.6197*log(height.m-crown_base_height.m)+
        +0.5372*log(crown_radius.m)+
        +0.2011*log(ageBH)+
        +0.2142*log(DBH_i5.mm)
    )
  )
}

MarklundPineLivingBranches = list(Marklund_1988_T13,Marklund_1988_T14,Marklund_1988_T15,Marklund_1988_T16)
arg_counts <- sapply(MarklundPineLivingBranches, function(f) length(formals(f)))
MarklundPineLivingBranches = MarklundPineLivingBranches[order(-arg_counts)]


#Pine needles
Marklund_1988_T17 <- function(DBH){
  return(
    exp(
      -3.7983+
        +7.7681*(DBH/(DBH+7))
    )
  )
}

Marklund_1988_T18 <- function(DBH, height.m){
  return(
    exp(
      -3.4781+
        +12.1095*(DBH/(DBH+7))+
        +0.0413*height.m+
        -1.5650*log(height.m)
    )
  )
}

Marklund_1988_T19 <- function(DBH, height.m, crown_base_height.m, latitude){
  return(
    exp(
      -2.6024+
        +9.8471*(DBH/(DBH+7))+
        +0.0260*height.m+
        -1.6717*log(height.m)+
        +1.0419*log(height.m-crown_base_height.m)+
        -0.0123*latitude
    )
  )
}

Marklund_1988_T20 <- function(DBH, height.m, crown_base_height.m, crown_radius.m, ageBH, DBH_i5.mm, altitude){
  return(
    exp(
      -4.6082+
        +7.7998*(DBH/(DBH+7))+
        -0.6978*log(height.m)+
        +0.4588*log(height.m-crown_base_height.m)+
        +0.2398*log(crown_radius.m)+
        +0.2632*log(ageBH)+
        +0.4040*log(DBH_i5.mm)+
        +0.5144*altitude
    )
  )
}

MarklundPineNeedles = list(Marklund_1988_T17,Marklund_1988_T18,Marklund_1988_T19,Marklund_1988_T20)

arg_counts <- sapply(MarklundPineNeedles, function(f) length(formals(f)))
MarklundPineNeedles = MarklundPineNeedles[order(-arg_counts)]


#Pine dead branches
Marklund_1988_T21 <- function(DBH){
  return(
    exp(
      -5.3338+
        +9.5938*(DBH/(DBH+10))
    )
  )
}

Marklund_1988_T22 <- function(DBH, height.m){
  return(
    exp(
      -5.8926+
        +7.1270*(DBH/(DBH+10))+
        -0.0465*height.m+
        +1.1060*log(height.m)
    )
  )
}

Marklund_1988_T23 <- function(DBH, height.m, latitude, altitude){
  return(
    exp(
      -0.9305+
        +7.1889*(DBH/(DBH+10))+
        -0.0850*height.m+
        +1.3027*log(height.m)+
        -0.0702*latitude+
        -1.0568*altitude
    )
  )
}

Marklund_1988_T24 <- function(DBH, height.m, ageBH, DBH_i5.mm, Dmax_10m, latitude, altitude){
  return(
    exp(
      -0.8931+
        +10.3377*(DBH/(DBH+10))+
        -0.0865*height.m+
        +0.8701*log(height.m)+
        -0.6209*log(ageBH)+
        -0.5100*log(DBH_i5.mm)+
        +0.5846*Dmax_10m+
        -0.0577*latitude+
        -1.1226*altitude

    )
  )
}

MarklundPineDeadBranches = list(Marklund_1988_T21,Marklund_1988_T22,Marklund_1988_T23,Marklund_1988_T24)

arg_counts <- sapply(MarklundPineDeadBranches, function(f) length(formals(f)))
MarklundPineDeadBranches = MarklundPineDeadBranches[order(-arg_counts)]

#Pine stump-root system
Marklund_1988_T25 <- function(DBH){
  return(
    exp(
      -3.3913+
        +11.1106*(DBH/(DBH+12))
    )
  )
}

Marklund_1988_T26 <- function(DBH, height.m, SI_species, H100, latitude){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)

  return(
    exp(
      -1.5530+
        +11.2246*(DBH/(DBH+12))+
        -0.0314*H100*pinus+
        -0.0268*H100*picea+
        -0.0192*latitude
    )
  )
}

Marklund_1988_T27 <- function(DBH, ageBH, SI_species, H100, soil_moisture, soil_texture, soil_water, altitude){
  if((isTRUE(soil_moisture==4)+ isTRUE(soil_texture==9)+ isTRUE(soil_moisture==1))>1){
    warning('Soil moisture "Moist", Soil Moisture "Dry" and Soil Texture "Peat" must not all be TRUE. Peat taking precedence.')
    soil_moisture==999 #Anything other than 4.
  }
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -3.1628+
        +10.7181*(DBH/(DBH+12))+
        +0.0952*log(ageBH)+
        -0.0168*H100*pinus+
        -0.0136*H100*picea+
        -0.0808*ifelse(soil_moisture==1,1,0)+
        +0.2165*ifelse(soil_moisture==4,1,0)+
        +0.3088*ifelse(soil_texture==9,1,0)+
        -0.1655*ifelse(soil_water==3,1,0)+
        -0.1070*ifelse(soil_water==2,1,0)+
        -0.5221*altitude
    )
  )
}

MarklundPineStumpRootSystem = list(Marklund_1988_T25,Marklund_1988_T26,Marklund_1988_T27)

arg_counts <- sapply(MarklundPineStumpRootSystem, function(f) length(formals(f)))
MarklundPineStumpRootSystem = MarklundPineStumpRootSystem[order(-arg_counts)]

#Pine stump
Marklund_1988_T28 <- function(DBH){
  return(
    exp(
      -3.9657+
      -11.0481*DBH/(DBH+15)
    )
  )
}

Marklund_1988_T29 <- function(DBH, ageBH, latitude){
  return(
    exp(
      -2.1762+
        9.5137*(DBH/(DBH+15))+
        0.3105*log(ageBH)+
        -0.0326*latitude*10^2 #TODO: Check this!


    )
  )
}

Marklund_1988_T30 <- function(DBH, ageBH, soil_moisture, soil_texture, soil_water, latitude, altitude){
  if((isTRUE(soil_moisture==4)+ isTRUE(soil_texture==9)+ isTRUE(soil_moisture==1))>1){
    warning('Soil moisture "Moist", Soil Moisture "Dry" and Soil Texture "Peat" must not all be TRUE. Peat taking precedence.')
    soil_moisture==999 #Anything other than 4.
  }
  return(
    exp(
      -2.5087+
        9.4014*(DBH/(DBH+15))+
        0.3220*log(ageBH)+
        -0.1794*ifelse(soil_moisture==1,1,0)+
        0.2047*ifelse(soil_moisture==4,1,0)+
        0.1247*ifelse(soil_texture==9,1,0)+
        -0.1031*ifelse(soil_water%in%c(2,3),1,0)+
        -0.0255*latitude*10^2+
        -0.6880*altitude

    )
  )
}

MarklundPineStump = list(Marklund_1988_T28,Marklund_1988_T29,Marklund_1988_T30)

arg_counts <- sapply(MarklundPineStump, function(f) length(formals(f)))
MarklundPineStump = MarklundPineStump[order(-arg_counts)]

#Pine roots above 5
Marklund_1988_T31 <- function(DBH){
  return(
    exp(
      -6.3413+
        +13.2902*(DBH/(DBH+9))
    )
  )
}

Marklund_1988_T32 <- function(DBH, height.m, SI_species, H100, latitude){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -3.5882+
        +13.6524*(DBH/(DBH+9))+
        -0.0467*H100*pinus+
        -0.0448*H100*picea+
        -0.0306*latitude
    )
  )
}

Marklund_1988_T33 <- function(DBH, height.m, SI_species, H100, soil_moisture, soil_texture, altitude){
  if((isTRUE(soil_moisture==4)+ isTRUE(soil_texture==9)+ isTRUE(soil_moisture==1))>1){
    warning('Soil moisture "Moist", Soil Moisture "Dry" and Soil Texture "Peat" must not all be TRUE. Peat taking precedence.')
    soil_moisture==999 #Anything other than 4.
  }
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -5.9660+
        +13.7465*(DBH/(DBH+9))+
        -0.0352*H100*pinus+
        -0.0356*H100*picea+
        -0.1443*ifelse(soil_moisture==1,1,0)+
        +0.3052*ifelse(soil_moisture==4,1,0)+
        +0.5078*ifelse(soil_texture==9,1,0)+
        -0.6359*altitude
    )
  )
}

MarklundPineLargeRoots = list(Marklund_1988_T31,Marklund_1988_T32,Marklund_1988_T33)

arg_counts <- sapply(MarklundPineLargeRoots, function(f) length(formals(f)))
MarklundPineLargeRoots = MarklundPineLargeRoots[order(-arg_counts)]

#Pine roots <5
Marklund_1988_T34 <- function(DBH){
  return(
    exp(
      -3.8375+
        +8.8795*(DBH/(DBH+10))
    )
  )
}

Marklund_1988_T35 <- function(DBH, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -3.5912+
        +8.9776*(DBH/(DBH+10))+
        -0.0162*H100*pinus+
        -0.0123*H100*picea

    )
  )
}

Marklund_1988_T36 <- function(DBH, SI_species, H100, altitude){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -3.3979+
        +8.9668*(DBH/(DBH+10))+
        -0.0204*H100*pinus+
        -0.0168*H100*picea+
        -0.4501*altitude
    )
  )
}

MarklundPineSmallRoots = list(Marklund_1988_T34,Marklund_1988_T35,Marklund_1988_T36)

arg_counts <- sapply(MarklundPineSmallRoots, function(f) length(formals(f)))
MarklundPineSmallRoots = MarklundPineSmallRoots[order(-arg_counts)]

#Spruce stem
Marklund_1988_G1 <- function(DBH){
  return(
    exp(
      -2.0571+
        +11.3341*(DBH/(DBH+14))
    )
  )
}

Marklund_1988_G2 <- function(DBH, height.m){
  return(
    exp(
      -2.1702+
        +7.4690*(DBH/(DBH+14))+
        +0.0289*height.m+
        +0.6828*log(height.m)
    )
  )
}

Marklund_1988_G3 <- function(DBH, height.m, ageBH, form_quotient5, form_quotient3){
  if(!(is.null(form_quotient5)|is.null(form_quotient3))){
    warning('Either form_quotient5 or form_quotient3 can be supplied. Not both.')
    warning('form_quotient5 taking precedence.')
    form_quotient3 = NULL
  }
  return(
    exp(
      -2.1781+
        +7.2601*(DBH/(DBH+14))+
        +0.0371*height.m+
        +0.4803*log(height.m)+
        +0.0934*log(ageBH)+
        +0.2239*ifelse(!is.null(form_quotient5),form_quotient5,0)+
        +0.1265*ifelse(!is.null(form_quotient3),form_quotient3,0)
    )
  )
}

MarklundSpruceStem = list(Marklund_1988_G1,Marklund_1988_G2,Marklund_1988_G3)

arg_counts <- sapply(MarklundSpruceStem, function(f) length(formals(f)))
MarklundSpruceStem = MarklundSpruceStem[order(-arg_counts)]

#Spruce stem wood
Marklund_1988_G4 <- function(DBH){
  return(
    exp(
      -2.2471+
        +11.4873*(DBH/(DBH+14))
    )
  )
}
Marklund_1988_G5 <- function(DBH, height.m){
  return(
    exp(
      -2.3032+
        +7.2309*(DBH/(DBH+14))+
        +0.0355*height.m+
        +0.7030*log(height.m)
    )
  )
}
Marklund_1988_G6 <- function(DBH, height.m, ageBH, form_quotient5, form_quotient3){
  if(!(is.null(form_quotient5)|is.null(form_quotient3))){
    warning('Either form_quotient5 or form_quotient3 can be supplied. Not both.')
    warning('form_quotient5 taking precedence.')
    form_quotient3 = NULL
  }
  return(
    exp(
      -2.2029+
        +7.0615*(DBH/(DBH+14))+
        +0.0448*height.m+
        +0.4522*log(height.m)+
        +0.0727*log(ageBH)+
        +0.3154*ifelse(!is.null(form_quotient5),form_quotient5,0)+
        +0.1467*ifelse(!is.null(form_quotient3),form_quotient3,0)

    )
  )
}

MarklundSpruceStemWood = list(Marklund_1988_G4,Marklund_1988_G5,Marklund_1988_G6)

arg_counts <- sapply(MarklundSpruceStemWood, function(f) length(formals(f)))
MarklundSpruceStemWood = MarklundSpruceStemWood[order(-arg_counts)]

#Spruce bark
Marklund_1988_G7 <- function(DBH){
  return(
    exp(
      -3.3912+
        +9.8364*(DBH/(DBH+15))
    )
  )
}
Marklund_1988_G8 <- function(DBH, height.m){
  return(
    exp(
      -3.4020+
        +8.3089*(DBH/(DBH+15))+
        +0.0147*height.m+
        +0.2295*log(height.m)
    )
  )
}
Marklund_1988_G9 <- function(DBH, height.m, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -2.9427+
        +7.2807*(DBH/(DBH+15))+
        +0.0341*height.m+
        +0.3363*log(height.m)+
        -0.0203*H100*pinus+
        -0.0208*H100*picea

    )
  )
}
Marklund_1988_G10 <- function(DBH, height.m, double_bark.mm, ageBH, Dmax_10m, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -3.1923+
        +6.5893*(DBH/(DBH+15))+
        +0.0353*height.m+
        +0.2818*log(height.m)+
        +0.1662*log(double_bark.mm)+
        +0.1729*log(ageBH)+
        -0.1836*log(Dmax_10m)+
        -0.00725*H100*pinus+
        -0.00849*H100*picea
    )
  )
}

MarklundSpruceBark = list(Marklund_1988_G7,Marklund_1988_G8,Marklund_1988_G9,Marklund_1988_G10)

arg_counts <- sapply(MarklundSpruceBark, function(f) length(formals(f)))
MarklundSpruceBark = MarklundSpruceBark[order(-arg_counts)]

#Spruce living branches
Marklund_1988_G11 <- function(DBH){
  return(
    exp(
      -1.2804+
        +8.5242*(DBH/(DBH+13))
    )
  )
}

Marklund_1988_G12 <- function(DBH, height.m){
  return(
    exp(
      -1.2063+
        +10.9708*(DBH/(DBH+13))+
        -0.0124*height.m+
        -0.4923*log(height.m)

    )
  )
}

Marklund_1988_G13 <- function(DBH, height.m, crown_base_height.m, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -1.1209+
        +10.4621*(DBH/(DBH+13))+
        -1.5211*log(height.m)+
        +1.0179*log(height.m-crown_base_height.m)+
        +0.0121*H100*pinus+
        +0.0110*H100*picea
    )
  )
}

Marklund_1988_G14 <- function(DBH, height.m, crown_base_height.m, crown_radius.m, ageBH, DBH_i5.mm, Dmax_10m){
  return(
    exp(
      -1.3242+
        +8.0106*(DBH/(DBH+13))+
        -0.9993*log(height.m)+
        +0.6623*log(height.m-crown_base_height.m)+
        +0.5003*log(crown_radius.m)+
        +0.2248*log(ageBH)+
        +0.2518*log(DBH_i5.mm)+
        -0.1640*log(Dmax_10m)

    )
  )
}


MarklundSpruceLivingBranches = list(Marklund_1988_G11,Marklund_1988_G12,Marklund_1988_G13,Marklund_1988_G14)
arg_counts <- sapply(MarklundSpruceLivingBranches, function(f) length(formals(f)))
MarklundSpruceLivingBranches = MarklundSpruceLivingBranches[order(-arg_counts)]



#Spruce needles
Marklund_1988_G15 <- function(DBH){
  return(
    exp(
      -1.9602+
        +7.8171*(DBH/(DBH+12))
    )
  )
}

Marklund_1988_G16 <- function(DBH, height.m){
  return(
    exp(
      -1.8551+
        +9.7809*(DBH/(DBH+12))+
        -0.4873*log(height.m)
    )
  )
}

Marklund_1988_G17 <- function(DBH, height.m, crown_base_height.m){
  return(
    exp(
      -1.5732+
        +8.4127*(DBH/(DBH+12))+
        -1.5628*log(height.m)+
        +1.4032*log(height.m-crown_base_height.m)
    )
  )
}
Marklund_1988_G18 <- function(DBH, height.m, crown_base_height.m, crown_radius.m, ageBH, DBH_i5.mm, Dmax_10m){
  return(
    exp(
      -2.6982+
        +6.6949*(DBH/(DBH+12))+
        -0.8733*log(height.m)+
        +0.7249*log(height.m-crown_base_height.m)+
        +0.2066*log(crown_radius.m)+
        +0.2820*log(ageBH)+
        +0.4526*log(DBH_i5.mm)+
        -0.1467*log(Dmax_10m)

    )
  )
}

MarklundSpruceNeedles = list(Marklund_1988_G15,Marklund_1988_G16,Marklund_1988_G17,Marklund_1988_G18)

arg_counts <- sapply(MarklundSpruceNeedles, function(f) length(formals(f)))
MarklundSpruceNeedles = MarklundSpruceNeedles[order(-arg_counts)]

#Spruce dead branches
Marklund_1988_G19 <- function(DBH){
  return(
    exp(
      -4.3308+
        +9.9550*(DBH/(DBH+18))
    )
  )
}
Marklund_1988_G20 <- function(DBH, height.m){
  return(
    exp(
      -4.6351+
        +3.6518*(DBH/(DBH+18))+
        +0.0493*height.m+
        +1.0129*log(height.m)
    )
  )
}
Marklund_1988_G21 <- function(DBH, height.m, crown_base_height.m){
  return(
    exp(
      -5.3924+
        +5.6333*(DBH/(DBH+18))+
        +2.7826*log(height.m)+
        -1.7460*log(height.m-crown_base_height.m)
    )
  )
}

Marklund_1988_G22 <- function(DBH, height.m, crown_base_height.m, DBH_i5.mm, Dmax_10m){
  return(
    exp(
      -5.0472+
        +5.7144*(DBH/(DBH+18))+
        +1.7185*log(height.m)+
        -0.5287*log(height.m-crown_base_height.m)+
        -0.5739*log(DBH_i5.mm)+
        +0.2804*log(Dmax_10m)
    )
  )
}

MarklundSpruceDeadBranches = list(Marklund_1988_G19,Marklund_1988_G20,Marklund_1988_G21,Marklund_1988_G22)
arg_counts <- sapply(MarklundSpruceDeadBranches, function(f) length(formals(f)))
MarklundSpruceDeadBranches = MarklundSpruceDeadBranches[order(-arg_counts)]

#Spruce stump-root system
Marklund_1988_G23 <- function(DBH){
  return(
    exp(
      -2.4447+
        +10.5381*(DBH/(DBH+14))
    )
  )
}
Marklund_1988_G24 <- function(DBH, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -2.0810+
        +10.6680*(DBH/(DBH+14))+
        -0.0162*H100*pinus+
        -0.0190*H100*picea
    )
  )
}

Marklund_1988_G25 <- function(DBH, soil_moisture, soil_texture, soil_water, SI_species, H100){
  if(soil_moisture==4 & soil_texture==9){
    warning('Soil Moisture must not be moist whilst soil texture is Peat. Peat taking precedence.')
    soil_moisture==999 #Anything other than 4.
  }
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -2.2616+
        +10.6277*(DBH/(DBH+14))+
        -0.0102*H100*pinus+
        -0.0144*H100*picea+
        +0.2237*ifelse(soil_moisture==4,1,0)+
        +0.2693*ifelse(soil_texture==9,1,0)+
        -0.1919*ifelse(soil_water==3,1,0)
    )
  )
}

MarklundSpruceStumpRootSystem = list(Marklund_1988_G23,Marklund_1988_G24,Marklund_1988_G25)
arg_counts <- sapply(MarklundSpruceStumpRootSystem, function(f) length(formals(f)))
MarklundSpruceStumpRootSystem = MarklundSpruceStumpRootSystem[order(-arg_counts)]

#Spruce stump
Marklund_1988_G26 <- function(DBH){
  return(
    exp(
      -3.3645+
        +10.6686*(DBH/(DBH+17))
    )
  )
}

Marklund_1988_G27 <- function(DBH, SI_species, H100, latitude){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -0.8963+
        +10.6925*(DBH/(DBH+17))+
        -0.0196*H100*pinus+
        -0.0188*H100*picea+
        -0.0305*latitude
    )
  )
}


MarklundSpruceStump = list(Marklund_1988_G26,Marklund_1988_G27)

arg_counts <- sapply(MarklundSpruceStump, function(f) length(formals(f)))
MarklundSpruceStump = MarklundSpruceStump[order(-arg_counts)]


#Spruce roots > 5 cm
Marklund_1988_G28 <- function(DBH){
  return(
    exp(
      -6.3851+
        +13.3703*(DBH/(DBH+8))
    )
  )
}
Marklund_1988_G29 <- function(DBH, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -6.0559+
        +13.6140*(DBH/(DBH+8))+
        -0.0204*H100*pinus+
        -0.0211*H100*picea
    )
  )
}


Marklund_1988_G30 <- function(DBH, height.m, ageBH, SI_species, H100, soil_moisture, soil_texture, soil_water){
  if(soil_moisture==4 & soil_texture==9){
    warning('Soil Moisture must not be moist whilst soil texture is Peat. Peat taking precedence.')
    soil_moisture==999 #Anything other than 4.
  }

  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)

  return(
    exp(
      -5.9948+
        +12.5949*(DBH/(DBH+8))+
        +0.3864*log(height.m)+
        -0.1114*log(ageBH)+
        -0.0215*H100*pinus+
        -0.0246*H100*picea+
        +0.3267*ifelse(soil_moisture==4,1,0)+
        +0.4094*ifelse(soil_texture==9,1,0)+
        -0.4444*ifelse(soil_water==3,1,0)
    )
  )
}

MarklundSpruceLargeRoots = list(Marklund_1988_G28,Marklund_1988_G29,Marklund_1988_G30)

arg_counts <- sapply(MarklundSpruceLargeRoots, function(f) length(formals(f)))
MarklundSpruceLargeRoots = MarklundSpruceLargeRoots[order(-arg_counts)]

#Spruce roots <5 cm
Marklund_1988_G31 <- function(DBH){
  return(
    exp(
      -2.5706+
        +7.6283*(DBH/(DBH+12))
    )
  )
}

Marklund_1988_G32 <- function(DBH, SI_species, H100){
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)
  return(
    exp(
      -2.3177+
        +7.7441*(DBH/(DBH+12))+
        -0.0105*H100*pinus+
        -0.0148*H100*picea
    )
  )
}
Marklund_1988_G33 <- function(DBH, SI_species, H100, soil_moisture, soil_texture){
  if(soil_moisture==4 & soil_texture==9){
    warning('Soil Moisture must not be moist whilst soil texture is Peat. Peat taking precedence.')
    soil_moisture==999 #Anything other than 4.
  }
  pinus = ifelse(SI_species=='Pinus sylvestris',1,0)
  picea = ifelse(SI_species=='Picea abies',1,0)

  return(
    exp(
      -2.4676+
        +7.7375*(DBH/(DBH+12))+
        -0.00675*H100*pinus+
        -0.0117*H100*picea+
        +0.1777*ifelse(soil_moisture==4,1,0)+
        +0.2461*ifelse(soil_texture==9,1,0)
    )
  )
}

MarklundSpruceSmallRoots = list(Marklund_1988_G31,Marklund_1988_G32,Marklund_1988_G33)
arg_counts <- sapply(MarklundSpruceSmallRoots, function(f) length(formals(f)))
MarklundSpruceSmallRoots = MarklundSpruceSmallRoots[order(-arg_counts)]


#Birch stem
Marklund_1988_B1 <- function(DBH){
  return(
    exp(
      -3.0932+
        +11.0735*(DBH/(DBH+8))
    )
  )
}

Marklund_1988_B2 <- function(DBH, height.m){
  return(
    exp(
      -3.5686+
        +8.2827*(DBH/(DBH+7))+
        +0.0393*height.m+
        +0.5772*log(height.m)
    )
  )
}

Marklund_1988_B3 <- function(DBH, height.m, ageBH){
  return(
    exp(
      -3.5194+
        +8.0420*(DBH/(DBH+7))+
        +0.0531*height.m+
        +0.3897*log(height.m)+
        +0.1018*log(ageBH)
    )
  )
}

MarklundBirchStem = list(Marklund_1988_B1,Marklund_1988_B2,Marklund_1988_B3)
arg_counts <- sapply(MarklundBirchStem, function(f) length(formals(f)))
MarklundBirchStem = MarklundBirchStem[order(-arg_counts)]


#Birch stem wood
Marklund_1988_B4 <- function(DBH){
  return(
    exp(
      -2.3327+
        +10.8109*(DBH/(DBH+11))
    )
  )
}

Marklund_1988_B5 <- function(DBH, height.m){
  return(
    exp(
      -3.3045+
        +8.1184*(DBH/(DBH+11))+
        +0.9783*log(height.m)
    )
  )
}

Marklund_1988_B6 <- function(DBH, height.m, ageBH, double_bark.mm, latitude){
  return(
    exp(
      -3.0464+
        +8.3820*(DBH/(DBH+11))+
        +0.9113*log(height.m)+
        +0.1024*log(ageBH)+
        -0.1067*log(double_bark.mm)+
        -0.00552*latitude
    )
  )
}

MarklundBirchStemWood = list(Marklund_1988_B4,Marklund_1988_B5,Marklund_1988_B6)
arg_counts <- sapply(MarklundBirchStemWood, function(f) length(formals(f)))
MarklundBirchStemWood = MarklundBirchStemWood[order(-arg_counts)]

#Birch bark
Marklund_1988_B7 <- function(DBH){
  return(
    exp(
      -3.2518+
        +10.3876*(DBH/(DBH+14))
    )
  )
}

Marklund_1988_B8 <- function(DBH, height.m){
  return(
    exp(
      -4.0778+
        +8.3019*(DBH/(DBH+14))+
        +0.7433*log(height.m)
    )
  )
}

Marklund_1988_B9 <- function(DBH, height.m, ageBH, double_bark.mm, latitude){
  return(
    exp(
      -3.6430+
        +6.9285*(DBH/(DBH+14))+
        +0.5898*log(height.m)+
        +0.2772*log(ageBH)+
        +0.2038*log(double_bark.mm)+
        -0.0137*latitude
    )
  )
}

Marklund_1988_B10 <- function(DBH, height.m, double_bark.mm, ageBH, DBH_i5.mm, Dmax_10m, latitude){
  return(
    exp(
      -2.3569+
        +7.4965*(DBH/(DBH+14))+
        +0.5947*log(height.m)+
        +0.1820*log(double_bark.mm)+
        +0.1972*log(ageBH)+
        -0.1185*log(DBH_i5.mm)+
        -0.1974*log(Dmax_10m)+
        -0.0182*latitude
    )
  )
}

MarklundBirchBark = list(Marklund_1988_B7,Marklund_1988_B8,Marklund_1988_B9,Marklund_1988_B10)
arg_counts <- sapply(MarklundBirchBark, function(f) length(formals(f)))
MarklundBirchBark = MarklundBirchBark[order(-arg_counts)]


#Birch living branches
Marklund_1988_B11 <- function(DBH){
  return(
    exp(
      -3.3633+
        +10.2806*(DBH/(DBH+10))
    )
  )
}

Marklund_1988_B12 <- function(DBH, height.m, latitude){
  return(
    exp(
      0.0432+
        +12.7821*(DBH/(DBH+10))+
        -0.8525*log(height.m)+
        -0.0409*latitude
    )
  )
}

Marklund_1988_B13 <- function(DBH, height.m, crown_base_height.m, latitude){
  return(
    exp(
      0.0282+
        +10.7485*(DBH/(DBH+10))+
        -1.2066*log(height.m)+
        +1.0409*log(height.m-crown_base_height.m)+
        -0.0415*latitude
    )
  )
}

Marklund_1988_B14 <- function(DBH, height.m, crown_base_height.m, crown_radius.m, ageBH, DBH_i5.mm, latitude){
  return(
    exp(
      -0.3916+
        +8.0492*(DBH/(DBH+10))+
        -1.1407*log(height.m)+
        +0.7207*log(height.m-crown_base_height.m)+
        +0.9133*log(crown_radius.m)+
        +0.1702*log(ageBH)+
        +0.1747*log(DBH_i5.mm)+
        -0.0320*latitude
    )
  )
}

MarklundBirchBranches = list(Marklund_1988_B11,Marklund_1988_B12,Marklund_1988_B13,Marklund_1988_B14)
arg_counts <- sapply(MarklundBirchBranches, function(f) length(formals(f)))
MarklundBirchBranches = MarklundBirchBranches[order(-arg_counts)]



#Birch dead branches
Marklund_1988_B15 <- function(DBH){
  return(
    exp(
      -5.9507+
        +7.9266*(DBH/(DBH+5))
    )
  )
}

Marklund_1988_B16 <- function(DBH, height.m){
  return(
    exp(
      -6.6237+
        +11.2872*(DBH/(DBH+30))+
        -0.3081*height.m+
        +2.6821*log(height.m)
    )
  )
}

Marklund_1988_B17 <- function(DBH, height.m, altitude, latitude){
  return(
    exp(
      -0.6700+
        +12.0799*(DBH/(DBH+30))+
        -0.3448*height.m+
        +2.7062*log(height.m)+
        +1.5634*altitude+
        -0.0914*latitude

    )
  )
}

MarklundBirchDeadBranches = list(Marklund_1988_B15,Marklund_1988_B16,Marklund_1988_B17)
arg_counts <- sapply(MarklundBirchDeadBranches, function(f) length(formals(f)))
MarklundBirchDeadBranches = MarklundBirchDeadBranches[order(-arg_counts)]


applyFunction <- function(args,funList){
  for (func_name in 1:length(funList)) {
    func <- funList[[func_name]]
    func_args <- formals(func)

    # Check if all required arguments for the current function are provided
    if (all(names(func_args) %in% names(args))) {
      # Check if all arguments of the function are used
      if (length(names(func_args)) == length(intersect(names(args), names(func_args)))) {
        return(do.call(func, args[names(func_args)]))
      }
    }
  }

  warning("No suitable regression function found for the supplied arguments. Returning NA")
  return(NA)
}


#' Calculate dry-weight biomass for individual trees.
#'
#' @source Marklund, Lars-Gunnar. 1988. Biomassafunktioner för Tall, Gran och Björk i Sverige: Biomass functions for pine, spruce and birch in Sweden. Report 45.  73 pp. Dept. of Forest Survey. Swedish University of Agricultural Sciences. Umeå. ISSN 0348-0496. ISBN 91-576-3524-2.
#'
#' @details
#' Unused arguments should be set to NULL.
#'
#'
#' @param species One of 'Picea abies', 'Pinus sylvestris', 'Betula pendula' or 'Betula pubescens'.
#' @param DBH Diameter at breast height (1.3m), in cm.
#' @param diameter_3m.cm Diameter at 3 m height, in cm.
#' @param diameter_5m.cm Diameter at 5 m height, in cm.
#' @param height.m Height of tree, in m.
#' @param ageBH Age at breast height (1.3m)
#' @param H100 Site Index H100 per [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()],[forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()],[forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param SI_Species One of 'Picea abies' or 'Pinus sylvestris'.
#' @param DBH_i5.mm Five year diameter increment at breast height, in mm.
#' @param double_bark.mm Double bark thickness, mm.
#' @param soil_texture Code 1-9. [forester::Sweden_soil_types('texture')]
#' @param soil_moisture Code 1-5. [forester::Sweden_soil_types('moisture')]
#' @param soil_water Code 1-3. [forester::Sweden_soil_types('water')]
#' @param latitude Latitude, decimal.
#' @param longitude Longitude, decimal.
#' @param epsg_in EPSG of latitude, longitude arguments.
#' @param altitude Altitude, meters above sea level.
#' @param crown_base_height.m Height from ground to lowest green branch, in meters. See SWE-NFI detailed instruction.
#' @param crown_radius.m Crown radius, meters.
#' @param Dmax_10m DBH of the thickest tree on a plot of radius 10 m.
#'
#' @return List of dry weights of tree components (kg).
#' @export
#'
#' @examples
#' Marklund_1988_biomass_Sweden(species='Picea abies',DBH=20)
Marklund_1988_biomass_Sweden <- function(
    species=NULL,
    DBH=NULL,
    diameter_3m.cm=NULL,
    diameter_5m.cm=NULL,
    height.m=NULL,
    ageBH=NULL,
    H100=NULL,
    SI_Species=NULL,
    DBH_i5.mm=NULL,
    double_bark.mm=NULL,
    soil_texture=NULL,
    soil_moisture=NULL,
    soil_water=NULL,
    latitude=NULL,
    longitude=NULL,
    epsg_in=NULL,
    altitude=NULL,
    crown_base_height.m=NULL,
    crown_radius.m=NULL,
    Dmax_10m=NULL
  ){

  #Capture args
  arglist = as.list(match.call())

  #Check tree species is supported.
  if(!(species%in%c("Picea abies",
                    "Pinus sylvestris",
                    "Betula pendula",
                    "Betula pubescens"))){
    stop("Tree species not supported.")
  }

  if(is.na(DBH)){
    stop('DBH must always be supplied.')
  }

  #altitude to km above sea level.
  if(!is.null(altitude)){
    arglist$altitude <- altitude/1000
  }


  #Calculate form quotient if available.
  if(!is.null(diameter_3m.cm)){
    arglist$form_quotient3 <- diameter_3m.cm/diameter.cm
  }
  if(!is.null(diameter_5m.cm)){
    arglist$form_quotient5 <- diameter_5m.cm/diameter.cm
  }

  #Convert input crs to RT90.
  if(!is.null(latitude) & is.null(epsg_in)) stop('Latitude is provided but no EPSG is given!')
  if(!is.null(latitude) & is.null(longitude)) stop('Latitude but no Longitude provided.')

  if(!is.null(epsg_in)){
  if(!is.null(longitude) & !is.null(latitude)){

    message("Input is reprojected to RT90 2.5 gon V,  EPSG:3021")

    geom_point <- as.matrix(data.frame(x=longitude,y=latitude))

    geom_point <- terra::vect(geom_point, crs=paste0("epsg:",epsg_in))

    geom_point <- terra::project(geom_point, paste0("epsg:",3021)) #RT90

    #km*10^2 = 100 000 m.
    arglist$latitude <- terra::geom(geom_point)[4]/100000

    rm(geom_point)

  }
  }

  if(!is.null(double_bark.mm)){
    arglist$relative_bark_thickness <-  (double_bark.mm / (diameter.cm*10))*100
  }

  stemOutput  <- list(
    'Stem over bark'=NA,
    'Stem wood'=NA,
    'Stem bark'=NA,
    'Living branches (incl. needles)'=NA,
    'Needles'=NA,
    'Dead branches'=NA,
    'Stump-root system'=NA,
    'Stump'=NA,
    'Roots diameter >= 5 cm'=NA,
    'Roots diameter < 5 cm'=NA
  )

  #Large if statements..
  #If Scots Pine
  if(species=='Pinus sylvestris'){
    #Stem.
    stemOutput$'Stem over bark' = applyFunction(arglist,MarklundPineStem)

    #Stem wood.
    stemOutput$'Stem wood' = applyFunction(arglist,MarklundPineStemWood)

    #Stem bark
    stemOutput$'Stem bark' = applyFunction(arglist,MarklundPineBark)

    #Living branches /w needles
    stemOutput$'Living branches (incl. needles)' = applyFunction(arglist,MarklundPineLivingBranches)

    #Needles
    stemOutput$'Needles' = applyFunction(arglist,MarklundPineNeedles)

    #Dead branches
    stemOutput$'Dead branches' = applyFunction(arglist,MarklundPineDeadBranches)

    #Stump root system
    stemOutput$'Stump-root system' = applyFunction(arglist,MarklundPineStumpRootSystem)

    #Stump
    stemOutput$'Stump' = applyFunction(arglist,MarklundPineStump)

    #Roots w diameter >= 5 cm
    stemOutput$'Roots diameter >= 5 cm' = applyFunction(arglist,MarklundPineLargeRoots)

    #Roots w diameter <5 cm
    stemOutput$'Roots diameter < 5 cm' = applyFunction(arglist,MarklundPineSmallRoots)


  }

  #Spruce
  if(species=='Picea abies'){
    #Stem.
    stemOutput$'Stem over bark' = applyFunction(arglist,MarklundSpruceStem)

    #Stem wood.
    stemOutput$'Stem wood' = applyFunction(arglist,MarklundSpruceStemWood)

    #Stem bark
    stemOutput$'Stem bark' = applyFunction(arglist,MarklundSpruceBark)

    #Living branches /w needles
    stemOutput$'Living branches (incl. needles)' = applyFunction(arglist,MarklundSpruceLivingBranches)

    #Needles
    stemOutput$'Needles' = applyFunction(arglist,MarklundSpruceNeedles)

    #Dead branches
    stemOutput$'Dead branches' = applyFunction(arglist,MarklundSpruceDeadBranches)

    #Stump root system
    stemOutput$'Stump-root system' = applyFunction(arglist,MarklundSpruceStumpRootSystem)

    #Stump
    stemOutput$'Stump' = applyFunction(arglist,MarklundSpruceStump)

    #Roots w diameter >= 5 cm
    stemOutput$'Roots diameter >= 5 cm' = applyFunction(arglist,MarklundSpruceLargeRoots)

    #Roots w diameter <5 cm
    stemOutput$'Roots diameter < 5 cm' = applyFunction(arglist,MarklundSpruceSmallRoots)


  }

  #Birch
  if(species%in%c('Betula pendula','Betula pubescens')){
    #Stem.
    stemOutput$'Stem over bark' = applyFunction(arglist,MarklundBirchStem)

    #Stem wood.
    stemOutput$'Stem wood' = applyFunction(arglist,MarklundBirchStemWood)

    #Stem bark
    stemOutput$'Stem bark' = applyFunction(arglist,MarklundBirchBark)

    #Living branches /w needles
    stemOutput$'Living branches (incl. needles)' = applyFunction(arglist,MarklundBirchBranches)

    #Needles
    stemOutput$'Needles' = NA

    #Dead branches
    stemOutput$'Dead branches' = applyFunction(arglist,MarklundBirchDeadBranches)

    #Stump root system
    stemOutput[[7]] = NA

    #Stump
    stemOutput[[8]] = NA

    #Roots w diameter >= 5 cm
    stemOutput[[9]] = NA

    #Roots w diameter <5 cm
    stemOutput[[10]] = NA
  }

  return(
    stemOutput
  )
}
