#' Dominant height development of Norway Spruce and Scots Pine in Sweden.
#'
#' @source Pettersson, H. 1955. Barrskogens volymproduktion (Die
#' Massenproduktion des Nadelwaldes). Reports of the forest research institute
#' of Sweden. Vol 45:1. Centraltryckeriet Esselte AB. Stockholm. Available
#' Online (07/08/2022): \href{PUB EPSILON SLU}{https://pub.epsilon.slu.se/9982/1/medd_statens_skogsforskningsinst_045_01_a.pdf}
#' @details age and age2 absolutely must be of same type!
#' @param dominant_height Dominant height of the stand (Height corresponding to \eqn{3\sigma} of the diameter distribution)
#' @param age Age of stand.
#' @param age2 Desired age of stand.
#' @param ageType If 1: Total age. If 2: Age at breast height (1.3m).
#' @param planted If 0 (default), no. If 1, planted.
#' @param q_val Relation between the time to breast height for planted / non-planted stands.
#'
#' @return Dominant height of the stand in metres at desired age.
#' @export
#' @name Pettersson_1955_height

Pettersson_1955_height_trajectory_Northern_Sweden_Pine <-  function(
  dominant_height,
  age,
  age2,
  ageType=1,
  planted=0,
  q_val=0.7
){

  #t_val <- 9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100)
  # Primary measured t are estimated with a nls SSAsymp.
  # #t, age at 1.3 m.
  # t_val <- dplyr::case_when(
  #  H100 == 12 ~ 17.05,
  #  H100 == 16 ~ 14.53,
  #  H100 == 20 ~ 13.02,
  #  H100 == 24 ~ 12.01,
  #  H100 == 28 ~ 11.29,
  #  H100 == 32 ~ 10.75
  # )

  #T-val reduction if planted, attached to t_val calculations.
  planted <- ifelse(planted==1,q_val,1)

  # Chi Out.
  ChiOutReverse<- optimise(f=function(x) abs(dominant_height-1/(((1/1.3)^(1/3)) - x)^3),interval = c(-3,1))$minimum

  #H100
  H100Reverse <- if(ageType==1){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
    ((9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100))*planted)/ #t_val
      100)))*
      (1-(
        ((9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100))*planted)/ #t_val
          age)) #age
    ),interval = c(0,50))$minimum
  } else if(ageType==2){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100))*planted)/ #t_val
        100)))/
        (1+(
          ((9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  }

  ChiDiffReverse <- ((1/1.3)^(1/3))-((1/H100Reverse)^(1/3))

  tValReverse <-  (9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100Reverse))*planted

  BetaReverse = ChiDiffReverse  / (1-(tValReverse/100))

  if(ageType==1){ # Total age
    Chi_OutReverse = BetaReverse * (1-tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )


  } else if(ageType==2){ # Age at BH.
    Chi_OutReverse = BetaReverse / (1+tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )
  }

}


#' @rdname Pettersson_1955_height
#' @export
Pettersson_1955_height_trajectory_southern_Sweden_Pine <-  function(
    dominant_height,
    age,
    age2,
    ageType=1,
    planted=0,
    q_val=0.7
){

  #   Chi_Difference <- ((1/1.3)^(1/3))-((1/H100)^(1/3))
  # #
  # #   t_val <- dplyr::case_when(
  # #     H100 == 12 ~ 13.88,
  # #     H100 == 16 ~ 11.27,
  # #     H100 == 20 ~ 9.7,
  # #     H100 == 24 ~ 8.65,
  # #     H100 == 28 ~ 7.9,
  # #     H100 == 32 ~ 7.34,
  # #     TRUE ~ stop("H100 must be one of 12,16,20,24,28,32")
  # #   )
  #
  #   #nls(age~SSasymp(SI,Asym,R0,lrc),data=data.frame(SI=c(12,16,20,24,28,32),age=c(13.88,11.27,9.7,8.65,7.9,7.34)))
  #   t_val <- 6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100)

  #T-val reduction if planted, attached to t_val calculations.
  planted <- ifelse(planted==1,q_val,1)


  # Chi Out.
  ChiOutReverse<- optimise(f=function(x) abs(dominant_height-1/(((1/1.3)^(1/3)) - x)^3),interval = c(-3,1))$minimum

  #H100
  H100Reverse <- if(ageType==1){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100))*planted)/ #t_val
        100)))*
        (1-(
          ((6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  } else if(ageType==2){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100))*planted)/ #t_val
        100)))/
        (1+(
          ((6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  }

  ChiDiffReverse <- ((1/1.3)^(1/3))-((1/H100Reverse)^(1/3))

  tValReverse <-  (6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100Reverse))*planted

  BetaReverse = ChiDiffReverse  / (1-(tValReverse/100))

  if(ageType==1){ # Total age
    Chi_OutReverse = BetaReverse * (1-tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )


  } else if(ageType==2){ # Age at BH.
    Chi_OutReverse = BetaReverse / (1+tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )
  }

}

#' @rdname Pettersson_1955_height
#' @export
Pettersson_1955_height_trajectory_southern_Sweden_Spruce <-  function(
    dominant_height,
    age,
    age2,
    ageType=1,
    planted=0,
    q_val=0.7
){

  # t_val <- dplyr::case_when(
  #   #   H100 == 12 ~ 15.26,
  #   #   H100 == 16 ~ 12.68,
  #   #   H100 == 20 ~ 11.14,
  #   #   H100 == 24 ~ 10.11,
  #   #   H100 == 28 ~ 9.37,
  #   #   H100 == 32 ~ 8.82,
  #   #   TRUE ~ stop("H100 must be one of 12,16,20,24,28,32")
  #   # )
  #   #
  #   # nls(age~SSasymp(SI,Asym,R0,lrc),data=data.frame(SI=c(12,16,20,24,28,32),age=c(15.26,12.68,11.14,10.11,9.37,8.82)))
  #   t_val <- 7.967 + (33.286-7.967)*exp(-exp(-2.263)*H100)

  #T-val reduction if planted, attached to t_val calculations.
  planted <- ifelse(planted==1,q_val,1)


  # Chi Out.
  ChiOutReverse<- optimise(f=function(x) abs(dominant_height-1/(((1/1.3)^(1/3)) - x)^3),interval = c(-3,1))$minimum

  #H100
  H100Reverse <- if(ageType==1){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((7.967 + (33.286-7.967)*exp(-exp(-2.263)*H100))*planted)/ #t_val
        100)))*
        (1-(
          ((7.967 + (33.286-7.967)*exp(-exp(-2.263)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  } else if(ageType==2){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((7.967 + (33.286-7.967)*exp(-exp(-2.263)*H100))*planted)/ #t_val
        100)))/
        (1+(
          ((9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  }

  ChiDiffReverse <- ((1/1.3)^(1/3))-((1/H100Reverse)^(1/3))

  tValReverse <-  (7.967 + (33.286-7.967)*exp(-exp(-2.263)*H100Reverse))*planted

  BetaReverse = ChiDiffReverse  / (1-(tValReverse/100))

  if(ageType==1){ # Total age
    Chi_OutReverse = BetaReverse * (1-tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )


  } else if(ageType==2){ # Age at BH.
    Chi_OutReverse = BetaReverse / (1+tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )
  }

}


#' @rdname Pettersson_1955_height
#' @export
Pettersson_1955_height_trajectory_northern_Sweden_Spruce <-  function(
    dominant_height,
    age,
    age2,
    ageType=1,
    planted=0,
    q_val=0.7
){

  # # t_val <- dplyr::case_when(
  # #   H100 == 12 ~ 22,
  # #   H100 == 16 ~ 19.63,
  # #   H100 == 20 ~ 18.21,
  # #   H100 == 24 ~ 17.26,
  # #   H100 == 28 ~ 16.58,
  # #   H100 == 32 ~ 16.07,
  # #   TRUE ~ stop("H100 must be one of 12,16,20,24,28,32")
  # # )
  #
  # # nls(age~SSasymp(SI,Asym,R0,lrc),data=data.frame(SI=c(12,16,20,24,28,32),age=c(22,19.63,18.21,17.26,16.58,16.07)))
  # t_val <- 15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100)

  #T-val reduction if planted, attached to t_val calculations.
  planted <- ifelse(planted==1,q_val,1)

  # Chi Out.
  ChiOutReverse<- optimise(f=function(x) abs(dominant_height-1/(((1/1.3)^(1/3)) - x)^3),interval = c(-3,1))$minimum

  #H100
  H100Reverse <- if(ageType==1){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100))*planted)/ #t_val
        100)))*
        (1-(
          ((15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  } else if(ageType==2){
    optimise(f=function(H100) abs(ChiOutReverse - ((((1/1.3)^(1/3))-((1/H100)^(1/3)))/(1-(
      ((15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100))*planted)/ #t_val
        100)))/
        (1+(
          ((15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100))*planted)/ #t_val
            age)) #age
    ),interval = c(0,50))$minimum
  }

  ChiDiffReverse <- ((1/1.3)^(1/3))-((1/H100Reverse)^(1/3))

  tValReverse <-  (15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100Reverse))*planted

  BetaReverse = ChiDiffReverse  / (1-(tValReverse/100))

  if(ageType==1){ # Total age
    Chi_OutReverse = BetaReverse * (1-tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )


  } else if(ageType==2){ # Age at BH.
    Chi_OutReverse = BetaReverse / (1+tValReverse/age2)

    return(
      1/(((1/1.3)^(1/3)) - Chi_OutReverse)^3
    )
  }

}


#' Pettersson time to breast height.
#'
#' @details This is a smoothed function over calculated years to breast height
#' 1.3 m in Petterson 1955. On the form: \eqn{timeToBH = Asymptote + (R0 - Asymptote) * exp(-exp(lrc) * H100)}
#' Which is included in the height_trajectory functions.
#'
#' @source Pettersson, H. 1955. Barrskogens volymproduktion (Die
#' Massenproduktion des Nadelwaldes). Reports of the forest research institute
#' of Sweden. Vol 45:1. Centraltryckeriet Esselte AB. Stockholm. Available
#' Online (07/08/2022): \href{PUB EPSILON SLU}{https://pub.epsilon.slu.se/9982/1/medd_statens_skogsforskningsinst_045_01_a.pdf}
#'
#' @param H100 Dominant height at total age 100.
#' @param species One of "Picea abies" or "Pinus sylvestris
#' @param planted 1 if planted, 0 if natural regeneration.
#' @param northern 1 if in northern sweden, else 0 if in southern sweden.
#'
#' @return time to breast height (years).
Petterson_1955_time_to_BH <- function(
    species,
    H100=20,
    planted=0,
    northern=0,
    q_val=0.7
){

  planted=ifelse(planted==1,qval,1)

  if(species=="Pinus sylvestris"){

    if(northern==1){
      #Pine northern Sweden
      tBH = (9.913445+(34.667120-9.913445)*exp(-exp(-2.263625)*H100))*planted
    } else {

      #Pine southern Sweden
      tBH = (6.462 + (32.083-6.462)*exp(-exp(-2.267)*H100))*planted

    }

  }

  if(species=="Picea abies"){
    if(northern==1){
      #Spruce northern Sweden
      tBH = (15.278 + (38.518-15.278)*exp(-exp(-2.266)*H100))*planted

    } else {
      #Spruce southern Sweden
      tBH = (7.967 + (33.286-7.967)*exp(-exp(-2.263)*H100))*planted
    }

  }

  return(
    tBH
  )

}






#' Diameter increment for 5 years for Swedish Conifers from Petterson 1955.
#'
#' @details This relates to the procentual increment over 5 years for the Mean
#'  diameter.
#'
#' @source Pettersson, H. 1955. Barrskogens volymproduktion (Die
#' Massenproduktion des Nadelwaldes). Reports of the forest research institute
#' of Sweden. Vol 45:1. Centraltryckeriet Esselte AB. Stockholm. Available
#' Online (07/08/2022): \href{PUB EPSILON SLU}{https://pub.epsilon.slu.se/9982/1/medd_statens_skogsforskningsinst_045_01_a.pdf}
#'
#' @param stems Number of stems per hectare.
#' @param diameterCm Mean diameter in cm.
#' @param totalAge Total age of stand.
#' @param yearsSinceFirstThinning Number of years since the first thinning.
#' @param stemsAfterThinningPeriodStart stems remaining after the thinning
#' at the start of the period.
#' @param diameterAfterThinningCm Mean diameter in cm after the thinning stage
#' at the start of the perid.
#'
#' @return Diameter increment in percent.
#' @export
#' @name Pettersson_1955_Dp5


Pettersson_1955_diameter_procentual_increment_northern_Sweden_Pine <- function(
  stems,
  diameterCm,
  totalAge,
  yearsSinceFirstThinning,
  stemsAfterThinningPeriodStart,
  diameterAfterThinningCm
){

  a=4.216
  b2=0.6737
  b3=-0.5925
  b4=-122.4
  b5=106.9
  b6=-1.791
  b7=12.06

  #Diameter sum on bark per ha.. before first thinning in meters!
  w = (diameterCm/100)*stems


  logp5 <- a +
    b2*log(w)+
    b3*log(totalAge)+
    b4*(1/yearsSinceFirstThinning+30)+
    b5*(log(yearsSinceFirstThinning+30)/(yearsSinceFirstThinning+30))+
    b6*log(stemsAfterThinningPeriodStart+1000)+
    b7*(1/(diameterAfterThinningCm+3))



  return(
    exp(logp5)
  )
}


#' @export
#' @rdname Pettersson_1955_Dp5
Pettersson_1955_diameter_procentual_increment_southern_Sweden_Pine <- function(
    stems,
    diameterCm,
    totalAge,
    yearsSinceFirstThinning,
    diameterAfterThinningCm
){

  a=5.712
  b2=0.3941
  b3=-0.6819
  b4=-0.003613
  b5=-1.723
  b6=16.58

  #Diameter sum on bark per ha.. before first thinning in meters!
  w = (diameterCm/100)*stems


  logp5 <- a +
    b2*log(w)+
    b3*log(totalAge)+
    b4*yearsSinceFirstThinning+
    b5*log(yearsSinceFirstThinning+600)+
    b6*(1/(diameterAfterThinningCm+3))



  return(
    exp(logp5)
  )
}

#' @export
#' @rdname Pettersson_1955_Dp5
Pettersson_1955_diameter_procentual_increment_southern_Sweden_Spruce <- function(
    stems,
    diameterCm,
    totalAge,
    yearsSinceFirstThinning,
    diameterAfterThinningCm
){

  a=5.628
  b2=0.6970
  b3=-0.9404
  b4=-0.004580
  b5=-1.721
  b6=14.84
  #Diameter sum on bark per ha.. before first thinning in meters!
  w = (diameterCm/100)*stems


  logp5 <- a +
    b2*log(w)+
    b3*log(totalAge)+
    b4*yearsSinceFirstThinning+
    b5*log(yearsSinceFirstThinning+500)+
    b6*(1/(diameterAfterThinningCm+3))



  return(
    exp(logp5)
  )
}



#' @export
#' @details For the diameter increment viz. Norway Spruce in northern Sweden,
#' only 10 yr intervals from the initial age can be provided. This due to the
#' function being a preliminary work based on 'long-cores' (every tenth yr)
#' which was collected from "Skogsforskningsinstitutets stora undersökning i
#' orörd skog", field work 1941-1949. Measurements for last 100 years of age
#' class 140-179. Site quality classes III-VIII in counties Norrbotten,
#' Västerbotten, Jämtland & Västernorrland. p. 289. Original strength of
#' site index was deemed too high for immediate use and subjectively corrected.
#' @rdname Pettersson_1955_Dp5
Pettersson_1955_diameter_procentual_increment_northern_Sweden_Spruce <- function(
    diameterCm,
    ageAtBreastHeight,
    H100,
    initialAge,
    yearSinceInitialAge
){
  #Since material was measured at 10 yr intervals, it was sorted into 5 periods
  #(c(10,20),c(30,40),c(50,60),c(70,80),c(90,100)) from the center.
  if((yearsSinceInitialAge%%10!=0)) stop("yearsSinceInitialAge must be evenly
                                         divisible by 10. Read documentation.")

  if(yearsSinceInitialAge%in%c(10,20)){
    A = -0.1716*initialAge + 1.096*(0.25*H100+15)
    B = 1.149
  }

  if(yearsSinceInitialAge%in%c(30,40)){
    A = -0.3070*initialAge + 2.206*(0.25*H100+15)
    B = 1.284
  }

  if(yearsSinceInitialAge%in%c(50,60)){
    A = -0.3733*initialAge + 3.041*(0.25*H100+15)
    B = 1.368
  }

  if(yearsSinceInitialAge%in%c(70,80)){
    A = -0.3930*initialAge + 3.748*(0.25*H100+15)
    B = 1.410
  }

  if(yearsSinceInitialAge%in%c(90,100)){
    A = -0.3960*initialAge + 4.36*(0.25*H100+15)
    B = 1.443
  }

  return(
    A+B*diameterCm
  )

}


#' Petterson 1955 Diameter Classes for truncated normal distributions.
#' @details The parameters suffix '0' indicates that this is the initial state.
#' @param Phi0 Range of the truncated distribution from upper limit to
#' truncation expressed in standard deviations of the normal distribution. e.g.
#' untruncated normal distribution Phi0 is 6.
#' @param Sigman0 Standard deviation of the normal distribution.
#' @param alpha0 The truncation point.
#' @param nClasses Number of desired diameter classes.
#' @export
Petterson_1955_DiameterClasses <- function(Phi0=3,Sigman0, alpha0, nClasses){
upperDistributionLimit = alpha0 + Phi0*Sigman0
lowerDistributionLimit = Phi0*Sigman0
classWidth = (Phi0*Sigman0)/nClasses
classMiddleDiameters = seq(alpha0+0.5*classWidth,upperDistributionLimit-0.5*classWidth,by=classWidth)

return(
  list(
    "upperDistributionLimit"=upperDistributionLimit,
    "lowerDistributionLimit"=lowerDistributionLimit,
    "classWidth"=classWidth,
    "classMiddleDiameters"=classMiddleDiameters
  )
)
}


Petterson_1955_DiameterToHeight_Sweden_Pine_Spruce <- function(
    species="Picea abies",
    dominantHeightM = 20,
    diameterUpperDistributionLimit,
    diameterCm,
    northernSweden=TRUE
){
  if(northernSweden==TRUE) #For both Pine and Spruce due to lack of material.
  {
    K = 0.0035*dominantHeightM + 0.7718 #R^2  =1
    n=2
  }

  if(northernSweden!=TRUE && species=="Pinus sylvestris")
  {
    K = 0.0063*dominantHeightM + 0.7281 #R^2 = 1
    n=2
  }

  if(northernSweden!=TRUE && species=="Picea abies")
  {
    K = 0.0028*dominantHeightM + 0.8347 #R^2 = 1
    n=3
  }

  APrim = (1-K)/(dominantHeightM-1.3)^(1/n)
  B = K / (dominantHeightM-1.3)^(1/n)

  return(
    ((1/(((APrim*diameterUpperDistributionLimit)/diameterCm)+B))^n)+1.3
  )
}
