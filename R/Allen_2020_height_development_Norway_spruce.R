#' Height Development function for Norway Spruce in Norway, from Allen et al. 2020.
#'
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: <https://doi.org/10.1080/02827581.2020.1773525>
#'
#' @details N.B. Although the paper uses log10(), it should be ln()!
#' This is corrected in this function.
#'
#'
#'
#' N.B. The function fitted in the paper has several changes compared to the function from
#' Diéguez-Aranda, U.; Burkhart, H. E.; Rodríguez-Soalleiro, R. (2005). Modeling dominant
#' height growth of radiata pine (Pinus radiata D. Don) plantations in north-western Spain.
#' Forest Ecology and Management. Vol. 215. #1-3. pp. 271-284.
#' <https://doi.org/10.1016/j.foreco.2005.05.015>
#'
#' @param dominant_height numeric. Dominant height of stand in metres.
#' @param stand_age numeric. Stand age, years from planting.
#' @param ref_age numeric. Reference age. Age at output.
#'
#' @return numeric. Height at reference age in metres.
Allen_2020_height_development_Norway_spruce <- function(dominant_height,
                                                        stand_age,
                                                        ref_age){

  beta1 <- 0.01605
  beta2 <- 0.61208
  beta3 <- 4.43722

  L <- log(1 - exp(-beta1*stand_age))

  X0 <- 0.5*((log(dominant_height) + beta2*L) + sqrt((log(dominant_height) - beta2*L)^2 - (4*beta3*L)))

  H2 <- dominant_height*((1-exp(-beta1*ref_age))/(1-exp(-beta1*stand_age)))^((beta2+beta3)/X0)

  return(H2)

}


#' Volume of Norway Spruce stand
#'
#' @param basalArea Basal Area m2/ha
#' @param dominantHeight Dominant height of stand, m.
#' @param Age Stand Age (total).
#'
#' @return Volume, m3/ha.
Allen_2020_Volume_Norway_Spruce_Norway <- function(
  basalArea,
  dominantHeight,
  Age
){
  return(
    0.24961 * (basalArea^(1.15036)) * (dominantHeight*(1.01153)) * (exp(2.320398/(Age)))
  )
}

#Calculate the basal area at the end of the period.
Allen_2020_Basal_Area_Norway_Spruce_Norway <- function(
  basalAreaBeforeThinning,
  basalAreaAfterThinning,
  dominantHeight1,
  dominantHeightAtThinning,
  dominantHeight2,
  stemsBefore,
  stemsAfter
){
  b1=4.77696
  b2=0.30957
  b3= -0.1479
  TR = (basalAreaAfterThinning/basalAreaBeforeThinning)^(b3*(dominantHeightAtThinning/dominantHeight2))

  return(
    (basalAreaAfterThinning^(dominantHeight1/dominantHeight2))*exp(b1*((stemsAfter/stemsBefore)^b2)*(1-(dominantHeight1/dominantHeight2))*TR)
  )
}


#Calculate the number of surviving stems (... self-thinning ...)
Allen_2020_surviving_stems_Norway_Spruce_Norway <- function(
    stemsAfterThinning,
    basalAreaAfterThinning,
    basalAreaBeforeThinning,
    standAge,
    standAge2,
    SI40,
    IncrementPeriod=5){
  return(
    ((stemsAfterThinning^-1.0085) + 0.03675*(basalAreaAfterThinning / basalAreaBeforeThinning)*
       ((SI40/1000)^3.76228)*((standAge+IncrementPeriod)^2.5541 - standAge*2.5541))^(1/-1.0097)
  )
}


#Get the number of stems after thinning if thinning by basal area.
Allen_2020_stems_after_thinning_Norway_Spruce_Norway <- function(
  basalAreaBeforeThinning,
  basalAreaAfterThinning,
  stemsBeforeThinning
){
  return(
    stemsBeforeThinning*(exp(-1.93267 + 1.92953*(basalAreaAfterThinning/basalAreaBeforeThinning)))
  )
}


#Get the basal area after thinning if thinning by stem numbers.
Allen_2020_basal_area_after_thinning_Norway_Spruce_Norway <- function(
    stemsBeforeThinning,
    stemsAfterThinning,
    basalAreaBeforeThinning
){
  return(
    ((log(stemsAfterThinning/stemsBeforeThinning)- -1.93267)/1.92953)*basalAreaBeforeThinning
  )
}


#Simple helper to calculate QMD from Basal area and stems.
getQMD <- function(BasalArea,Stems){
  return(
    sqrt(
      BasalArea/((pi/40000)*Stems)
    )
  )
}




#Create a new stand.
#Not quite true OOP, but works as a list of lists.
#Each iteration I extend the appropriate sublist by c(list, x).
new_AllenStand <- function(Age,SI40,Stems,BasalArea,Volume){
  x <- list(
    SI40 = SI40
  )
  x$startStems = c(NA)
  x$Stems = c(Stems)
  x$StemsThinned = c(0)
  x$BasalAreaThinned = c(0)
  x$VolumeThinned = c(0)
  x$startBasalArea = c(NA)
  x$BasalArea = c(BasalArea)
  x$startVolume = c(NA)
  x$Volume = c(Volume)
  x$MAI_Volume = c(Volume/Age)
  x$MAI_BasalArea = c(BasalArea/Age)
  x$CAI_Volume= c(NA)
  x$CAI_BasalArea=c(NA)
  x$startAge = c(NA)
  x$Age = c(Age)
  x$IncrementPeriod = c(NA)
  x$QMD <- getQMD(BasalArea,Stems)
  x$ThinningHeight <- c(NA)
  x$Total_Volume <- c(Volume)
  x$Total_BasalArea <- c(BasalArea)


  #Calculate height
  x$Height <- c(Allen_2020_height_development_Norway_spruce(dominant_height = SI40, stand_age = 40,ref_age = Age))
  x$startHeight <- c(NA)

  x <- structure(x,class=c('AllenStand','list'))

  return(
    x
  )
}


#Convert list of lists to data.frame.
as.data.frame.AllenStand <- function(x){
  x_df <- do.call(cbind,x)
  x_df <- x_df %>% as.data.frame() %>%  dplyr::select(Age,Height,startStems,startBasalArea,startVolume,Stems,BasalArea,Volume,QMD,StemsThinned,BasalAreaThinned,VolumeThinned,MAI_BasalArea,MAI_Volume,CAI_BasalArea,CAI_Volume,IncrementPeriod,Total_BasalArea,Total_Volume)
  return(
    x_df
  )
}




#Grow dispatcher for S3.
grow <- function(x,...){
  UseMethod('grow',x)
}

#grow an AllenStand.
#dplyr::last() retrieves the last value of a list.
grow.AllenStand <- function(x,incrementPeriod=5,thinned_stems=NULL,thinned_basalArea=NULL,ThinningHeight=NULL){

  if(!is.null(thinned_stems) & !is.null(thinned_basalArea))
    stop('Provide only one of thinned_stems or thinned_basalArea')

  #Shifting of endvalues to startvalues
  x$startAge <- c(x$startAge,dplyr::last(x$Age))
  x$Age <- c(x$Age,dplyr::last(x$startAge)+incrementPeriod)
  x$startStems <- c(x$startStems,dplyr::last(x$Stems))
  x$Stems <- c(x$Stems,dplyr::last(x$Stems)) #Necessary.
  x$startVolume <- c(x$startVolume,dplyr::last(x$Volume))
  x$startBasalArea <- c(x$startBasalArea,dplyr::last(x$BasalArea))
  x$startHeight <- c(x$startHeight,dplyr::last(x$Height))
  x$IncrementPeriod <- c(x$IncrementPeriod,incrementPeriod)
  x$Height <- c(x$Height,
                Allen_2020_height_development_Norway_spruce(
                  x$SI40,
                  40,
                  dplyr::last(x$Age)
                )
                )
  x$ThinningHeight <- c(x$ThinningHeight,ifelse(!is.null(ThinningHeight),ThinningHeight,NA))

  #Pers. comm. Micky Allen, future stems per ha is calculated before thinning.

  #Self thinning during period...
  x$Stems[length(x$Stems)] <- Allen_2020_surviving_stems_Norway_Spruce_Norway(
    stemsAfterThinning = dplyr::last(x$Stems),
    basalAreaAfterThinning = dplyr::last(x$BasalArea),
    basalAreaBeforeThinning = dplyr::last(x$startBasalArea),
    standAge = dplyr::last(x$startAge),
    standAge2 = dplyr::last(x$Age),
    SI40 = x$SI40,
    IncrementPeriod = incrementPeriod)

  if(!is.null(thinned_basalArea)){
    #Removed nr stems
    x$BasalAreaThinned <- c(x$BasalAreaThinned,thinned_basalArea)

    #Stems at end of period.
    x$BasalArea <- c(x$BasalArea,dplyr::last(x$startBasalArea)-thinned_basalArea)

    #Remaining stems before increment growth...
    x$Stems[[length(x$Stems)]] <-
      Allen_2020_stems_after_thinning_Norway_Spruce_Norway(
        basalAreaBeforeThinning = dplyr::last(x$startBasalArea),
        basalAreaAfterThinning = dplyr::last(x$BasalArea),
        stemsBeforeThinning = dplyr::last(x$startStems)
      )


    x$StemsThinned<- c(x$StemsThinned, dplyr::last(x$startStems)-dplyr::last(x$Stems))
  }




  if(!is.null(thinned_stems)){
    #Removed nr stems
    x$StemsThinned <- c(x$StemsThinned,thinned_stems)

    #Stems at end of period.
    x$Stems[[length(x$Stems)]] <- dplyr::last(x$startStems)-thinned_stems

    #Remaining basal area before increment growth...
    x$BasalArea <- c(x$BasalArea,
                  Allen_2020_basal_area_after_thinning_Norway_Spruce_Norway(
                    stemsBeforeThinning = dplyr::last(x$startStems),
                    stemsAfterThinning = dplyr::last(x$Stems),
                    basalAreaBeforeThinning = dplyr::last(x$startBasalArea)
                  )
                  )

    x$BasalAreaThinned <- c(x$BasalAreaThinned, dplyr::last(x$startBasalArea)-dplyr::last(x$BasalArea))
  }


  if(is.null(thinned_basalArea) & is.null(thinned_stems)){
    x$StemsThinned<- c(x$StemsThinned,0)
    x$BasalAreaThinned <- c(x$BasalAreaThinned,0)
    x$BasalArea <- c(x$BasalArea,dplyr::last(x$startBasalArea))
  }




  ## IF NO THINNING WOULD BE CONDUCTED ###
  #NECESSARY TO FIGURE OUT VOLUME THINNED ###
  #Self thinning if no thinning would be done.
  stemsIfNoThinning <- Allen_2020_surviving_stems_Norway_Spruce_Norway(
    stemsAfterThinning = dplyr::last(x$Stems), #should this still be dplyr::last(x$startStems)? i.e. not including self-thinning.
    basalAreaAfterThinning = dplyr::last(x$startBasalArea),
    basalAreaBeforeThinning = dplyr::last(x$startBasalArea),
    standAge = dplyr::last(x$startAge),
    standAge2 = dplyr::last(x$Age),
    SI40 = x$SI40,
    IncrementPeriod = incrementPeriod)

  #Basal area as if no thinning was conducted
  basalAreaIfNoThinning <- Allen_2020_Basal_Area_Norway_Spruce_Norway(
    basalAreaBeforeThinning = dplyr::last(x$startBasalArea),
    basalAreaAfterThinning = dplyr::last(x$startBasalArea),
    dominantHeight1 = dplyr::last(x$startHeight),
    dominantHeightAtThinning = ifelse(is.null(ThinningHeight),dplyr::last(x$startHeight),ThinningHeight),
    dominantHeight2 = dplyr::last(x$Height),
    stemsBefore = dplyr::last(x$startStems),
    stemsAfter = dplyr::last(x$Stems)
  )

  #Volume as if not thinning.
  volumeIfNoThinning <- Allen_2020_Volume_Norway_Spruce_Norway(
                                      basalArea = basalAreaIfNoThinning,
                                      dominantHeight = dplyr::last(x$Height),
                                      Age = dplyr::last(x$Age)
                                      )


  ###################


  #Basal Area at end of increment period
  x$BasalArea[[length(x$BasalArea)]] <- Allen_2020_Basal_Area_Norway_Spruce_Norway(
    basalAreaBeforeThinning = dplyr::last(x$startBasalArea),
    basalAreaAfterThinning = dplyr::last(x$BasalArea),
    dominantHeight1 = dplyr::last(x$startHeight),
    dominantHeightAtThinning = ifelse(is.null(ThinningHeight),dplyr::last(x$startHeight),ThinningHeight),
    dominantHeight2 = dplyr::last(x$Height),
    stemsBefore = dplyr::last(x$startStems),
    stemsAfter = dplyr::last(x$Stems)
      )

  #Volume at end of increment period
  x$Volume <- c(x$Volume,
                Allen_2020_Volume_Norway_Spruce_Norway(
                  basalArea = dplyr::last(x$BasalArea),
                  dominantHeight = dplyr::last(x$Height),
                  Age = dplyr::last(x$Age))
                )

  x$VolumeThinned <- c(x$VolumeThinned,volumeIfNoThinning-dplyr::last(x$Volume))

  #Update QMD
  x$QMD <- c(x$QMD, getQMD(BasalArea = dplyr::last(x$BasalArea),Stems = dplyr::last(x$Stems)))

  #Update Totals..
  x$Total_Volume <- c(x$Total_Volume,dplyr::last(x$Total_Volume)+dplyr::last(x$Volume)-dplyr::last(x$startVolume)+dplyr::last(x$VolumeThinned)) #No Thinning alternative?
  x$Total_BasalArea <- c(x$Total_BasalArea,dplyr::last(x$Total_BasalArea)+dplyr::last(x$BasalArea)-dplyr::last(x$startBasalArea)+dplyr::last(x$BasalAreaThinned))

  #MAI (Net?)
  x$MAI_Volume <- c(x$MAI_Volume, dplyr::last(x$Volume)/dplyr::last(x$Age))
  x$MAI_BasalArea <- c(x$MAI_BasalArea, dplyr::last(x$BasalArea)/dplyr::last(x$Age))

  #CAI (Net?)
  x$CAI_Volume <- c(x$CAI_Volume, (dplyr::last(x$Volume)-dplyr::last(x$startVolume)+dplyr::last(x$VolumeThinned))/incrementPeriod)
  x$CAI_BasalArea <- c(x$CAI_BasalArea, (dplyr::last(x$BasalArea)-dplyr::last(x$startBasalArea)+dplyr::last(x$BasalAreaThinned))/incrementPeriod)


  return(
    x
  )

}


#Repetitive Periods

grow_repeat <-  function(x,...){
  UseMethod('grow_repeat',x)
}

#Scheduling Thinning to be added.
grow_repeat.AllenStand <- function(x,incrementPeriod=5,finalAge=130, cutHeights=NULL, cutAges=NULL, cutBasalAreaPercent=NULL){

  stopifnot('Ages to cut at must be given (not NA )or NULL.' = all(!is.na(cutAges)) | is.null(cutAges))
  stopifnot('Heights to cut at must be given (not NA )or NULL.' = all(!is.na(cutHeights)) | is.null(cutHeights))
  stopifnot('Amounts to cut must be given (not NA )or NULL.' = all(!is.na(cutBasalAreaPercent)) | is.null(cutBasalAreaPercent))
  stopifnot('Final age needs to be given..'=!is.null(finalAge))
  stopifnot('Each thinning must be provided with an amount to thin.'=length(cutBasalAreaPercent)==(length(cutHeights)+length(cutAges)))

  #Ensure these are arranged in increasing order
  if(!is.null(cutHeights)) cutHeights <- sort(cutHeights)
  if(!is.null(cutAges)) cutAges <- sort(cutAges)

  #Translate cutAges to heights to work with height only
  if(!is.null(cutAges)){
    cutHeights2 <- Allen_2020_height_development_Norway_spruce(20, #If current height at...
                                                               100,#Current age...
                                                               cutAges#Then what height at end of period?
    )

    #Append to list.
    cutHeights = c(cutHeights,cutHeights2)
    cutHeights = sort(cutHeights)
  }

  #Main loop
  while(dplyr::last(x$Age)<finalAge){

    #Defaults
    cutHeight=NULL
    cutAmount=NULL


      #This periods intended length is full length for as long as it doesn't exceed finalAge.
      #This can be shortened in the loop if there are several activities happening in same period.
      thisPeriod= ifelse(finalAge>=(dplyr::last(x$Age)+incrementPeriod),incrementPeriod,finalAge-dplyr::last(x$Age))

      if(!is.null(cutBasalAreaPercent)){ #If there is any thinning queued
        #Check if thinning is to be conducted
        currentHeight <- dplyr::last(x$Height)
        nextHeight <- Allen_2020_height_development_Norway_spruce(currentHeight, #If current height at...
                                                                  dplyr::last(x$Age),#Current age...
                                                                  dplyr::last(x$Age)+thisPeriod #Then what height at end of period?
        )

        #Is any cutting Height fulfilled?
        if(!is.null(cutHeights)){
          if(any(cutHeights>=currentHeight & cutHeights<nextHeight)){
            #Decrease increment period if more than one thinning to be conducted in same period at different time points..
            while(sum(cutHeights>=currentHeight & cutHeights<nextHeight)>1){

              #If approximately equal in time..
              if(thisPeriod<=2){
                cat('Thinnings scheduled within 2 years of eachother aggregated. Thinning to occur at first timing.')
                firstHeight = cutHeights[which(cutHeights>=currentHeight & cutHeights<nextHeight)][1] #Grab first
                cutHeights = cutHeights[-which(cutHeights>=currentHeight & cutHeights<nextHeight)] #Remove both.
                cutHeights = c(cutHeights,firstHeight) #append
                cutHeights = sort(cutHeights) #sort.
                rm(firstHeight) #Free memory..
               #Percentage to cut to add together occurs later..
                next #Skip division of period part of loop.
              }

              #Divide period in half if more than one thinning during period.
              thisPeriod = thisPeriod/2
              #Get new height at end of period
              nextHeight <- Allen_2020_height_development_Norway_spruce(currentHeight, #If current height at...
                                                                        dplyr::last(x$Age),#Current age...
                                                                        dplyr::last(x$Age)+thisPeriod #Then what height at end of period?
              )

            }

            #Use same index to get Height and amount.
            cutHeight  = cutHeights[which(cutHeights>=currentHeight & cutHeights<nextHeight)]
            cutAmount  = (sum(cutBasalAreaPercent[which(cutHeights>=currentHeight & cutHeights<nextHeight)],na.rm=TRUE)/100)*dplyr::last(x$BasalArea)

          }

        }
      }

        x <- grow(x,incrementPeriod=thisPeriod,thinned_basalArea=cutAmount,ThinningHeight=cutHeight)

  }


  return(
    x
  )
}


##Examples from the article..
#
# #Input stands.
# Stand1 <- new_AllenStand(Age = 46,SI40 = 11,Stems = 2977,BasalArea = 31.3,Volume = 155.9)
# Stand2 <- new_AllenStand(34,14,3090,26.2,125.9)
# Stand3 <- new_AllenStand(25,17,2038,22.7,109.6)
# Stand4 <- new_AllenStand(22,20,2710,24.6,115.9)
#
# #Mgmt 1 25% BA removed at 12 m H
# #Mgmt 2 25% BA removed at 12 m H & 25% BA removed at 16 m.
#
#
# #Unthinned stand 1
# Stand1 %>%
#   grow_repeat(incrementPeriod=5,finalAge=115,cutHeights=c(16,24),cutBasalAreaPercent=c(25,25)) %>%
#   as.data.frame() %>%
#   ggplot(aes(x=Age))+
#   geom_line(aes(y=Volume,linetype='Volume'))+
#   geom_point(aes(y=Volume))+
#   geom_line(aes(y=Total_Volume,linetype='Total Volume'))+
#   geom_point(aes(y=Total_Volume))+
#   geom_col(aes(y=VolumeThinned,fill='Volume Thinned'))+
#   scale_x_continuous(lim=c(20,120),breaks=seq(20,100,by=20),labels=seq(20,100,by=20))+
#   scale_y_continuous(lim=c(0,1000),breaks=seq(0,1000,by=200),labels=seq(0,1000,by=200))
#
# #Need to include NET volume increment! i.e. stems lost..
#
#
# Stand1 %>%
#   grow_repeat(incrementPeriod=5,finalAge=115,cutHeights=c(16,24),cutBasalAreaPercent=c(25,25)) %>%
#   as.data.frame() %>%
#   ggplot(aes(x=Age))+
#   geom_line(aes(y=CAI_Volume))+
#   geom_point(aes(y=CAI_Volume))+
#   geom_line(aes(y=MAI_Volume))+
#   geom_point(aes(y=MAI_Volume))+
#   scale_x_continuous(lim=c(20,160),breaks=seq(20,160,by=20),labels=seq(20,160,by=20))+
#   scale_y_continuous(lim=c(2.5,25),breaks=seq(5,20,by=5),labels=seq(5,20,by=5))
#
#
# Stand1 %>%
#   grow_repeat(incrementPeriod=5,finalAge=115) %>%
#   as.data.frame() %>%
#   ggplot(aes(x=Age))+
#   geom_line(aes(y=CAI_Volume))+
#   geom_point(aes(y=CAI_Volume))+
#   geom_line(aes(y=MAI_Volume))+
#   geom_point(aes(y=MAI_Volume))+
#   scale_x_continuous(lim=c(20,160),breaks=seq(20,160,by=20),labels=seq(20,160,by=20))+
#   scale_y_continuous(lim=c(2.5,25),breaks=seq(5,20,by=5),labels=seq(5,20,by=5))
#
