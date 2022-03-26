#' Dominant height trajectories for Scots Pine and Norway Spruce in Finland
#' from Gustavsen, 1980.
#' @source Gustavsen, H-G (1980). Talousmetsien kasvupaikkaluokittelu
#' valtapituuden avulla. (Site index curves for conifer stands in Finland.)
#' Folia Forestalia 454: 1-31. Available (12/03/2021):
#' \url{https://jukuri.luke.fi/handle/10024/522037}
#'
#' @description "Site index curves for natural Scots Pine.. and Norway spruce
#' stands in Finland, based on 1069 temporary sample plots of 0.1 ha in pine-dominated
#' and 617 plots in spruce-dominated stands from the 3rd national forest inventory
#' (1951-53)".
#'
#' @param dominant_height Dominant height, in metres.
#' @param age Total age
#' @param age2 Total age at output.
#' @name gustavsen1980
#' @export
Gustavsen_1980_height_trajectory_Finland_Scots_Pine <- function(
    dominant_height,
    age,
    age2
    ){
  #Internal calc SI100
  Gustavsen_1980_SI100_Finland_Scots_Pine <- function(age,dominant_height){
    nim=log(dominant_height)-log(128.229)
    sel=exp((4.70248/(age^0.47692))-(4.70248/(100^0.47692)))
    return(
      128.229*(exp(nim/sel))
    )
  }

  SI100 <- Gustavsen_1980_SI100_Finland_Scots_Pine(age = {age},dominant_height = {dominant_height})

  dominant_height <- {dominant_height}

  hd_df <- data.frame(age=numeric(),height=numeric(),increment=numeric())
  for(i in age:age2){
    increment = (dominant_height * log(SI100/128.229) * exp(4.70248/i^0.47692 - 4.70248/100^0.47692) * (-2.24269/i^1.47692))
    hd_df <- bind_rows(hd_df,data.frame("age"=i,"height"=dominant_height,"increment"=increment))
    dominant_height <- if(age<age2) (dominant_height+increment) else (dominant_height-increment)
  }

  return(
    hd_df[,"height"][nrow(hd_df)]
  )
}

#' @export
#' @rdname gustavsen1980
Gustavsen_1980_height_trajectory_Finland_Norway_Spruce <- function(
    dominant_height,
    age,
    age2
){

  #Internal calc SI100
  Gustavsen_1980_SI100_Finland_Norway_Spruce <- function(age,dominant_height){
    nim=log(dominant_height)-log(147.481)
    sel=exp((4.64631/(age^0.29981))-(4.64631/(100^0.29981)))
    return(
      147.481*(exp(nim/sel))
    )
  }

  SI100 <- Gustavsen_1980_SI100_Finland_Norway_Spruce(age = {age},dominant_height = {dominant_height})

  dominant_height <- {dominant_height}

  hd_df <- data.frame(age=numeric(),height=numeric(),increment=numeric())
  for(i in age:age2){
    increment = (dominant_height * log(SI100/147.481) * exp(4.64631/i^0.29981 - 4.64631/100^0.29981) * (-1.39299/i^1.29981))
    hd_df <- bind_rows(hd_df,data.frame("age"=i,"height"=dominant_height,"increment"=increment))
    dominant_height <- if(age<age2) (dominant_height+increment) else (dominant_height-increment)
  }

  return(
    hd_df[,"height"][nrow(hd_df)]
  )
}

#Time to reach breast height, neg. exp.
# PineBh <- data.frame(SI100=seq(30,9,-3),t1.3=c(9,10,11,12,14,17,19,23))
# SpruceBh <- data.frame(SI100=seq(30,12,-3),t1.3=c(11,12,13,15,17,20,23))
#
# PineBhYr <- PineBh %>% lm(formula = log(t1.3)~SI100)
# SpruceBhYr <- SpruceBh %>% lm(formula = log(t1.3)~SI100)
#
# PineBh %>% mutate(Pred=exp(predict(PineBhYr)))
# SpruceBh %>% mutate(Pred=exp(predict(SpruceBhYr)))

#' Smoothing of time required to reach breast height according to Gustavsen 1980.
#' @details Both species modelled with log-linear function.
#' @param SI100 Site index 100, according to Gustavsen 1980.
#' @param species One of 'Picea abies' (default) or 'Pinus sylvestris'.
#' @return Number of years required to reach breast height 1.3m.
#' @export
#' @author Carl Vigren SLU
Gustavsen_1980_time_to_breast_height <- function(SI100,species="Picea abies"){
  if(species=="Picea abies") return(exp(3.607141 - 0.041699*SI100))

  if(species=="Pinus sylvestris") return(exp(3.486402 -0.044592*SI100))
}
