#' Dominant height trajectory for Norway Spruce in Bayern, Germany.
#'
#' @source Assmann, E. & Franz, F. 1965. Vorläufige Fichten-Ertragstafel für
#' Bayern. Forstwissenschaftliches Centralblatt 84(1/2):13-43. Available online (2022-04-06):
#' \url{https://www.waldwachstum.wzw.tum.de/fileadmin/publications/Assmann_1965_Vorlaeufige_Fichten-Ertragstafel.pdf}
#'
#' @param dominant_height Dominant height of the stand, m.
#' @param age Age of the stand.
#' @param age2 Desired age of the stand.
#' @param output "Height" (default) or "Equation"
#'
#' @return Height of the stand in metres at the desired age.
#' @export

Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  #Find SI  function
  if(age!=100){
  find_SI <- function(SIH100,
                      dominant_height,
                      age){
    a1 <- 0.4515343E01 + -0.2665320E-01*exp(log(SIH100)) + -0.2203244E-06 *exp((log(SIH100))^2)
    a2 <- -0.8840923E00 + 0.2262338E-02*exp(log(SIH100)) + 0.8994708E-07 *exp((log(SIH100))^2)

    a0 <- log10(SIH100) - a1*log10(100) - a2*((log10(100))^2)

    return(
      abs(dominant_height-10^(a0 + a1 *log10(age) + a2*(log10(age)^2)))
    )
  }

  #Find SI by optimising for lowest discrepancy between dominant height, age pair and OH.
  SI100 <- optimise(find_SI,lower=0,upper=100,dominant_height={dominant_height},age={age})[[1]]

  }

  if(age==100){
    SI100 <- dominant_height
  }

  a1 <- 0.4515343E01 + -0.2665320E-01*exp(log(SI100)) + -0.2203244E-06 *exp((log(SI100))^2)
  a2 <- -0.8840923E00 + 0.2262338E-02*exp(log(SI100)) + 0.8994708E-07 *exp((log(SI100))^2)

  a0 <- log10(SI100) - a1*log10(100) - a2*((log10(100))^2)

  if(output=="Height"){
    return(
      10^(a0 + a1 *log10(age2) + a2*(log10(age2)^2))
    )
  }

  if(output=="Equation"){
    return(
      paste0("10^(",a0,"+",a1,"*log10(age2)+",a2,"*log10((age2)^2))")
    )

  }

}

Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce <- Vectorize(Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce)

#
# Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce <- function(
#     dominant_height,
#     age,
#     SIH100,
#     YieldLevel = 3
# ){
#
#   # First calculate the Site Index ("OH".)
#
#
#
#
#
#
#   # Now calculate the difference between the dominant height and the HL.
#   paramsB<- switch(
#     YieldLevel,
#     list("b00"=1.95,"b01"=0.019,"b10"=158.4,"b11"=-2.16,"b20"=0.02584,"b21"=0.01553),
#     list("b00"=2.3,"b01"=0.015,"b10"=160,"b11"=-2.5,"b20"=-0.0202,"b21"=0.01783),
#     list("b00"=2.7,"b01"=0.01,"b10"=160,"b11"=-2.75,"b20"=-0.06899,"b21"=0.02027)
#     )
#
#   D2 <- paramsB[["b00"]]+paramsB[["b01"]]*SIH100
#   A2 <- paramsB[["b10"]]+paramsB[["b11"]]*SIH100 #
#   #NB D1 calculation is missing exp in paper!
#   D1 <- exp(paramsB[["b20"]]+paramsB[["b21"]]*SIH100) #Diff at age 20.
#
#   A1  = 20
#
# #At age 100 should be 2.8m
#
#
#
#
#   b0 = log(D1) - b1*log(20) - b2*log(log(20))
#
#
#
#   return(
# log(dominant_height-HL) = b0 + b1*log(age)+ b2*log(log(age))
#   )
#
#
# }
#
#
# #examples
#
