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
