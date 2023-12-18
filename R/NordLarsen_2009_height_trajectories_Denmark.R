#' Site-specific height growth models for six common tree species in Denmark.
#'
#' @source Thomas Nord-Larsen , Henrik Meilby & Jens Peter Skovsgaard (2009)
#' Site-specific height growth models for six common tree species in Denmark,
#' Scandinavian Journal of Forest Research, 24:3, 194-204, DOI:
#' \url{https://doi.org/10.1080/02827580902795036}
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "Equation" or "Height".
#' @md
#'
#' @name nordlarsen2009
#'
#' @return m.
#' @export
#' @examples
#' # A ggplot of some common site indices for Beech in Denmark.
#' ggplot()+
#' geom_function(fun=function(x) NordLarsen_2009_height_trajectory_Denmark_Beech(dominant_height = 10,age=50,age2 = x))+
#' geom_function(fun=function(x) NordLarsen_2009_height_trajectory_Denmark_Beech(dominant_height = 15,age=50,age2 = x))+
#' geom_function(fun=function(x) NordLarsen_2009_height_trajectory_Denmark_Beech(dominant_height = 20,age=50,age2 = x))+
#' geom_function(fun=function(x) NordLarsen_2009_height_trajectory_Denmark_Beech(dominant_height = 25,age=50,age2 = x))+
#' geom_function(fun=function(x) NordLarsen_2009_height_trajectory_Denmark_Beech(dominant_height = 30,age=50,age2 = x))+
#' xlim(c(0,125))+
#' ylim(c(0,35))+
#' coord_cartesian(ylim=c(0,40))


NordLarsen_2009_height_trajectory_Denmark_Beech <- function(
    dominant_height,
    age,
    age2,
    output="Height"){
  b1 <- 1.7644
  b2 <- 3472.7999
  b3 <- 36.8037

  Z0 <- dominant_height - b3

  R <- Z0 + (Z0^2 + (2*b2*dominant_height)/(age^b1))^0.5

  if(output=="Height") return(dominant_height*(((age2^b1)*((age^b1)*R + b2))/((age^b1)*((age2^b1)*R + b2))))
  if(output=="Equation") return(paste0(dominant_height,"*(((age2^",b1,")*((",age,"^",b1,")*",R," + ",b2,"))/((",age,"^",b1,")*((age2^",b1,")*",R," + ",b2,")))"))
}

#' @export
#' @rdname nordlarsen2009
#' @md
#' @details Beech: R^2=0.991, RMSE cross-validation 1.619
NordLarsen_2009_height_trajectory_Denmark_Beech <- Vectorize(NordLarsen_2009_height_trajectory_Denmark_Beech)

NordLarsen_2009_height_trajectory_Denmark_Oak <- function(
    dominant_height,
    age,
    age2,
    output="Height"){

  warning("Authors warn that site indices beyond data range provide unrealistic
          growth patterns - e.g. SI50 = 30")

  b1 <- 1.6277
  b2 <- 1
  b3 <- 31.9667

  Z0 <- dominant_height - b3

  R <- Z0 + (Z0^2 + (2*b2*dominant_height)/(age^b1))^0.5

  if(output=="Height") return(dominant_height*(((age2^b1)*((age^b1)*R + b2))/((age^b1)*((age2^b1)*R + b2))))
  if(output=="Equation") return(paste0(dominant_height,"*(((age2^",b1,")*((",age,"^",b1,")*",R," + ",b2,"))/((",age,"^",b1,")*((age2^",b1,")*",R," + ",b2,")))"))
}

#' @export
#' @rdname nordlarsen2009
#' @md
#' @details Oak: R^2=0.990, RMSE cross-validation 1.269
NordLarsen_2009_height_trajectory_Denmark_Oak <- Vectorize(NordLarsen_2009_height_trajectory_Denmark_Oak)

NordLarsen_2009_height_trajectory_Denmark_Norway_Spruce <- function(
    dominant_height,
    age,
    age2,
    output="Height"){
  b1 <- 2.0323
  b2 <- 6448.8721
  b3 <- 32.5165

  Z0 <- dominant_height - b3

  R <- Z0 + (Z0^2 + (2*b2*dominant_height)/(age^b1))^0.5

  if(output=="Height") return(dominant_height*(((age2^b1)*((age^b1)*R + b2))/((age^b1)*((age2^b1)*R + b2))))
  if(output=="Equation") return(paste0(dominant_height,"*(((age2^",b1,")*((",age,"^",b1,")*",R," + ",b2,"))/((",age,"^",b1,")*((age2^",b1,")*",R," + ",b2,")))"))
}

#' @export
#' @rdname nordlarsen2009
#' @md
#' @details Norway Spruce: R^2=0.982, RMSE cross-validation 1.740
NordLarsen_2009_height_trajectory_Denmark_Norway_Spruce <- Vectorize(NordLarsen_2009_height_trajectory_Denmark_Norway_Spruce)

NordLarsen_2009_height_trajectory_Denmark_Sitka_Spruce <- function(
    dominant_height,
    age,
    age2,
    output="Height"){
  b1 <- 2.1538
  b2 <- -2870.2666
  b3 <- 38.8609

  Z0 <- dominant_height - b3

  R <- Z0 + (Z0^2 + (2*b2*dominant_height)/(age^b1))^0.5

  if(output=="Height") return(dominant_height*(((age2^b1)*((age^b1)*R + b2))/((age^b1)*((age2^b1)*R + b2))))
  if(output=="Equation") return(paste0(dominant_height,"*(((age2^",b1,")*((",age,"^",b1,")*",R," + ",b2,"))/((",age,"^",b1,")*((age2^",b1,")*",R," + ",b2,")))"))
}

#' @export
#' @rdname nordlarsen2009
#' @md
#' @details Sitka Spruce: R^2=0.984, RMSE cross-validation 1.583
NordLarsen_2009_height_trajectory_Denmark_Sitka_Spruce <- Vectorize(NordLarsen_2009_height_trajectory_Denmark_Sitka_Spruce)

NordLarsen_2009_height_trajectory_Denmark_Silver_Fir<- function(
    dominant_height,
    age,
    age2,
    output="Height"){
  b1 <- 2.1708
  b2 <- 140562.7517
  b3 <- 20.4927

  Z0 <- dominant_height - b3

  R <- Z0 + (Z0^2 + (2*b2*dominant_height)/(age^b1))^0.5

  if(output=="Height") return(dominant_height*(((age2^b1)*((age^b1)*R + b2))/((age^b1)*((age2^b1)*R + b2))))
  if(output=="Equation") return(paste0(dominant_height,"*(((age2^",b1,")*((",age,"^",b1,")*",R," + ",b2,"))/((",age,"^",b1,")*((age2^",b1,")*",R," + ",b2,")))"))
}

#' @export
#' @rdname nordlarsen2009
#' @md
#' @details Silver Fir: R^2=0.989, RMSE cross-validation 1.263
NordLarsen_2009_height_trajectory_Denmark_Silver_Fir <- Vectorize(NordLarsen_2009_height_trajectory_Denmark_Silver_Fir)

NordLarsen_2009_height_trajectory_Denmark_Douglas_Fir<- function(
    dominant_height,
    age,
    age2,
    output="Height"){
  b1 <- 1.8
  b2 <- 1
  b3 <- 44.4553

  Z0 <- dominant_height - b3

  R <- Z0 + (Z0^2 + (2*b2*dominant_height)/(age^b1))^0.5

  if(output=="Height") return(dominant_height*(((age2^b1)*((age^b1)*R + b2))/((age^b1)*((age2^b1)*R + b2))))
  if(output=="Equation") return(paste0(dominant_height,"*(((age2^",b1,")*((",age,"^",b1,")*",R," + ",b2,"))/((",age,"^",b1,")*((age2^",b1,")*",R," + ",b2,")))"))
}

#' @export
#' @rdname nordlarsen2009
#' @md
#' @details Douglas Fir: R^2=0.989, RMSE cross-validation 1.894
NordLarsen_2009_height_trajectory_Denmark_Douglas_Fir <- Vectorize(NordLarsen_2009_height_trajectory_Denmark_Douglas_Fir)
