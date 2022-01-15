#' New Dynamic top height models for Great Britain
#'
#' @description GADA functions to replace Edwards & Christie 1981 used in
#' 'Forest Yield' & 'MOSES_GB'.
#'
#' @source Manso, R., McLean J.P., Arcangeli, C., Matthews, R. (2020) Dynamic
#' top height models for several major forest tree species in Great Britain.
#' Forestry: An International Journal of Forest Research, Vol 94:2, pp. 181-192.
#'  Available Online (15/01/2022): \url{https://doi.org/10.1093/foresj/cpaa036}
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output.
#' @param output One of "SIH100","Equation" or "Height" (default).
#'
#' @name Manso2020
#'
#' @export
#'
#' @examples
#' ggplot()+geom_function( fun= function(x) Manso_2020_height_trajectory_Great_Britain_Western_Hemlock(dominant_height=25,age=50,age2=x))+xlim(c(0,200))
Manso_2020_height_trajectory_Great_Britain_Beech <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0241
  b2 <- -5.7098
  b3 <- 25.1162


  L <- log(1-exp(-b1*age))

  X <- 0.5* (log(dominant_height) - b2*L + sqrt((log(dominant_height) - b2*L)^2 - (4*b3*L)))

  if(output=="SIH100"){
    return(
      dominant_height*(((1-exp(-b1*100))/(1-exp(-b1*age)))^(b2+(b3/X)))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*(((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^(",b2,"+(",b3,"/",X,")))")
    )
  }

  if(output=="Height"){
  return(
    dominant_height*(((1-exp(-b1*age2))/(1-exp(-b1*age)))^(b2+(b3/X)))
  )
  }
}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Corsican_Pine <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0172
  b2 <- 1.3424

  X <- dominant_height/(1-exp(-b1*age))

  if(output=="SIH100"){
    return(
      dominant_height*((1-exp(-b1*100))/(1-exp(-b1*age)))^b2
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^",b2)
    )
  }

  if(output=="Height"){
  return(
    dominant_height*((1-exp(-b1*age2))/(1-exp(-b1*age)))^b2
  )
  }

}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Douglas_Fir <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0207
  b2 <- 1.3872

  X <- dominant_height/(1-exp(-b1*age))

  if(output=="SIH100"){
    return(
      dominant_height*((1-exp(-b1*100))/(1-exp(-b1*age)))^b2
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^",b2)
    )
  }

  if(output=="Height"){
  return(
    dominant_height*((1-exp(-b1*age2))/(1-exp(-b1*age)))^b2
  )
  }

}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_European_Larch <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }
  b1 <- 0.0232
  b2 <- 0.3944
  b3 <- 2.7545


  L <- log(1-exp(-b1*age))

  X <- 0.5* (log(dominant_height) - b2*L + sqrt((log(dominant_height) - b2*L)^2 - (4*b3*L)))

  if(output=="SIH100"){
    return(
      dominant_height*(((1-exp(-b1*100))/(1-exp(-b1*age)))^(b2+(b3/X)))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*(((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^(",b2,"+(",b3,"/",X,")))")
    )
  }

  if(output=="Height"){
  return(
    dominant_height*(((1-exp(-b1*age2))/(1-exp(-b1*age)))^(b2+(b3/X)))
  )
  }


}


#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Lodgepole_Pine <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }
  b1 <- 0.0164
  b2 <- 1.3032

  X <- dominant_height/(1-exp(-b1*age))

  if(output=="SIH100"){
    return(
      dominant_height*((1-exp(-b1*100))/(1-exp(-b1*age)))^b2
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^",b2)
    )
  }

  if(output=="Height"){
  return(
    dominant_height*((1-exp(-b1*age2))/(1-exp(-b1*age)))^b2
  )
  }

}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Norway_Spruce <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }
  b1 <- 0.0237
  b2 <- 1.5540

  X <- dominant_height/(1-exp(-b1*age))

  if(output=="SIH100"){
    return(
      dominant_height*((1-exp(-b1*100))/(1-exp(-b1*age)))^b2
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^",b2)
    )
  }

  if(output=="Height"){
  return(
    dominant_height*((1-exp(-b1*age2))/(1-exp(-b1*age)))^b2
  )
  }

}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Poplar <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0454
  b2 <- 0.9683

  X <- dominant_height/(1-exp(-b1*age))

  if(output=="SIH100"){
    return(
      dominant_height*((1-exp(-b1*100))/(1-exp(-b1*age)))^b2
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^",b2)
    )
  }

  if(output=="Height"){
  return(
    dominant_height*((1-exp(-b1*age2))/(1-exp(-b1*age)))^b2
  )
  }


}


#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Scots_Pine <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0204
  b2 <- -0.4206
  b3 <- 5.8544


  L <- log(1-exp(-b1*age))

  X <- 0.5* (log(dominant_height) - b2*L + sqrt((log(dominant_height) - b2*L)^2 - (4*b3*L)))

  if(output=="SIH100"){
    return(
      dominant_height*(((1-exp(-b1*100))/(1-exp(-b1*age)))^(b2+(b3/X)))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*(((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^(",b2,"+(",b3,"/",X,")))")
    )
  }

  if(output=="Height"){
  return(
    dominant_height*(((1-exp(-b1*age2))/(1-exp(-b1*age)))^(b2+(b3/X)))
  )
  }

}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Sitka_Spruce <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0261
  b2 <- -2.6577
  b3 <- 17.5827


  L <- log(1-exp(-b1*age))

  X <- 0.5* (log(dominant_height) - b2*L + sqrt((log(dominant_height) - b2*L)^2 - (4*b3*L)))

  if(output=="SIH100"){
    return(
      dominant_height*(((1-exp(-b1*100))/(1-exp(-b1*age)))^(b2+(b3/X)))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*(((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^(",b2,"+(",b3,"/",X,")))")
    )
  }

  if(output=="Height"){
  return(
    dominant_height*(((1-exp(-b1*age2))/(1-exp(-b1*age)))^(b2+(b3/X)))
  )
  }
}

#'@rdname Manso2020
#'@export
Manso_2020_height_trajectory_Great_Britain_Western_Hemlock <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  b1 <- 0.0179
  b2 <- -1.5299
  b3 <- 11.2673


  L <- log(1-exp(-b1*age))

  X <- 0.5* (log(dominant_height) - b2*L + sqrt((log(dominant_height) - b2*L)^2 - (4*b3*L)))

  if(output=="SIH100"){
    return(
      dominant_height*(((1-exp(-b1*100))/(1-exp(-b1*age)))^(b2+(b3/X)))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",dominant_height,"*(((1-exp(-",b1,"*",age2,"))/(1-exp(-",b1,"*age)))^(",b2,"+(",b3,"/",X,")))")
    )
  }
  return(
    dominant_height*(((1-exp(-b1*age2))/(1-exp(-b1*age)))^(b2+(b3/X)))
  )
}
