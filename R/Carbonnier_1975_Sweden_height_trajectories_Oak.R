#' Carbonnier 1975 Height growth of Oak plantations in Sweden.
#'
#' @description Largely based on the Fortran IV script appended to the original source.
#' Checked against table VI.1 (ibid), p. 66.
#'
#' This function was written by Björn Hägglund following the same method as outlined
#' in Hägglund 1972.
#'
#' @details
#' multiple correlation coefficient: 0.995
#'
#' 100*spread around the function/spread around the mean: 10.1%
#'
#' spread around the mean of ln(height): 0.30096
#'
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 89.
#'
#' @param total_age total age of stand or tree.
#' @param top_height_m Top height of tree or stand in m.
#' @param age_2 Necessary if output is "Height". The age for which height along
#' the same curve is to be computed.
#' @param output One of "SIH100", "Height", or "Equation".
#'
#' @return If output is "SIH100", the numeric value for the height (m) a stand
#' will reach at age 100.
#'
#'  If output is "Height", the numeric value for the height (m) a stand will
#'  reach at age_2.
#'
#'  If output is "Equation", a text response with the equation for that height
#'  curve.
#' @export
#'
#' @examples
#' Carbonnier_1975_Sweden_height_trajectories_Oak(total_age = 50,top_height_m = 16.9,age_2 = 100,output = "Height")

Carbonnier_1975_Sweden_height_trajectories_Oak <- function(total_age,
                                                          top_height_m,
                                                          age_2,
                                                          output="SIH100"
){

  if(missing(output)){
    stop("Output must be defined.")
  }

  if(!(output%in%c("SIH100","Height","Equation"))){
    stop("Output must be one of 'SIH100', 'Height' or 'Equation'.")
  } else if(output=="Height" && missing(age_2)){
    stop("Height at age_2 cannot be calculated without age_2")
  }

  if(total_age<0){
    stop("Total age must be between 0 and 170.")
  }

  if(total_age>170){
    warning("Stand probably too old, outside of material.")
  }

  top_height_dm <- top_height_m*10



  subroutine_bonitering <- function(
    top_height,
    total_age){

    AI1 <- 10
    AI2 <- 600

    while(abs(AI1-AI2)>1){

      AI3 <- (AI1+AI2)/2
      RM <- 1.008/(0.78222+2.3283*AI3/10^10)

      RK <- 0.013552+3.4923*AI3/10^5

      A2 <- 1.00046*AI3



      DIF <- top_height-A2*(1-exp(-total_age*RK))^RM

      if(DIF<=0){
        AI2 <- AI3
      } else {
        AI1 <- AI3
      }


    }


    params <- list("A2"=A2,"RK"=RK,"RM"=RM)
    return(params)

  }

  params <- subroutine_bonitering(top_height = top_height_dm,
                                  total_age = total_age)

  if(output=="SIH100"){
    H100 <- params$A2*(1-exp(-100*params$RK))^params$RM
    return(H100/10)
  } else if(output=="Height"){
    return((params$A2*(1-exp(-age_2*params$RK))^params$RM)/10)
  } else if(output=="Equation"){
    return("Equation"=paste0("y~(",params$A2,"*(1-exp(-age*",params$RK,"))^",params$RM,")/10")
           )
  }


}

