#' Hägglund 1973 Height growth of Spruce in southern Sweden.
#'
#' @description Largely based on the Fortran IV script appended to the original source.
#' Does not include certain functions to calculate if age at breast height is unknown,
#' and has been adapted to R such that it will return one value at a time rather than
#' print a matrix for height developments.
#'
#' @details OBSERVE! Will throw warnings if outside of material, but not stop.
#'
#' OBSERVE! The SIH100 returned is H100 TOTAL AGE, whereas if you enter age2 as 100 it is age at breast height.
#'
#' In order to calculate Height at another point in time (age2 at breast height) based on a SIH100 total age (age1 total age) - subtract the time to breast height from age1.
#' e.g. From Total Age 100, H=16 to age at breast height age2.
#' Hagglund_1973_southern_Sweden_Height_trajectories_Spruce(16,age=100-forester::Hagglund_1973_Norway_Spruce_time_to_BH_southern_Sweden(16,1,61),age2=140,61,1)
#'
#' Based on the Chapman-Richards function.
#'
#' @seealso [forester::Hagglund_1973_Norway_spruce_time_to_BH_southern_Sweden()]
#'
#' @source Hägglund, Björn (1973) Om övre höjdens utveckling för gran i södra
#'  Sverige: Site index curves for Norway Spruce in southern Sweden. Diss. Dept.
#'  of Forest Yield Research. Royal College of Forestry. Report 24. 49 pp. Stockholm.
#' @param dominant_height Dominant height of stand, m.
#' @param age Age of stand or tree at *breast height 1.3 m.*
#' @param age2 Necessary if output is "Height". The age for which height along
#' the same curve is to be computed.
#' @param output One of "SIH100", "Height", or "Equation".
#'
#' @return If output is "SIH100", the numeric value for the height (m) a stand
#' will reach at age 100.
#'
#'  If output is "Height", the numeric value for the height (m) a stand will
#'  reach at age2.
#'
#'  If output is "Equation", a named list with 2 elements: I) "Equation": text response with the equation for that height
#'  curve. II) "T13": The time taken to reach breast height 1.3m.
#' @export
#'
#' @examples
#' Hagglund_1973_southern_Sweden_Height_trajectories_Spruce(dominant_height = 22,age = 40,age2 = 40)


Hagglund_1973_southern_Sweden_Height_trajectories_Spruce <- function(dominant_height,
                                                                     age,
                                                                     age2,
                                                                     output="Height"
){

  if(!(output%in%c("SIH100","Height","Equation"))){
    stop("Output must be one of 'SIH100', 'Height' or 'Equation'.")
  }

  if(output=="Height" & missing(age2)){
    stop("Height at age2 cannot be calculated without age2")
  }

  top_height_dm <- dominant_height*10



  top_height_dm <- top_height_dm - 13


  subroutine_bonitering <- function(
    top_height,
    age){

    AI1 <- 10
    AI2 <- 600

    while(abs(AI1-AI2)>1){

      AI3 <- (AI1+AI2)/2

      RK <- 0.042624-7.1145/AI3^1.0068
      #try(print(A2))
      A2 <- 1.0017*AI3^0.99808

      RM <- 0.15933+3.7*10^6/AI3^3.156

      if(RM>0.95){
        RM <- 0.95
      }

      RM2 <- 0.98822/(1-RM)

      if(RK<0.0001){
        RK <- 0.0001
      }

      DIF <- top_height-A2*(1-exp(-age*RK))^RM2


      ifelse(DIF<=0,
             assign("AI2",AI3),
             assign("AI1",AI3)
             )

      T26 <- (-1/RK)*log(1-(13/A2)^(1/RM2))
      T13 <- 4.9546+0.63934*T26+0.031992*T26*T26


    }

    params <- list("A2"=A2,"RK"=RK,"RM2"=RM2, "T26"=T26,"T13"=T13)
    return(params)

  }

  params <- subroutine_bonitering(top_height = top_height_dm,
                                  age = age)


  if(params$A2>400){
    warning("Too high productivity, outside of the material.")

  }

  if(params$A2<250){
    warning("Too low productivity, outside of the material.")

  }

  if(params$A2>375 & top_height_dm>267){
    warning("Too old stand, outside of the material.")

  }

  if(age>90){
    warning("Too old stand, outside of the material.")
  }



  if(output=="SIH100"){
    return((13+params$A2*(1-exp((params$T13-100)*params$RK))^params$RM2)/10)

  }

  if(output=="Height"){
    return((13+params$A2*(1-exp(-age2*params$RK))^params$RM2)/10)
  }

  if(output=="Equation"){
    return(list("Equation"=(paste0("y~(13+",params$A2,"*(1-exp(-age*",params$RK,"))^",params$RM2,")/10")),
                "T13"=params$T13))
  }


}






