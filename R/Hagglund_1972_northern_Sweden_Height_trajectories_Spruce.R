#' Hägglund 1972 Height growth of Spruce in Northern Sweden.
#'
#' @description Largely based on the Fortran IV script appended to the original source.
#' Does not include certain functions to calculate if age at breast height is unknown,
#' and has been adapted to R such that it will return one value at a time rather than
#' print a matrix for height developments.
#'
#'
#' @details OBSERVE! Will throw warnings if outside of material, but not stop.
#'
#' OBSERVE! The SIH100 returned is H100 TOTAL AGE, whereas if you enter age2 as 100 it is age at breast height.
#'
#' In order to calculate Height at another point in time (age2 at breast height) based on a SIH100 total age (age1 total age) - subtract the time to breast height from age1.
#' e.g. From Total Age 100, H=16 to age at breast height age2.
#' Hagglund_1972_northern_Sweden_Height_trajectories_Spruce(16,age=100-forester::Hagglund_1972_Norway_Spruce_time_to_BH_northern_Sweden(16,1,61),age2=140,61,1)
#'
#'
#' Based on the Chapman-Richards function.
#'
#' @seealso [forester::Hagglund_1972_Norway_spruce_time_to_BH_northern_Sweden()]
#'
#' @source Hägglund, Björn (1972) Om övre höjdens utveckling för gran i norra
#'  Sverige: Site index curves for Norway Spruce in northern Sweden. Diss. Dept.
#'  of Forest Yield Research. Royal College of Forestry. Report 21. 298 pp. Stockholm.
#' @param dominant_height height of tree or stand in m.
#' @param age Age of stand or tree at *breast height 1.3 m.*
#' @param age2 Necessary if output is "Height". The age for which height along the same curve is to be computed.
#' @param culture 1 (Default) if stand is culture, otherwise 0.
#' @param latitude Latitude in degrees. For function 8.4, set to 0.
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
#' Hagglund_1972_northern_Sweden_Height_trajectories_Spruce(latitude = 60.1,age = 80, top_height_dm = 120,culture = 0,output = "Equation")
#' #"y~13+224.224608372892*(1-exp(-age*0.0113938018668758))^1.44412711110086"
#' Hagglund_1972_northern_Sweden_Height_trajectories_Spruce(latitude = 66.9,age = 80, top_height_dm = 120,culture = 0,output = "Equation")
#' #"y~13+228.827607753106*(1-exp(-age*0.0116464323317232))^1.52753582497555"
#'
#' #Remember to divide by 10 to go to metres from dm.
#' ggplot()+
#'   geom_function(aes(color="Lat=60.1°"), fun = function(age) (13+224.224608372892*(1-exp(-age*0.0113938018668758))^1.44412711110086)/10)+
#'   geom_function(aes(color="Lat=66.9°"), fun = function(age) (13+228.827607753106*(1-exp(-age*0.0116464323317232))^1.52753582497555)/10)+
#'   labs(x="Age at breast height (years)",
#'        y= "Dominant height (metres)")+
#'   xlim(c(0,150))+
#'   ylim(c(0,30))
#'
#'
Hagglund_1972_northern_Sweden_Height_trajectories_Spruce <- function(dominant_height,
                                                                     age,
                                                                     age2,
                                                                     latitude,
                                                                     culture=1,
                                                                     output="Height"
){

  if(!(output%in%c("SIH100","Height","Equation"))){
    stop("Output must be one of 'SIH100', 'Height' or 'Equation'.")
  }

  if(output=="Height" & missing(age2)){
    stop("Height at age2 cannot be calculated without age2")
  }

  if(missing(latitude)){
    stop("Cannot calculate without latitude.")
  }

  P <- 0.9175^culture

  top_height_dm <- dominant_height*10

  top_height_dm <- top_height_dm - 13

  if(age>(407-1.167*top_height_dm)){
    warning("Too old stand, outside of the material.")
  }

  if(latitude>=67 | latitude<=60){
    warning("Outside of latitudinal range, 60°<= L <= 67° N, using function 8.4 ")
    #function 8.4
    B <-  3.4501
    C <- 0.77518
    D <- -0.42579
    E <- 1.33935
  } else {
    #function 8.7
    B <- 3.3816
    C <- 0.77896
    D <- -1.24207 + 0.0014629*latitude*10
    E <- 1.25998
  }




subroutine_bonitering <- function(
    top_height,
    age,
    B,
    C,
    D,
    E){

    AI1 <- 10
    AI2 <- 600

    while(abs(AI1-AI2)>1){

    AI3 <- (AI1+AI2)/2

    RK <- 0.001936+0.00004100*AI3^1.0105
    #try(print(A2))
    A2 <- B*AI3**C

    RM2 <- D+E/(0.56721+0.000008*AI3^1.8008)

    DIF <- top_height-A2*(1-exp(-age*RK))^RM2

    ifelse(DIF<=0,
           assign("AI2",AI3),
           assign("AI1",AI3)
    )


    }

    params <- list("A2"=A2,"RK"=RK,"RM2"=RM2)
    return(params)

}

params <- subroutine_bonitering(top_height = top_height_dm,
                                age = age,
                                B=B,
                                C=C,
                                D=D,
                                E=E)


if(params$A2>336){
  warning("Too high productivity, outside of the material.")

}

if(params$A2<189){
  warning("Too low productivity, outside of the material.")
}

T26 <- (-1/params$RK)*log(1-(13/params$A2)^(1/params$RM2))
T13 <- P*(7.0287+0.66118*T26)

if(output=="SIH100"){
  return((13+params$A2*(1-exp((T13-100)*params$RK))^params$RM2)/10)

} else if(output=="Height"){
  return((13+params$A2*(1-exp(-age2*params$RK))^params$RM2)/10)
} else if(output=="Equation"){
  return(list("Equation"=paste0("y~(13+",params$A2,"*(1-exp(-age*",params$RK,"))^",params$RM2,")/10"),
              "T13"=T13))
}


}



