#' Hägglund 1974 Height growth of Scots Pine in Sweden.
#'
#' @description Largely based on the Fortran IV script appended to the original source.
#' Does not include certain functions to calculate if age at breast height is unknown,
#' and has been adapted to R such that it will return one value at a time rather than
#' print a matrix for height developments.
#'
#' @details OBSERVE! Will throw warnings if outside of material, but not stop.
#'
#' Based on the Chapman-Richards function. f.4.1
#'
#' @source Hägglund, Björn (1974) Övre höjdens utveckling i tallbestånd:
#' Site index curves for Scots Pine in Sweden. Dept.
#'  of Forest Yield Research. Royal College of Forestry. Report 31. 54 pp. Stockholm.
#' @param dominant_height Top height of tree or stand in m.
#' @param age Age of stand or tree at breast height 1.3 m.
#' @param age2 Necessary if output is "Height". The age for which height along
#' the same curve is to be computed.
#' @param regeneration How was the stand established? One of "culture", "natural" or "unknown".
#' @param output One of "Height" (default), "SIH100", or "Equation".
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
#' ggplot()+
#' scale_x_continuous(limits = c(0,120),expand = c(0,0))+
#' scale_y_continuous(limits=c(0,27),expand = c(0,0))+
#' #expand_limits(x=c(0,130),y=c(0,30))+
#' geom_function(aes(linetype="a"), fun = function (age)
#'  #20
#'  (13+233.564868164063*(1-exp(-age*0.0172925395397362))^1.14013833378919)/10
#' )+
#' geom_function(aes(linetype="b"), fun = function(age)
#'
#'  #22
#'  (13+248.657690429688*(1-exp(-age*0.0188389760256776))^1.12794900485356)/10
#'
#' )+
#'
#' geom_function(aes(linetype="c"), fun= function(age)
#'
#'  #24
#'  (13+264.911499023438*(1-exp(-age*0.0205442213276424))^1.11788897659132)/10
#'
#' )+
#'
#' geom_function(aes(linetype="d"), fun= function(age)
#'
#'   #16
#'   (13+203.379223632813*(1-exp(-age*0.0143125270333732))^1.17804835993454)/10
#'
#' )+
#' theme_classic()+
#' theme(plot.title = element_text(face="italic",family="serif"),
#'       plot.margin = unit(c(10,20,10,10), units="points"),
#'       legend.position = "none")+
#' labs(
#'   title="Development of Dominant height of\nScots pine in Sweden",
#'   caption="After Hägglund (1974) f. 4.1.",
#'   x="Age at breast height (years)",
#'   y="Dominant Height (m)"
#' )+
#' annotate("text",x=115, y=26,label="24",size=3)+
#' annotate("text",x=115, y=24, label="22", size=3)+
#' annotate("text",x=115, y=22, label="20",size=3)+
#' annotate("text",x=115, y=18, label="16",size=3)


Hagglund_1974_Sweden_height_trajectories_Pine <- function(dominant_height,
                                                          age,
                                                          age2,
                                                          regeneration = "culture",
                                                          output = "Height"
){

  if(!(output%in%c("SIH100","Height","Equation"))){
    stop("Output must be one of 'SIH100', 'Height' or 'Equation'.")
  }

  if(output=="Height" & missing(age2)){
    stop("Height at age2 cannot be calculated without age2")
  }

  if(!(regeneration%in%c("culture","natural","unknown"))){
    stop("Argument regeneration must be one of 'culture','natural','unknown'.")
  }

  top_height_dm <- dominant_height*10

  top_height_dm <- top_height_dm - 13

  if(age>120){
    warning("Too old stand, outside of the material.")
  }



  subroutine_bonitering <- function(
    top_height,
    age,
    regeneration){

    AI1 <- 10
    AI2 <- 600

    while(abs(AI1-AI2)>1){

      AI3 <- (AI1+AI2)/2
      RM <- 0.066074+4.4189*10^5/AI3^2.9134
      if(RM>0.95){
        RM <- 0.95
      }

      RM2 <- 1.000/(1-RM)

      RK <- 1.0002/10^4+9.5953*AI3^1.3755/10^6
      if(RK<0.0001){
        RK <- 0.0001
      }

      A2 <- 1.0075*AI3



      DIF <- top_height-A2*(1-exp(-age*RK))^RM2

      ifelse(DIF<=0,
             assign("AI2",AI3),
             assign("AI1",AI3)
      )


    }

    T26 <- (-1/RK)*log(1-(13/A2)^(1/RM2))
    T262 <- T26^2

    if(regeneration=="natural"){
      T13 <- 7.4624+0.11672*T262
    }

    if(regeneration=="unknown"){
      T13 <- 6.8889+0.12405*T262
    }

    if(regeneration=="culture"){
      T13 <- 7.4624+0.11672*T262-0.39276*T26
    }

    if(T13>50){
      T13 <- 50
    }

    params <- list("A2"=A2,"RK"=RK,"RM2"=RM2, "T13"=T13)
    return(params)

  }

  params <- subroutine_bonitering(top_height = top_height_dm,
                                  age = age,
                                  regeneration = regeneration)


  if(params$A2>311){
    warning("Too high productivity, outside of the material.")

  }

  if(params$A2<180){
    warning("Too low productivity, outside of the material.")

  }

  if(params$A2>250 & age>100){
    warning("Too old stand, outside of material.")
  }

  if(output=="SIH100"){
    return((13+params$A2*(1-exp((params$T13-100)*params$RK))^params$RM2)/10)

  } else if(output=="Height"){
    return((13+params$A2*(1-exp(-age2*params$RK))^params$RM2)/10)
  } else if(output=="Equation"){
    return(list("Equation"=paste0("y~(13+",params$A2,"*(1-exp(-age*",params$RK,"))^",params$RM2,")/10"),
                "T13"=params$T13))
  }


}


