#' Hägglund 1973 Height growth of Spruce in southern Sweden.
#'
#' @description Largely based on the Fortran IV script appended to the original source.
#' Does not include certain functions to calculate if age at breast height is unknown,
#' and has been adapted to R such that it will return one value at a time rather than
#' print a matrix for height developments.
#'
#' @details OBSERVE! Will throw warnings if outside of material, but not stop.
#'
#' Based on the Chapman-Richards function.
#'
#' @source Hägglund, Björn (1973) Om övre höjdens utveckling för gran i södra
#'  Sverige: Site index curves for Norway Spruce in southern Sweden. Diss. Dept.
#'  of Forest Yield Research. Royal College of Forestry. Report 24. 49 pp. Stockholm.
#' @param age_at_breast_height Age of stand or tree at breast height 1.3 m.
#' @param top_height_dm Top height of tree or stand in dm.
#' @param age_2 Necessary if output is "Height". The age for which height along
#' the same curve is to be computed.
#' @param output One of "SIH100", "Height", or "Equation".
#'
#' @return If output is "SIH100", the numeric value for the height (dm) a stand
#' will reach at age 100.
#'
#'  If output is "Height", the numeric value for the height (dm) a stand will
#'  reach at age_2.
#'
#'  If output is "Equation", a text response with the equation for that height
#'  curve.
#' @export
#'
#' @examples
#'You can get the function of any Site Index Curve by inserting Height=x and Age= Reference Age.
#'Let's plot a series of SI curves for Spruce.
#'
#' ggplot()+
#'   geom_function(aes(linetype="40"), fun= function(age) (13+422.35797942705*(1-exp(-age*0.0266185526050425))^1.20201373284005)/10)+
#'   geom_function(aes(linetype="36"), fun= function(age) (13+384.775298598485*(1-exp(-age*0.0250409202452354))^1.21137203397815)/10)+
#'   geom_function(aes(linetype="32"), fun= function(age) (13+348.32473797798*(1-exp(-age*0.0231840367659744))^1.22519440445717)/10)+
#'   geom_function(aes(linetype="28"), fun= function(age) (13+314.145675076416*(1-exp(-age*0.0210495096196056))^1.24552388213254)/10)+
#'   geom_function(aes(linetype="24"), fun= function(age) (13+282.238764614593*(1-exp(-age*0.0185880465399335))^1.27615908436592)/10)+
#'   geom_function(aes(linetype="20"), fun= function(age) (13+253.744599459728*(1-exp(-age*0.0158640670067426))^1.3214218575403)/10)+
#'   geom_function(aes(linetype="16"), fun= function(age) (13+228.664654816028*(1-exp(-age*0.0129020244806058))^1.38858683017501)/10)+
#'   scale_y_continuous(limits=c(0,33),expand=c(0,0))+ #Enforce max height of 33 in picture; force 0 at origin.
#'   scale_x_continuous(expand=c(0,0))+ #Force 0 at origin.
#'   expand_limits(x=c(0,100),y=c(0,33))+ #Enforce plot limits
#'   labs(
#'     title="Development of Dominant height of\nNorway spruce in southern Sweden",
#'     x="Age at breast height (years)",
#'     y="Dominant Height (m)",
#'     caption="After Hägglund (1973) f. 5.1"
#'   )+
#'   theme_classic()+
#'   theme(
#'     plot.title = element_text(face="italic",family="serif"),
#'     plot.subtitle = element_text(face="italic",family="serif"),
#'     legend.position = "none",
#'     plot.margin= unit(c(10,10,10,10), "points")
#'   )+
#'   annotate("text",x=40,y=30,label="40+",size=3)+
#'   annotate("text",x=80,y=32,label="36",size=3)+
#'   annotate("text",x=97,y=30.5,label="32",size=3)+
#'   annotate("text",x=97,y=26.5,label="28",size=3)+
#'   annotate("text",x=97,y=22.5,label="24",size=3)+
#'   annotate("text",x=97,y=18.5,label="20",size=3)+
#'   annotate("text",x=97,y=14.5,label="16-",size=3)


Hagglund_1973_southern_Sweden_Height_trajectories_Spruce <- function(age_at_breast_height,
                                                                     top_height_dm,
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



  top_height_dm <- top_height_dm - 13


  subroutine_bonitering <- function(
    top_height,
    age_at_breast_height){

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

      DIF <- top_height-A2*(1-exp(-age_at_breast_height*RK))^RM2

      if(DIF<=0){
        AI2 <- AI3
      } else {
        AI1 <- AI3
      }

      T26 <- (-1/RK)*log(1-(13/A2)^(1/RM2))
      T13 <- 4.9546+0.63934*T26+0.031992*T26*T26


    }

    params <- list("A2"=A2,"RK"=RK,"RM2"=RM2, "T26"=T26,"T13"=T13)
    return(params)

  }

  params <- subroutine_bonitering(top_height = top_height_dm,
                                  age_at_breast_height = age_at_breast_height)


  if(params$A2>400){
    warning("Too high productivity, outside of the material.")

  } else if(params$A2<250){
    warning("Too low productivity, outside of the material.")

  } else if(params$A2>375 && top_height_dm>267){
    warning("Too old stand, outside of the material.")

  } else if(age_at_breast_height>90){
    warning("Too old stand, outside of the material.")
  }



  if(output=="SIH100"){
    H100 <- 13+params$A2*(1-exp((params$T13-100)*params$RK))^params$RM2
    return(H100)
  } else if(output=="Height"){
    return(13+params$A2*(1-exp(-age_2*params$RK))^params$RM2)
  } else if(output=="Equation"){
    return(print(paste0("y~13+",params$A2,"*(1-exp(-age*",params$RK,"))^",params$RM2)))
  }


}






