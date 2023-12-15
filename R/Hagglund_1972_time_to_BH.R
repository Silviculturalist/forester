#' Time to breast height for Spruce in Northern Sweden.
#' @param H100TotalAge Site Index H100 from Hagglund 1972.
#' @param culture 1 if culture, otherwise 0.
#' @param latitude degrees North.
#'@seealso [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()]
#'@export
#'@examples
#'ggplot()+xlim(c(0,200))+geom_function(fun=function(x) Hagglund_1972_northern_Sweden_Height_trajectories_Spruce(16,age=100-Hagglund_1972_Norway_Spruce_time_to_BH_northern_Sweden(16,1,61),age2=x,61,1))+geom_point(aes(x=seq(10,200,10),y=c(27,46,66,86,104,121,137,151,164,175,185,194,203,210,216,222,227,231,235,239)/10))
Hagglund_1972_Norway_Spruce_time_to_BH_northern_Sweden <- function(H100TotalAge,culture=1,latitude){

  P <- 0.9175^culture

  age = 100

  top_height_dm <- H100TotalAge*10

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

  return(
    T13
  )
}

#' Time to breast height for Spruce in Southern Sweden.
#' @param H100TotalAge Site Index H100 from Hagglund 1973.
#'@export
#'@seealso [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#'@examples
#'ggplot()+xlim(c(0,100))+geom_function(fun=function(x) Hagglund_1973_southern_Sweden_Height_trajectories_Spruce(28,age=100-Hagglund_1973_Norway_Spruce_time_to_BH_southern_Sweden(28),age2=x))+geom_point(aes(x=seq(10,100,10),y=c(55,100,140,175,204,228,248,264,277,288)/10))+xlab("Age at breast height")+ylab("Dominant Height, m")
Hagglund_1973_Norway_Spruce_time_to_BH_southern_Sweden <- function(H100TotalAge){
  top_height_dm <- H100TotalAge*10
  top_height_dm <- top_height_dm - 13
  age = 100



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


      return(
        params$T13
      )
}
