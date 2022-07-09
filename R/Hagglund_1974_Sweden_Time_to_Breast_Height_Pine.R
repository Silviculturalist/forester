#' Time to breast height for Scots Pine in Sweden.
#'@seealso [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'@export
Hagglund_1974_Sweden_Time_to_Breast_Height_Pine <- function(H100TotalAge,
                                                            regeneration = "culture"
){

  if(!(regeneration%in%c("culture","natural","unknown"))){
    stop("Argument regeneration must be one of 'culture','natural','unknown'.")
  }

  age = 100

  top_height_dm <- H100TotalAge*10

  top_height_dm <- top_height_dm - 13

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

  return(
    params$T13
  )

}


