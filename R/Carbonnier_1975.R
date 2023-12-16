#' Basal area increment percentage according to compound interest
#'
#' @param total_age total age
#' @param SIH100 SIH 100 Oak, e.g. [forester::Carbonnier_1975_Sweden_height_trajectories_Oak]
#' @param silt_fraction Percent, Soil fraction composed of particles less than 0.06 mm.
#' @param BA_before_thinning Basal Area before thinning.
#' @param BA_before_thinning_understory Basal area of understory before thinning.
#' @param mean_basal_area_diameter_after_thinning Diameter corresponding to the mean basal area after thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]
#' @param mean_height_Lorey_after_thinning Basal area weighted mean height of stand, per Loreys formula, e.g. [forester::Lorey_mean_height]
#' @param stems_after_thinning Stems remaining per hectare after thinning.
#' @param removal_last_thinning_BA BA removed by thinning during the last thinning.
#' @param removal_last_thinning_BA_understory BA removed by thinning from the understory during the last thinning.
#' @param removal_second_last_thinning_BA BA removed by thinning during the second last thinning.
#' @param removal_second_last_thinning_BA_understory BA removed by thinning from the understory during the second last thinning.
#' @param removal_third_last_thinning_BA BA removed by thinning during the third last thinning.
#' @param removal_third_last_thinning_BA_understory BA removed by thinning from the understory during the third last thinning.
#'
#' @return Basal area increment percentage according to compound interest.
#' @export
Carbonnier_1975_compound_basal_area_growth_Oak <- function(
  SIH100,
  silt_fraction,
  total_age,
  BA_before_thinning,
  BA_before_thinning_understory,
  mean_basal_area_diameter_after_thinning,
  mean_height_Lorey_after_thinning,
  stems_after_thinning,
  removal_last_thinning_BA,
  removal_last_thinning_BA_understory,
  removal_second_last_thinning_BA,
  removal_second_last_thinning_BA_understory,
  removal_third_last_thinning_BA,
  removal_third_last_thinning_BA_understory

){

  return(
  -0.7804+
  +0.1728*((1000)/total_age)+
  +0.7061*((100)/mean_basal_area_diameter_after_thinning)+
  -0.1408*((mean_height_Lorey_after_thinning*sqrt(stems_after_thinning))/mean_basal_area_diameter_after_thinning)+
  +0.1841*10*sqrt(
    (((removal_last_thinning_BA+removal_last_thinning_BA_understory)/2)+
      (removal_second_last_thinning_BA+removal_second_last_thinning_BA_understory) +
      ((removal_third_last_thinning_BA+removal_third_last_thinning_BA_understory)/2))/(BA_before_thinning+BA_before_thinning_understory)
  )+
  -0.0945*((10*SIH100)/silt_fraction)
  )
}



#' Calculate the form factor for Oak stands, from Carbonnier (1975)
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 72.
#'
#' @param Lorey_mean_height Mean height of stand according to Lorey's formula, e.g. [forester::Lorey_mean_height]
#'
#' @return Form factor for stand.
#' @export
Carbonnier_1975_form_factor_Oak <- function(
  Lorey_mean_height
){
  return(
    +0.4225+
    +0.7788*(1/Lorey_mean_height)
  )
}
#' Mean diameter for initial Oak stands from Carbonnier 1975
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 18.
#'
#' @details
#' s= 0.872
#'
#'
#'
#' @param stems Stems per hectare
#' @param dominant_height_m Dominant height, m.
#'
#' @return Mean stem diameter, cm.
#' @export
Carbonnier_1975_initial_stand_mean_diameter <- function(
  stems,
  dominant_height_m
){
  return(
  2.3377+
  +0.6322*(10000/stems)+
  +0.3617*dominant_height_m
  )
}
#' Calculate the mean height in an Oak stand after a thinning.
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 72.
#'
#' @param Loreys_mean_height_before_thinning Mean height before thinning as calculated by Lorey's formula, e.g. [forester::Lorey_mean_height]
#' @param basal_area_weighted_mean_diameter_cm_before_thinning Basal area weighted mean diameter before thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]
#' @param basal_area_weighted_mean_diameter_cm_after_thinning Basal area weighted mean diameter after thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]
#'
#' @return Mean height of the Oak stand, in meters.
#' @export
Carbonnier_1975_mean_height_after_thinning_Oak <- function(
  Loreys_mean_height_before_thinning,
  basal_area_weighted_mean_diameter_cm_before_thinning,
  basal_area_weighted_mean_diameter_cm_after_thinning
){
  return(
    +1.7553*(Loreys_mean_height_before_thinning*basal_area_weighted_mean_diameter_cm_after_thinning/basal_area_weighted_mean_diameter_cm_before_thinning)+
    -0.7543*((Loreys_mean_height_before_thinning*(basal_area_weighted_mean_diameter_cm_after_thinning^2))/(basal_area_weighted_mean_diameter_cm_before_thinning^2))
  )
}
#' Calculate the mean height in an Oak stand before a thinning.
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 72.
#'
#' @param dominant_height_m Dominant height of stand, in metres.
#' @param stems Number of stems per hectare.
#'
#' @return Mean height of the Oak stand, in meters.
#' @export
Carbonnier_1975_mean_height_before_thinning_Oak <- function(
  dominant_height_m,
  stems
){
  return(
    -1.3955+
    +1.0632*dominant_height_m+
    +0.1520*(stems/100)+
    -0.1574*((stems*dominant_height_m)/1000)
  )
}
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

