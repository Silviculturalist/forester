#' Site Index 100 Estimations for Sweden
#' @description Provides interface for the site index functions from "\emph{Nya höjdutvecklingskurvor för bonitering}" (2013),
#' by Johansson, U., Ekö, P-M, Elfving, B., Johansson, T. and Nilsson, U.
#'
#'     \strong{Reference ages by species}: Picea abies (100);Pinus sylvestris (100); Larix (100); Fagus sylvatica (100); Quercus (100); Betula (50); Alnus (50); Populus (50).
#'
#'     \strong{Suitable ages for use by species}: Picea abies (10-80); Pinus sylvestris (10-80); Larix (10-100); Fagus sylvatica (20-150); Quercus (20-150);  Betula (10-90); Alnus glutinosa (<100); Alnus incana (<70); Populus tremula x tremuloides (<50); Populus tremula (<50-60)
#' @param data Tidy data frame.
#' @param species One of "NorwaySpruce", "ScotsPine", "Larch", "Beech","Oak","Birch","AlnusIncana", "AlnusGlutinosa", "Aspen".
#' @param age Average total age in years. For birch age at breast height.
#' @param height Average Height in metres of dominant trees.
#' @param outputcolumn Name of column to create for output.
#' @param ...
#'
#' @return data frame with an additional column with the estimate of the height of the tree at age 100.
#' @export
#'
#' @examples
#' SWE_si_100_estimate(df=df, species="NorwaySpruce",age=30, height=6.2, outputcolumn="SI_estimate")
SWE_si_100_estimate <- function(data,species=c("NorwaySpruce","ScotsPine","Larch","Beech","Oak","Birch","AlnusIncana","AlnusGlutinosa","Aspen"),
                           age, height, outputcolumn){

    #Set top_height to NA if input not numeric.
  if(!is.numeric(data[[age]])) {
   data[[outputcolumn]] <- 999
    return(data)
  }
  if (!is.numeric(data[[height]])) {
    data[[outputcolumn]] <- 999
    return(data)
  }
    #Set params

  if (species=="NorwaySpruce") {
    paramasi <- 10
    parambeta <- 1495.3
    paramb2 <- -1.5978
    refAge <- 100-3
  } else if(species=="ScotsPine"){
      paramasi <- 25
      parambeta <- 7395.6
      paramb2 <- -1.7829
      refAge <- 100
    } else if(species=="Larch"){
      paramasi <- 17.97
        parambeta <- 1529
        paramb2 <- -1.3451
        refAge <- 100
    } else if(species=="Beech"){
        paramasi <- 15
          parambeta <- 4239.3
          paramb2 <- -1.7753
          refAge <- 100
    } else if(species=="Oak"){
        paramasi <- 1000
          parambeta <- 8841.4
          paramb2 <- -1.4317
          refAge <- 100
    } else if(species=="Birch"){
        paramasi <- 7
          parambeta <- 394
          paramb2 <- -1.387
          refAge <- 50
    } else if(species=="AlnusIncana"){
        paramasi <- 7
          parambeta <- 278.9
          paramb2 <- -1.3152
          refAge <- 50
    } else if(species=="AlnusGlutinosa"){
        paramasi <- 7
          parambeta <- 381.5
          paramb2 <- -1.3823
          refAge <- 50
    } else if(species=="Aspen"){
        paramasi <- 7
          parambeta <- 693.2
          paramb2 <- -0.9771
          refAge <- 50
    }

  #Define general function
  d <- parambeta*(paramasi^paramb2)

  if(species!="NorwaySpruce"){
  r <- (((data[[height]]-d)^2)+(4*parambeta*data[[height]]*(data[[age]]^paramb2)))^0.5
  } else if (species == "NorwaySpruce"){
  r <- (((data[[height]]-d)^2)+(4*parambeta*data[[height]]*((data[[age]]-3)^paramb2)))^0.5
  }

  data[[outputcolumn]] <- (data[[height]]+d+r)/ (2+(4*parambeta*(refAge^paramb2)) / (data[[height]]-d+r))

  return(data)
}

