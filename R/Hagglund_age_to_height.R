#' Age at a certain height according to height trajectories by Hägglund (1972,1973,1974)
#'
#' @param description Wrapper to find age at breast height, total age and age to
#' breast height at a set SI and/or height for Hagglunds functions.
#'
#' @param latitude Degrees N. Only required for "Picea abies" if locality is "northern".
#' @param SI Site Index H100
#' @param dominant_height_m Given height if interested in height above breast height. Defaults to 12.5 m.
#' @param species "Picea abies" or "Pinus sylvestris".
#' @param locality For "Picea abies": "northern" or "southern" Sweden.
#' @param regeneration How was the stand established? One of "culture", "natural regeneration" or "unknown".
#'
#' @return List of 3 elements:
#'
#' "Age at breast height given dominant height", numeric.
#'
#' "Time to breast height 1.3 m", numeric.
#'
#' "Total age", numeric.
#'
#'@seealso [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()] ,
#'
#'
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()],
#'
#' [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#'
#' @export
#' @md
#'
#' @examples
Hagglund_age_to_height <-  function(
  latitude,
  SI,
  dominant_height_m=12.5,
  species,
  locality,
  regeneration="culture"
){

  if(dominant_height_m<=1.3){
    stop("dominant_height_m must be strictly above 1.3m.")
  }

  if(species=="Pinus sylvestris"){

    g <- function(x) forester::Hagglund_1974_Sweden_height_trajectories_Pine(age_at_breast_height = x,
                                                                             top_height_dm = dominant_height_m*10,
                                                                             regeneration = regeneration,
                                                                             output = "SIH100") - (SI*10)
    root <- uniroot(g, c(0,150))[[1]] #The answer is between the ages of...

    age_to_breast_height <- forester::Hagglund_1974_Sweden_height_trajectories_Pine(age_at_breast_height = root, #Keep all the decimal places
                                                                                    top_height_dm = (SI*10),
                                                                                    regeneration = regeneration,
                                                                                    output="Equation")[[2]]



    agelist <- list(
      "Age at breast height at given dominant height"= root,
      "Time to breast height 1.3m"= age_to_breast_height,
      "Total age"= root + age_to_breast_height
    )

    #Insert point to get equation
    return(
      agelist

    )



  } else if(species=="Picea abies"){

    if(locality=="northern"){

      culture <- if(regeneration=="culture"){
        1
      } else {
        0
      }


      g <- function(x) forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce(
        latitude=latitude,
        age_at_breast_height = x,
        top_height_dm = dominant_height_m*10,
        culture = culture,
        output = "SIH100") - (SI*10)
      root <- uniroot(g, c(0,150))[[1]] #The answer is between the ages of...

      age_to_breast_height <- forester::Hagglund_1972_northern_Sweden_height_trajectories_Spruce(latitude=latitude,
                                                                                                 age_at_breast_height = root, #Keep all the decimal places
                                                                                                 top_height_dm = (SI*10),
                                                                                                 culture=culture,
                                                                                                 output="Equation")[[2]]



      agelist <- list(
        "Age at breast height at given dominant height"= root,
        "Time to breast height 1.3m"= age_to_breast_height,
        "Total age"= root + age_to_breast_height
      )

      #Insert point to get equation
      return(
        agelist
      )







    } else if(locality=="southern"){

      g <- function(x) forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce(
        age_at_breast_height = x,
        top_height_dm = dominant_height_m*10,
        output = "SIH100") - (SI*10)
      root <- uniroot(g, c(0,150))[[1]] #The answer is between the ages of...

      #Insert point to get equation

      age_to_breast_height <- forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce(
        age_at_breast_height = root, #Keep all the decimal places
        top_height_dm = (SI*10),
        output="Equation")[[2]]



      agelist <- list(
        "Age at breast height at given dominant height"= root,
        "Time to breast height 1.3m"= age_to_breast_height,
        "Total age"= root + age_to_breast_height
      )


      return(
        agelist

      )




    }

  }


}
