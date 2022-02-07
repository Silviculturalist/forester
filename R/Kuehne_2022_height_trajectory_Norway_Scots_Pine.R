#' Height trajectory for Scots Pine in Norway from Kuehne et al 2022.
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C.,
#'  Astrup R. (2022). A stand-level growth and yield model for thinned and
#'  unthinned even-aged Scots pine forests in Norway. Silva Fennica vol. 56
#'   no. 1 article id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: 0.01458, Mean absolute error: 0.34876, relative MAE: 2.18346
#'
#' @details Compared to Tveite (1976) [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#' and Sharma (2011), estimates a higher Site Index.
#' @param age Total stand age
#' @param age2 Total stand age at output.
#' @param dominant_height Dominant height, metres.
#' @param output One of "SIH100","Equation" or "Height" (default).
#'
#' @return One of "SIH100","Equation" or "Height".
#' @export
Kuehne_2022_height_trajectory_Norway_Scots_Pine <- function(
  age,
  age2,
  dominant_height,
  output="Height"
){

  b1 <- 68.41819
  b2 <- -24.04110
  b3 <- 1.46991

  X <- (dominant_height - b1) / (1 - b2*dominant_height*(age^-b3))


  if(output=="Height"){
    return(
      (b1 + X)/(1+b2*X*(age2^-b3))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~ (",b1," + ",X,")/(1+",b2,"*",X,"*(age^-",b3,"))")
    )
  }

  if(output=="SIH100"){
    return(
      (b1 + X)/(1+b2*X*(100^-b3))
    )
  }

}

Kuehne_2022_height_trajectory_Norway_Scots_Pine <-  Vectorize(Kuehne_2022_height_trajectory_Norway_Scots_Pine)
