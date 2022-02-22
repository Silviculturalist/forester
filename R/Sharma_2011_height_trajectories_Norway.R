#' Height trajectories for Norway Spruce and Scots Pine in Norway
#' @source Sharma, R.P., Brunner, A., Eid, T., Øyen B-H. (2011) Modelling dominant
#' height growth from national forest inventory individual tree data with short
#' time series and large age errors. For. Ecol. Manage. 262:12. 2162-2175. DOI:
#' \url{https://doi.org/10.1016/j.foreco.2011.07.037}
#' @param age Age at breast height (1.3m)
#' @param age2 Age at breast height (1.3m) at output.
#' @param dominant_height Dominant height, metres.
#' @param output One of "Equation" or "Height" (default).
#' @name Sharma2011
#' @details Prediction statistics for all trees (MPE=-0.0269 m, R^2adj. = 0.9931,
#' N=36 for Spruce; MPE = 0.0007m, R^2adj.=0.9909, N = 37 for Pine)
#'
#' @return One of "Equation" or "Height".
#' @export
Sharma_2011_height_trajectory_Norway_Norway_Spruce <- function(age,age2,dominant_height,output="Height"){
  b1 <- 18.9206
  b2 <- 5175.18
  b3 <- 1.1576
  theta <- (dominant_height-1.3) - b1

  X <- 0.5*(theta + sqrt((theta^2) + 4*b2*(dominant_height-1.3)*(age^-b3)))


  if(output=="Height") return( (b1 + X)/(1 + b2 / X *(age2^(-b3))) + 1.3)
  if(output=="Equation") return(paste0("(",b1,"+",X,")/(1+",b2,"/",X,"*(age^(-",b3,"))) + 1.3"))
}

#' @rdname Sharma2011
#' @export
Sharma_2011_height_trajectory_Norway_Scots_Pine <- function(age,age2,dominant_height,output="Height"){
  b1 <- 12.8361
  b2 <- 3263.99
  b3 <- 1.1758

  theta <- (dominant_height-1.3) - b1

  X <- 0.5*(theta + sqrt((theta^2) + 4*b2*(dominant_height-1.3)*(age^-b3)))

  if(output=="Height") return( (b1 + X)/(1 + b2 / X *(age2^(-b3))) + 1.3)
  if(output=="Equation") return(paste0("(",b1,"+",X,")/(1+",b2,"/",X,"*(age^(-",b3,"))) + 1.3"))

}
