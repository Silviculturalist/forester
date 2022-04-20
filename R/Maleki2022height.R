#' Height trajectories for main species groups in Norway.
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param dominant_height Dominant height, in metres, at age.
#' @param age Total age, years.
#' @param age2 Desired total age, years.
#' @param output "Height" (default) or "Equation"
#'
#' @return Height of the stand in metres at the desired age.
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 0.004 \tab 0.590 \tab 0.821 \tab 0.963 \cr
#' Scots Pine \tab 0.0045 \tab 0.441 \tab 0.644 \tab 0.969 \cr
#' Broadleaves \tab 0.002 \tab 0.668 \tab 0.933 \tab 0.941 \cr
#' }
#'
#' @family {Maleki2022}
#' @name Maleki2022height
#' @export
Maleki_2022_height_trajectory_Norway_Norway_Spruce <- function(dominant_height,age,age2,output="Height"){
  b1 <- 39.5764
  b2 <- -396.3146
  b3 <- 1.6770

  if(output=="Height"){
  return(
    (b1 + ((dominant_height - b1)/(1-(b2*dominant_height*age^-b3))))/(1+b2*((dominant_height-b1)/(1-(b2*dominant_height*age^-b3)))*age2^-b3)
  )
  }

  if(output=="Equation"){
    return(
      paste0(
        "(",b1," + ((",dominant_height," - ",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,"))))/(1+",b2,"*((",dominant_height,"-",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,")))*age2^",-b3
        )
    )
  }
}

#' @export
#' @rdname Maleki2022height
Maleki_2022_height_trajectory_Norway_Scots_Pine <- function(dominant_height,age,age2,output="Height"){
  b1 <- 43.6698
  b2 <- -24.9476
  b3 <- 1.2967

  if(output=="Height"){
  return(
    (b1 + ((dominant_height - b1)/(1-(b2*dominant_height*age^-b3))))/(1+b2*((dominant_height-b1)/(1-(b2*dominant_height*age^-b3)))*age2^-b3)
  )
  }

  if(output=="Equation"){
    return(
      paste0(
        "(",b1," + ((",dominant_height," - ",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,"))))/(1+",b2,"*((",dominant_height,"-",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,")))*age2^",-b3
      )
    )
  }
}

#' @export
#' @rdname Maleki2022height
Maleki_2022_height_trajectory_Norway_Broadleaves <- function(dominant_height,age,age2,output="Height"){
  b1 <- 36.6501
  b2 <- -11.8787
  b3 <- 1.0643

  if(output=="Height"){
  return(
    (b1 + ((dominant_height - b1)/(1-(b2*dominant_height*age^-b3))))/(1+b2*((dominant_height-b1)/(1-(b2*dominant_height*age^-b3)))*age2^-b3)
  )
  }

  if(output=="Equation"){
    return(
      paste0(
        "(",b1," + ((",dominant_height," - ",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,"))))/(1+",b2,"*((",dominant_height,"-",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,")))*age2^",-b3
      )
    )
  }
}
