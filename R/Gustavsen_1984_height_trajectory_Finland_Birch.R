#' Dominant height trajectory for naturally regenerated Birch in Finland
#'
#' @source Gustavsen, H.G., Mielikäinen, K. (1984). Luontaisesti syntyneiden
#' koivikoiden kasvupaikka-luokittelu valtapituuden avulla. (Site Index Curves
#' for Natural Birch Stands in Finland). Folia For. 597: 1-20.
#' Available (26/03/2022): \url{https://jukuri.luke.fi/handle/10024/522180}
#'
#' @description 'For natural Betula pendula Roth and Betula pubescens Ehrh.
#' stands on mineral soils in Finland...using 341 observation
#' periods on permanent plots and stemanalyses of dominant trees on temporary
#' plots... the site index classes H50=12-18 will mainly serve the classification
#' of Betula pubescens stands under practical conditions. The upper classes of
#' the system, H50 = 20-26, will mainly be site index classes for Betula pendula
#' stands...
#' \strong{The site index curves and yield values calculated for natural birch
#' stands show that the results cannot be used for the classifications of
#' plantations of Betula pendula.}'
#'
#' \strong{NB Increment is for next year. Therefore to find height at a lower
#' age than at outset, function optimises to find height at output age which can
#' be incremented to the outset height, age pair.}
#'
#' @details ln(increment Hdom): N = 341. R^2= 0.578. s_m= 0.647, s_f = 0.422.
#'
#'
#'
#' @param dominant_height Dominant height, in metres.
#' @param age Total age
#' @param age2 Total age at output
#'
#' @return Dominant height at age2.
#' @export
Gustavsen_1984_height_trajectory_Finland_Birch <- function(
  dominant_height,
  age,
  age2
){
  if(age<=age2){
    while(age<age2){
      ln_increment <-  0.50766 + -1.34612*log(age) + 1.61986*log(dominant_height) - 0.07157*dominant_height
      dominant_height <-  dominant_height + exp(ln_increment)
      age <-  age+1
    }

    return(dominant_height)
  }

  #Cannot increment backwards. Optimise to find best match to increment forwards.
  birch_incrementer <- function(start_height,start_age,end_height,end_age){
    while(start_age<end_age){
    ln_increment <-  0.50766 + -1.34612*log(start_age) + 1.61986*log(start_height) - 0.07157*start_height
    start_height <-  start_height + exp(ln_increment)
    start_age <-  start_age+1
    }
    return(
      abs(start_height-end_height)
    )
  }

  return(
    optimise(f = birch_incrementer,
                              interval = c(0,30),
                              start_age=age2, #observe opposite order.
                              end_age=age,
                              end_height=dominant_height)[[1]]
  )

}


data.frame(H50 = c(26,24,22,20,18,16,14,12),t1.3=c(4,4,5,5,6,6,7,8)) %>% mutate(predicted=exp(predict(lm(formula= log(t1.3)~H50)))) %>% ggplot()+geom_point(aes(x=H50,y=t1.3))+geom_line(aes(x=H50,y=predicted))

#' Smoothing of time required to reach breast height according to Gustavsen 1984.
#' @details log-linear function log(t1.3)~H50.
#' @param SI50 Site index 50, according to Gustavsen 1984.
#' @return Number of years required to reach breast height 1.3m.
#' @seealso [forester::Gustavsen_1984_height_trajectory_Finland_Birch()]
#' @export
#' @author Carl Vigren SLU
Gustavsen_1984_time_to_breast_height <- function(SI50){
  return(exp(2.647712 - 0.049877*SI50))
}



