#' Expected volume in fully stocked Pine stands according to Jonsson (1914)
#'
#' @source Jonsson, T. (1914) Om boniteringen av skogsmark, eng: On the
#' estimation of the productivity of forest land. Skogsvårdsföreningens
#' tidsskrift, nr. 5, p. 379.
#'
#' @description Author recommends Picea abies only be used as species as
#' certain support with expressed reservation.
#'
#' @param stand_mean_height metres.
#' @param species "Pinus sylvestris" or "Picea abies".
#'
#' @return Stand volume, cubic metres.
#' @export
#'
#' @examples
jonsson_stand_volume_1914 <- function(stand_mean_height,species="Pinus
                                      sylvestris"){

  if(species=="Pinus sylvestris"){

  return(
    6*stand_mean_height^(1 + (1/3))
  )

  } else if(species=="Picea abies"){

    return(
      4.2*stand_mean_height^(1.5)
    )

  }

}

