#' Function for translating SI H100 for Pine to Birch according to Hägglund (1974) from
#' Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 79.
#'
#' @details Material:
#'
#' Number of plots: 55
#'
#' R: 0.62
#'
#' Standard deviation about the function (sf) : 32.9
#'
#' sf/standard deviation about the mean:  80\%
#'
#'
#' @param SI_Pine SI H100 Pine, meters.
#'
#' @return SI H100 Birch, meters.
#' @export
Agestam_1985_SI_translation_Pine_to_Birch <- function(
  SI_Pine
){
  return(
    ((0.736*(SI_Pine*10))-21.1)/10
  )
}
