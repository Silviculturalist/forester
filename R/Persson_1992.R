#' Average Basal area in stands of Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 59. pp. 206.
#'
#' @details
#' number of growth periods: 2269
#'
#' Coefficient of determination: 0.292
#'
#' Standard deviation about the function (sf): 0.282
#'
#' (sf)/standard deviation about the mean: 84.0%
#'
#' @param H100 Site Index e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param dominant_height Dominant height m.
#'
#' @return Basal area at the end of the period, m2/ha.
#' @export
Persson_1992_average_basal_area_Pine <- function(
  H100,
  dominant_height
){
  return(
    exp(
    -0.150317+ #including correction for logarithmic bias.
      +0.50463*log(dominant_height)+
      +0.62033*log(H100)
    )
  )
}
#' Basal area increment under bark at breast height for Scots Pine from Persson (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 56. pp. 206.
#'
#' @details
#'
#' Function no. 2.
#' More and less detailed functions available.
#'
#' number of growth periods: 1163
#'
#' Coefficient of determination: 0.790
#'
#' Standard deviation about the function (sf): 0.219
#'
#' (sf)/standard deviation about the mean: 45.9%
#' @param basal_area_after_thinning Basal area at the beginning of the period, after thinning, under bark, m2/ha
#' @param age_at_breast_height Age at breast height
#' @param H100 Site Index after Hägglund 1974, m.
#' @param latitude Latitude, degrees.
#'
#' @return Annual basal area increment under bark, m2/ha/yr
#' @export
Persson_1992_BA_increment_Pine <- function(
  basal_area_after_thinning,
  age_at_breast_height,
  H100,
  latitude
){

    return(
      exp(
        +4.90697+
        +0.44683*log(basal_area_after_thinning)+
        -0.63272*log(age_at_breast_height)+
        +0.30834*log(H100)+
        -1.32323*log(latitude)
      )
    )

}
#' Bark area for Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 58. pp. 206.
#'
#' @details
#' number of growth periods: 697
#'
#' Coefficient of determination: 0.896
#'
#' Standard deviation about the function (sf): 0.103
#'
#' (sf)/standard deviation about the mean: 32.4%
#'
#' @param basal_area_above_bark Basal area over bark, m2/ha
#' @param H100 Site Index, m. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param stems Number of stems per ha.
#' @param latitude Latitude, degrees.
#'
#' @return Bark Area, m2/ha.
#' @export
Persson_1992_bark_Pine <- function(
  basal_area_above_bark,
  H100,
  stems,
  latitude
){
  return(
    exp(
    8.43648+ #including correction for logarithmic bias.
      +0.94902*log(basal_area_above_bark)+
      -0.176223*log(H100)+
      +0.037108*log(stems)+
      -2.30456*log(latitude)
    )
  )
}
#' Diameter ratio of the mortality stems in stands of Scots Pine from Persson (1992)
#'
#'@source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 61. pp. 206.
#'
#' @details
#' number of growth periods: 1134
#'
#' Coefficient of determination: 0.188
#'
#' Standard deviation about the function (sf): 0.309
#'
#' (sf)/standard deviation about the mean: 90.2 \%
#'
#' @param dominant_height Dominant height, m.
#' @param stems Stems per hectare
#' @param thinned TRUE / FALSE if the thinning removal at the start of the period is at least 10\% of the basal area, 1, otherwise 0.
#'
#' @return Ratio between the diameter of self-thinned stems and the diameter of the remaining stems.
#' @export

Persson_1992_mortality_diameter_quotient_Pine <- function(
  dominant_height,
  stems,
  thinned
){
  return(
    exp(
      -1.06123+
      +0.38727*log(dominant_height)+
      -0.080630*log(stems)+
      +0.140766*thinned
    )
  )
}
#' Natural mortality in stands of Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 60. pp. 206.
#'
#' @details
#' If basal_area_above_bark denser than average basal area cf. [forester::Persson_1992_average_basal_area_Pine()]
#'
#' number of growth periods: 301
#'
#' Coefficient of determination: 0.371
#'
#' Standard deviation about the function (sf): 1.012
#'
#' (sf)/standard deviation about the mean: 79.7%
#'
#' If basal_area_above_bark thinner than average basal area cf. [forester::Persson_1992_average_basal_area_Pine()]
#'
#' number of growth periods: 1968
#'
#' Coefficient of determination: 0.173
#'
#' Standard deviation about the function (sf): 0.981
#'
#' (sf)/standard deviation about the mean: 91.0%
#'
#'
#' @param basal_area_above_bark Basal area over bark, m2/ha
#' @param dominant_height Dominant height m
#' @param H100 Site Index, m. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param stems Number of stems per ha.
#' @param latitude Latitude, degrees.
#'
#' @return Annual mortality, basal area on bark + 0.01 m2/ha/yr.
#' @export
Persson_1992_natural_mortality_Pine <- function(
  basal_area_above_bark,
  dominant_height,
  H100,
  stems,
  latitude
){

  average_BA_1_3 <- Persson_1992_average_basal_area_Pine(dominant_height= dominant_height,
                                                         H100 = H100)*1.3

  ifelse(G>=average_BA_1_3,
         return(
           exp(
             -17.24908+ #including correction for logarithmic bias.
               +1.86053*log(basal_area_above_bark)+
               +1.75182*log(dominant_height)+
               +0.44001*log(stems)
           )
         ),
         return(
           exp(
             +15.3390+
             +1.01872*log(dominant_height)+
             +0.69317*log(stems)+
             -6.38718*log(latitude)
           )
         )



         )



}
#' Volume for Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 58. pp. 206.
#'
#' @details
#' number of growth periods: 697
#'
#' Coefficient of determination: 0.9933
#'
#' Standard deviation about the function (sf): 0.0327
#'
#' (sf)/standard deviation about the mean: 8.2%
#'
#' @param basal_area_above_bark Basal area over bark, m2/ha
#' @param dominant_height Dominant height, metres.
#' @param stems Number of stems per ha.
#' @param latitude Latitude, degrees.
#'
#' @return Volume on bark cu. m. / ha.
#' @export

Persson_1992_volume_Pine <- function(
  basal_area_above_bark,
  dominant_height,
  stems,
  latitude
){
  return(
    exp(
    -0.58147+ #including correction for logarithmic bias.
      +1.11493*log(basal_area_above_bark)+
      +0.73376*log(dominant_height)+
      -0.072569*log(stems)+
      +0.160919*log(latitude)
    )
  )
}
