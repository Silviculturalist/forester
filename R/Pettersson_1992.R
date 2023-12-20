#' Basal area weighted mean diameter after PCT in young stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @details
#'
#'  R^2 = 0.95
#'  F=1563
#'
#' @param dominant_height Dominant height, m.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean basal area stem, cm.
#'
#'
#' @return Basal area weighted mean diameter, cm after thinning
#' @export
Pettersson_1992_BA_weighted_mean_diameter_after_PCT_Pine <- function(
  dominant_height,
  diameter_mean_basal_area_stem
){

  return(
    exp(
      0.296+
        +0.140*log(dominant_height)+
        +0.777*log(diameter_mean_basal_area_stem)
    )

  )


}
#' Basal area weighted mean diameter after PCT in stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=471
#' R^2 = 0.91
#'
#' @param dominant_height Dominant height, metres.
#' @param diameter_mean_basal_area_stem Diameter corresponding the mean basal area stem, cm.
#'
#' @return Basal area weighted mean diameter
#' @export
Pettersson_1992_BA_weighted_mean_diameter_after_PCT_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem
){
  return(
    exp(
      0.194 + 0.129*log(dominant_height) + 0.824*log(diameter_mean_basal_area_stem)
    )
  )
}
#' Form quotient after PCT in young stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @details
#'
#'  R^2 = 0.82
#'  F=882
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#' @param latitude Latitude, degrees.
#'
#'
#' @return Living stems per hectare after thinning
#' @export
Pettersson_1992_form_quotient_after_PCT_Pine <- function(
  stems,
  dominant_height,
  latitude
){

  a <- ifelse(latitude>60,-0.460,-0.516)

  return(
    exp(
      a+
        +0.857*log(dominant_height)+
        +0.004*log(stems)
    )

  )


}
#' Form quotient after PCT in stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=362
#' R^2 = 0.81
#'
#' @param dominant_height Dominant height, metres.
#' @param stems Stems per hectare
#'
#' @return Form quotient Spruce after PCT
#' @export
Pettersson_1992_form_quotient_after_PCT_Spruce <- function(
  dominant_height,
  stems
){
  return(
    exp(
      0.830 + 0.737*log(dominant_height) -0.113*log(stems)
    )
  )
}
#' Living stems after PCT in young stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @details
#'
#'  R^2 = 0.98
#'  F=12879
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#'
#'
#' @return Living stems per hectare after thinning
#' @export
Pettersson_1992_living_stems_after_PCT_Pine <- function(
  stems,
  dominant_height
){

  return(
    exp(
      0.460+
        -0.050*log(dominant_height)+
        +0.953*log(stems)
    )

  )


}
#' Living stems after PCT in stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=37284
#' R^2 = 0.99
#'
#' @param dominant_height Dominant height, metres.
#' @param stems Stems per hectare
#'
#' @return Number of living stems per hectare after PCT
#' @export
Pettersson_1992_living_stems_after_PCT_Spruce <- function(
  dominant_height,
  stems
){
  return(
    exp(
      0.168 - 0.038*log(dominant_height) + 0.989*log(stems)
    )
  )
}
#' Mean diameter of the thickest trees per hectare after PCT in stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#'
#' 800 stems
#'
#' F= 505
#' R^2 = 0.86
#'
#' 400 stems
#' F=557
#' R^2 = 0.86
#'
#'  100 stems
#'  F= 353
#'  R^2 = 0.80
#'
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area, in cm.
#' @param thickest_x_trees One of '800', '400' or '100'.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_mean_diameter_x_trees_after_PCT_Pine <- function(
  dominant_height,
  diameter_mean_basal_area_stem,
  thickest_x_trees
){

  if(!(thickest_x_trees%in%c(100,400,800))){
         stop("The argument 'thickest_x_trees' must be one of 100, 400 or 800.")
  }

  ifelse(thickest_x_trees == 800,
         return(
           exp(0.751 + 0.281 * log(dominant_height) + 0.502 * log(diameter_mean_basal_area_stem))
         ),
         ifelse(thickest_x_trees == 400,
                return(
                  exp(0.756 + 0.339 * log(diameter_mean_basal_area_stem) + 0.472 * log(dominant_height))
                ),
                return(
                  exp(0.858 + 0.351 * log(diameter_mean_basal_area_stem) + 0.459 * log(dominant_height))
                )))



}
#' Mean diameter of the thickest trees per hectare after PCT in stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#'
#' 800 stems
#'
#' F= 181
#' R^2 = 0.83
#'
#' 400 stems
#' F=213
#' R^2 = 0.82
#'
#'  100 stems
#'  F= 149
#'  R^2 = 0.76
#'
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area, in cm.
#' @param thickest_x_trees One of '800', '400' or '100'.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_mean_diameter_x_trees_after_PCT_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem,
  thickest_x_trees
){

  if(!(thickest_x_trees%in%c(100,400,800))){
    stop("The argument 'thickest_x_trees' must be one of 100, 400 or 800.")
  }

  ifelse(thickest_x_trees == 800,
         return(
           exp(0.676 + 0.352 * log(dominant_height) + 0.452 * log(diameter_mean_basal_area_stem))
         ),
         ifelse(thickest_x_trees == 400,
                return(
                  exp(0.591 + 0.387 * log(diameter_mean_basal_area_stem) + 0.482 * log(dominant_height))
                ),
                return(
                  exp(0.605 + 0.441 * log(diameter_mean_basal_area_stem) + 0.463 * log(dominant_height))
                )))



}
#' Mean diameter of the thickest trees per hectare in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 27.
#'
#' @details
#'
#' function 19. Mean diameter of thickest 800 trees per hectare.
#'
#' No. of observations = 86
#' R^2 = 0.91
#'
#'
#' function 20. Mean diameter of thickest 400 trees per hectare.
#'
#' No. of observations = 86
#' R^2 =  0.92
#'
#'
#' function 21. Mean diameter of thickest 100 trees per hectare.
#'
#' No. of observations = 86
#' R^2 = 0.87
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area.
#' @param thickest_x_trees One of '800', '400' or '100'.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_young_stands_mean_diameter_x_trees_Pine <- function(
  dominant_height,
  diameter_mean_basal_area_stem,
  thickest_x_trees
){

  if(!(thickest_x_trees%in%c(100,400,800))){
         stop("The argument 'thickest_x_trees' must be one of 100, 400 or 800.")
  }

  ifelse(thickest_x_trees == 800,
         return(
           0.8596 + 0.3010 * log(dominant_height) + 0.4435 * log(diameter_mean_basal_area_stem)
         ),
         ifelse(thickest_x_trees == 400,
                return(
                  0.9216 + 0.4888 * log(diameter_mean_basal_area_stem) + 0.2684 * log(dominant_height)
                ),
                return(
                  1.0489 + 0.5018 * log(diameter_mean_basal_area_stem) + 0.2421 * log(dominant_height)
                )))



}
#' Lorey's mean height after PCT in stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=1530
#' R^2 = 0.94
#'
#' @param dominant_height Dominant height, metres.
#' @param stems Stems per hectare
#'
#' @return Lorey's Mean Height
#' @export
Pettersson_1992_mean_height_Lorey_after_PCT_Pine <- function(
  dominant_height,
  stems
){
  return(
    exp(
      0.109 + 1.009*log(dominant_height) - 0.028*log(stems)
    )
  )
}
#' Lorey's mean height after PCT in stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=239
#' R^2 = 0.84
#'
#' @param dominant_height Dominant height, metres.
#' @param stems Stems per hectare
#'
#' @return Lorey's Mean Height
#' @export
Pettersson_1992_mean_height_Lorey_after_PCT_Spruce <- function(
  dominant_height,
  stems
){
  return(
    exp(
      0.759 + 0.832*log(dominant_height) - 0.054*log(stems)
    )
  )
}
#' Total basal area production before first thinning in stands of Scots Pine, from Pettersson 1992.
#'
#' @description "The mean losses of basal area were small, averaging 1.7 \% for Spruce
#' and 2.5 \% for Pine up to first thinning. The first thinning was carried out at
#' an average dominant height of 12.5 m for both spruce and pine.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'
#' @param basal_area_before_first_thinning Living basal area before first thinning m2 /ha.
#'
#' @return Total basal area production before first thinning m2 /ha.
#' @export
Pettersson_1992_total_BA_production_before_first_thinning_Pine <- function(
  basal_area_before_first_thinning
){
  return(
    exp(
      0.063 + 0.973*log(basal_area_before_first_thinning)
    )
  )
}
#' Total basal area production before first thinning in stands of Norway Spruce, from Pettersson 1992.
#'
#' @description "The mean losses of basal area were small, averaging 1.7 \% for Spruce
#' and 2.5 \% for Pine up to first thinning. The first thinning was carried out at
#' an average dominant height of 12.5 m for both spruce and pine.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'
#' @param basal_area_before_first_thinning Living basal area before first thinning m2 /ha.
#'
#' @return Total basal area production before first thinning m2 /ha.
#' @export
Pettersson_1992_total_BA_production_before_first_thinning_Spruce <- function(
  basal_area_before_first_thinning
){
  return(
    exp(
      0.044 + 0.980*log(basal_area_before_first_thinning)
    )
  )
}
#' Total volume yield after PCT in young stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 7.
#'
#'  @details R^2 = 0.97, for both classes.
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#' @param SI Site Index, according to Hägglund 1974 [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'
#' @return Total volume yield m3/ha.
#' @export
Pettersson_1992_total_volume_yield_after_PCT_Pine <- function(
  stems,
  dominant_height,
  SI
){

  a <- ifelse(SI>25,3422,83.1)
  b <- ifelse(SI>25,-3.130,-1.310)
  c <- ifelse(SI>25,0.445,0.918)
  d <- ifelse(SI>25,-1.765,-2.036)

  return(
    (stems/(a*(dominant_height^b)+stems*c*(dominant_height^d)))

  )


}
#' Total volume yield after PCT in young stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 7.
#'
#'  @details R^2 = 0.98 if SI<= 30, else R^2 = 0.96
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#' @param SI Site Index, according to Hägglund 1972,1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce] [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Total volume yield m3/ha.
#' @export

Pettersson_1992_total_volume_yield_after_PCT_Spruce <- function(
  stems,
  dominant_height,
  SI
){

  a <- ifelse(SI>30,1000,1000)
  b <- ifelse(SI>30,-2.488,-2.331)
  c <- ifelse(SI>30,1.066,0.259)
  d <- ifelse(SI>30,-2.098,-1.537)

  return(
    (stems/(a*(dominant_height^b)+stems*c*(dominant_height^d)))

  )


}
#' Mean of Volume distribution for stands after PCT of Scots Pine from Pettersson 1992
#'
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @description
#'
#' "A Gram-Charlier-series (Kendall et al. 1987), was used by Eriksson (1976) for
#'  the description of the frequency of stems and volume by diameter classes and by
#'  Pettersson (1992) for the description of the volume distribution by diameter classes.
#'  For the application on the distributions in my material the function has the following form:
#'
#' \deqn{
#' y = \frac{V}{\sigma} [ \varphi (\frac{x-m}{\sigma}) - \frac{\lambda_1}{6}\varphi^3(\frac{x-m}{\sigma})+\frac{\lambda_2}{24}\varphi^4(\frac{x-m}{\sigma})+\frac{\lambda_1^2}{720}\varphi^6(\frac{x-m}{\sigma}  ]
#' }
#'
#' where \eqn{\varphi} = the normal frequency distribution, \eqn{\varphi^{v}}= the v:th derivate of \eqn{\varphi}, \eqn{\lambda_{1}} = the skewness of the distribution, \eqn{\lambda_{2}}=the kurtosis of the distribution, V= total volume in the distribution, y=frequency of volume for the actual class, x= middle of the class, m=mean of the distribution, and \eqn{\sigma} = standard deviation of the distribution."
#'
#'
#'
#'  @details
#'
#'  Mean :
#'  F= 3106
#'  R^2 = 0.95
#'
#'  Standard deviation:
#'  F=37
#'  R^2 =  0.29
#'
#'  Skew + 3
#'  F= 122
#'  R^2 = 0.40
#'
#'  Kurtosis + 3
#'  F = 14
#'  R^2  = 0.13
#'
#' @param diameter_mean_basal_area_stem Diameter corresponding the mean basal area stem, cm.
#' @param stems Stems per hectare
#' @param dominant_height Dominant height, metres.
#'
#' @return Volume distribution for stands of Scots Pine after PCT
#' @export
Pettersson_1992_volume_distribution_after_PCT_Pine <- function(
  diameter_mean_basal_area_stem,
  stems,
  dominant_height
){

  mean <- exp(0.517 + 0.836*log(diameter_mean_basal_area_stem))
  standard_deviation <- exp(-1.346 + 0.707*log(dominant_height)+0.073*log(stems))
  skew_plus_3 <- exp(-1.071 + 0.264*log(stems))
  kurtosis_plus_3 <- exp(3.154 -0.492*log(dominant_height) - 0.105*log(stems))

  return(
    list(
      "Mean Volume"=mean,
      "Volume Std. Deviation"=standard_deviation,
      "Volume skew + 3"=skew_plus_3,
      "Volume kurtosis + 3"=kurtosis_plus_3
    )
  )

}
#' Mean of Volume distribution for stands after PCT of Norway Spruce from Pettersson 1992
#'
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @description
#'
#' "A Gram-Charlier-series (Kendall et al. 1987), was used by Eriksson (1976) for
#'  the description of the frequency of stems and volume by diameter classes and by
#'  Pettersson (1992) for the description of the volume distribution by diameter classes.
#'  For the application on the distributions in my material the function has the following form:
#'
#' \deqn{
#' y = \frac{V}{\sigma} [ \varphi (\frac{x-m}{\sigma}) - \frac{\lambda_1}{6}\varphi^3(\frac{x-m}{\sigma})+\frac{\lambda_2}{24}\varphi^4(\frac{x-m}{\sigma})+\frac{\lambda_1^2}{720}\varphi^6(\frac{x-m}{\sigma}  ]
#' }
#'
#' where \eqn{\varphi} = the normal frequency distribution, \eqn{\varphi^{v}}= the v:th derivate of \eqn{\varphi}, \eqn{\lambda_{1}} = the skewness of the distribution, \eqn{\lambda_{2}}=the kurtosis of the distribution, V= total volume in the distribution, y=frequency of volume for the actual class, x= middle of the class, m=mean of the distribution, and \eqn{\sigma} = standard deviation of the distribution."
#'
#'
#'  @details
#'
#'  Mean :
#'  F= 1426
#'  R^2 = 0.94
#'
#'  Standard deviation:
#'  F=45
#'  R^2 =  0.49
#'
#'  Skew + 3
#'  F= 7
#'  R^2 = 0.16
#'
#'  Kurtosis + 3
#'  F = 5
#'  R^2  = 0.10
#'
#' @param diameter_mean_basal_area_stem Diameter corresponding the mean basal area stem, cm.
#' @param stems Stems per hectare
#' @param dominant_height Dominant height, metres.
#' @param SI Site Index, m, according to Hägglund 1972, 1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce],[forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Volume distribution for stands of Norway Spruce after PCT
#' @export
Pettersson_1992_volume_distribution_after_PCT_Spruce <- function(
  diameter_mean_basal_area_stem,
  stems,
  dominant_height,
  SI
){

  mean <- exp(0.503 + 0.839*log(diameter_mean_basal_area_stem))
  standard_deviation <- exp(-2.361 + 0.956*log(dominant_height)+0.122*log(stems))
  skew_plus_3 <- exp(2.397 + 0.158*log(dominant_height) + 0.121*log(stems)) -0.774*log(SI)
  kurtosis_plus_3 <- exp(-0.306 -0.157*log(stems) + 0.783*log(SI))

  return(
    list(
      "Mean Volume"=mean,
      "Volume Std. Deviation"=standard_deviation,
      "Volume skew + 3"=skew_plus_3,
      "Volume kurtosis + 3"=kurtosis_plus_3
    )
  )

}
#' Basal area weighted mean diameter in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 17.
#'
#' No. of observations = 100
#' R^2 = 0.97
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_young_stands_BA_weighted_diameter_Pine <- function(
  dominant_height,
  diameter_mean_basal_area_stem
){
  return(
    exp(
      0.2924+0.1235*log(dominant_height)+0.8028*log(diameter_mean_basal_area_stem)
    )
  )

}
#' Basal area weighted mean diameter in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 18.
#'
#' No. of observations = 100
#' R^2 = 0.95
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_young_stands_BA_weighted_diameter_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem
){
  return(
    exp(
      0.3731+0.0632*log(dominant_height)+0.8332*log(diameter_mean_basal_area_stem)
    )
  )

}
#' Form quotient in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 24.
#'
#' @details
#'
#' function 13.
#'
#' No. of observations = 239
#' R^2 = 0.98
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Form quotient
#' @export
Pettersson_1992_young_stands_form_quotient_Pine <- function(
  dominant_height,
  initial_stems
){
  return(
    exp(
      0.1666-0.0282*log(initial_stems)+0.7054*log(dominant_height)
    )
  )

}
#' Form quotient in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 14.
#'
#' No. of observations = 198
#' R^2 = 0.97
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param SI SI from Hägglund 1972,1973.
#'
#' @return Form quotient
#' @export

Pettersson_1992_young_stands_form_quotient_Spruce <- function(
  dominant_height,
  initial_stems,
  SI
){
  return(
    exp(
      -0.9204-0.0286*log(initial_stems)+0.8294*log(dominant_height)+0.2265*log(SI)
    )
  )

}
#' Living basal area in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 15.
#'
#' No. of observations = 86
#' R^2 = 0.99
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param basal_area_m2_ha Basal Area m2/ha.
#'
#' @return Living basal area
#' @export
Pettersson_1992_young_stands_living_basal_area_Pine <- function(
  dominant_height,
  initial_stems,
  basal_area_m2_ha
){
  return(
    exp(
      0.1582+1.0176*log(basal_area_m2_ha)-0.0178*log(initial_stems)-0.0359*log(dominant_height)
    )
  )

}
#' Living basal area in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 16.
#'
#' No. of observations = 100
#' R^2 = 0.98
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param basal_area_m2_ha Basal Area m2/ha.
#'
#' @return Living basal area
#' @export

Pettersson_1992_young_stands_living_basal_area_Spruce <- function(
  dominant_height,
  initial_stems,
  basal_area_m2_ha
){
  return(
    exp(
      0.0803+1.0018*log(basal_area_m2_ha)-0.0072*log(initial_stems)-0.0133*log(dominant_height)
    )
  )

}
#' Mean diameter of the thickest trees per hectare in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 28.
#'
#' @details
#'
#' function 22. Mean diameter of thickest 800 trees per hectare.
#'
#' No. of observations = 100
#' R^2 = 0.91
#'
#'
#' function 23. Mean diameter of thickest 400 trees per hectare.
#'
#' No. of observations = 100
#' R^2 =  0.91
#'
#'
#' function 24. Mean diameter of thickest 100 trees per hectare.
#'
#' No. of observations = 100
#' R^2 = 0.86
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area.
#' @param thickest_x_trees One of '800', '400' or '100'.
#'
#' @return Basal area weighted diameter, in cm.
#' @export

Pettersson_1992_young_stands_mean_diameter_x_trees_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem,
  thickest_x_trees
){

  if(!(thickest_x_trees%in%c(100,400,800))){
    stop("The argument 'thickest_x_trees' must be one of 100, 400 or 800.")
  }

  ifelse(thickest_x_trees == 800,
         return(
           0.7246 + 0.2639 * log(dominant_height) + 0.5345 * log(diameter_mean_basal_area_stem)
         ),
         ifelse(thickest_x_trees == 400,
                return(
                  0.8600 + 0.2045 * log(dominant_height) + 0.5722 * log(diameter_mean_basal_area_stem)
                ),
                return(
                  1.1124 + 0.1263 * log(dominant_height)  + 0.5923 * log(diameter_mean_basal_area_stem)
                )))



}
#' Relationship between dominant height and Lorey's mean height in young stands of Scots Pine
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 20.
#'
#' @details Number of observations: 86.
#'
#' R^2 = 0.98.
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Lorey's Mean height of the stand.
#' @export

Pettersson_1992_young_stands_mean_height_Lorey_Pine <- function(
  dominant_height,
  initial_stems
){
  exp(
  -0.1534+
  +1.0637*log(dominant_height)+
  -0.01*log(initial_stems)
  )
}
#' Relationship between dominant height and Lorey's mean height in young stands of Norway Spruce
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 21.
#'
#' @details Number of observations: 100.
#'
#' R^2 = 0.97.
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param SI Site Index for Spruce, according to Hägglund 1972,1973, e.g.
#' [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce]
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Lorey's Mean height of the stand.
#' @export

Pettersson_1992_young_stands_mean_height_Lorey_Spruce <- function(
  dominant_height,
  initial_stems,
  SI
){
  exp(
    -0.4168+
    +1.0095*log(dominant_height)+
    -0.0558*log(initial_stems)+
    +0.2054*log(SI)
  )
}
#' Stems per hectare at time for first thinning in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 29.
#'
#' @details
#'
#' function 25. Stems per hectare at time for first thinning
#'
#' No. of observations = 86
#' R^2 = 0.97
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Number of planted stems per hectare
#' @param SI Site index according to Hägglund 1974 [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'
#' @return Number of stems at time for first thinning.
#' @export
Pettersson_1992_young_stands_stems_at_first_thinning_Pine <- function(
  dominant_height,
  initial_stems,
  SI
){

  return(
    exp(
      -0.5256-0.2160*log(dominant_height)+0.8760*log(initial_stems)+0.5789*log(SI)
    )
  )


}
#' Stems per hectare at time for first thinning in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 29.
#'
#' @details
#'
#' function 25. Stems per hectare at time for first thinning
#'
#' No. of observations = 100
#' R^2 = 0.98
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Number of planted stems per hectare
#' @param SI Site index according to Hägglund 1972,1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce]
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Number of stems at time for first thinning.
#' @export
Pettersson_1992_young_stands_stems_at_first_thinning_Spruce <- function(
  dominant_height,
  initial_stems,
  SI
){

  dummy <- ifelse(SI > 24, 1, ifelse(
    initial_stems < 2500,
    0,
    ifelse(
      initial_stems < 4000,-0.0404,
      ifelse(initial_stems < 6000,-0.0464, -0.0689)
    )
  ))


  return(
    exp(
      -0.5791+0.180*SI*dummy+1.0499*log(initial_stems)+log(dominant_height)*dummy
    )
  )


}
#' Total volume production in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 21.
#'
#' @details
#'
#' function 3.
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Total volume production, m3sk / ha.
#' @export

Pettersson_1992_young_stands_total_volume_Pine <- function(
  dominant_height,
  initial_stems
){

  return(
  initial_stems/(
    1873*(dominant_height^-2.4173) + 0.1482*initial_stems*(dominant_height^-1.4247)
  )
  )

}
#' Total volume production in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 21.
#'
#' @details
#'
#' function 4.
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Total volume production, m3sk / ha.
#' @export
Pettersson_1992_young_stands_total_volume_Spruce <- function(
  dominant_height,
  initial_stems
){

  return(
    initial_stems/(
      8094*(dominant_height^-2.8673) + 0.2511*initial_stems*(dominant_height^-1.6611)
    )
  )

}
#' Volume distribution in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 23.
#'
#' @details
#'
#' function 5-8.
#'
#' f. 5. mean Volume:
#'
#' No. of observations = 86.
#' R^2= 0.96
#'
#'
#'
#' f. 6 Standard Deviation of Volume:
#'
#' No. of observations = 86.
#' R^2 = 0.32
#'
#'
#'
#' f. 7 Volume skew
#'
#' No. of observations = 86.
#' R^2 = 0.25
#'
#'
#'
#' f. 8 Volume kurtosis
#'
#' No. of observations = 86
#' R^2 = 0.42
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_corresponding_to_mean_basal_area_cm Diameter corresponding to the mean basal area stem, in cm.
#' @param initial_stems Initial number of stems.
#'
#' @return List of distribution metrics.
#' @export

Pettersson_1992_young_stands_volume_distribution_Pine <- function(
  dominant_height,
  diameter_corresponding_to_mean_basal_area_cm,
  initial_stems
){
  mean_volume <- exp(0.3607+ 0.8345*log(diameter_corresponding_to_mean_basal_area_cm)+0.0675*log(dominant_height))

  volume_standard_deviation <- exp(-0.2696 + 0.1336*log(diameter_corresponding_to_mean_basal_area_cm)+0.3949*log(dominant_height))

  volume_skew_plus_3 <- exp(0.2825+0.0863*log(initial_stems))

  volume_kurtosis_plus_3 <- exp(2.6786-0.1827*log(initial_stems))

  return(
    list(
      "Mean Volume"=mean_volume,
      "Volume Std. Deviation"=volume_standard_deviation,
      "Volume skew + 3"=volume_skew_plus_3,
      "Volume kurtosis + 3"=volume_kurtosis_plus_3
        )
  )

}
#' Volume distribution in young stands of Norway spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 24.
#'
#' @details
#'
#' function 9-12.
#'
#' f. 9. mean Volume:
#'
#' No. of observations = 100
#' R^2= 0.94
#'
#'
#'
#' f. 10 Standard Deviation of Volume:
#'
#' No. of observations = 100
#' R^2 = 0.55
#'
#'
#'
#' f. 11 Volume skew
#'
#' No. of observations = 100
#' R^2 = 0.32
#'
#'
#'
#' f. 12 Volume kurtosis
#'
#' No. of observations = 100
#' R^2 = 0.42
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_corresponding_to_mean_basal_area_cm Diameter corresponding to the mean basal area stem, in cm.
#' @param initial_stems Initial number of stems.
#' @param H100 Site Index age 100 for Norway Spruce, e.g. [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()][forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()]
#'
#' @return List of distribution metrics.
#' @export

Pettersson_1992_young_stands_volume_distribution_Spruce <- function(
  dominant_height,
  diameter_corresponding_to_mean_basal_area_cm,
  initial_stems,
  H100
){
  mean_volume <- exp(0.5271 + 0.8418*log(diameter_corresponding_to_mean_basal_area_cm))

  #Note unclear double negation in text! Verify.
  volume_standard_deviation <- exp(2.1613 + 0.4634*log(diameter_corresponding_to_mean_basal_area_cm) - -0.6656*log(H100))

  volume_skew_plus_3 <- exp(0.9021 + 0.1278*log(initial_stems)-0.2647*log(H100))

  volume_kurtosis_plus_3 <- exp(1.2019-0.1900*log(initial_stems)+0.4445*log(H100))

  return(
    list(
      "Mean Volume"=mean_volume,
      "Volume Std. Deviation"=volume_standard_deviation,
      "Volume skew + 3"=volume_skew_plus_3,
      "Volume kurtosis + 3"=volume_kurtosis_plus_3
    )
  )

}
