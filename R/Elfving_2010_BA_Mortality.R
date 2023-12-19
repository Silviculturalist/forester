#' Mortality in Swedish stands of Scots Pine and Norway Spruce.
#'
#' @source Elfving, B. Natural mortality in thinning and fertilisation experiments with pine and spruce in Sweden. Forest Ecology and Management. Vol. 260. Issue 3. Available online:
#' \url{https://doi.org/10.1016/j.foreco.2010.04.025}
#'
#' @param dominant_height Dominant height of stand.
#' @param H100 Site Index H100 [forester::Elfving_Kiviste_1997_height_trajectory_Sweden_Pine()], [forester::Elfving_2003_height_trajectory_Sweden_Spruce()]
#' @param latitude Degrees N.
#' @param stems Stems/ha at period start.
#' @param ThinningProportionBAStart Proportion of basal area thinned at start of period. 0 - 0.8
#' @param ThinningForm Boolean. 1 if thinned from above or unthinned, otherwise 0.
#' @param fertilised Boolean. 1 if a fertilised stand, otherwise 0.
#' @name ElfvingMortality2010
#' @return Annual mortality in percent of basal area at start of period.
#' @export
Elfving_2010_BasalArea_Mortality_Percent_Spruce_Pine_Sweden <- function(
  dominant_height,
  H100,
  latitude,
  stems,
  ThinningProportionBAStart,
  ThinningForm=1,
  fertilised=0
){

  DENS = (stems*dominant_height^2)/100000
  return(
    -0.4093+
    +0.02189*H100+
    +0.005373*(DENS^2)+
    +0.3817*dominant_height*(ThinningProportionBAStart^3)+
    +0.01252*dominant_height*ThinningForm
  )
}

#' @rdname ElfvingMortality2010
#' @export
Elfving_2010_GMax_Spruce_Pine_Sweden <- function(
  dominant_height,
  H100,
  stems,
  latitude
){
  return(
    -89.7+
      2.47*dominant_height+
      0.0212*((dominant_height^3)/H100)+
      0.834*H100+
      0.00350*stems+
      0.890*latitude
  )
}
#
# Elfving_2010_GMax_Spruce_Pine_Sweden <- Vectorize(Elfving_2010_GMax_Spruce_Pine_Sweden)
#
#
# ggplot()+
#   scale_x_continuous(limits=c(8,24),breaks=seq(8,24,4))+
#   scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10),
#                      expand=expansion(mult=c(0,0)))+
#   geom_function(fun=(\(x) Elfving_2010_GMax_Spruce_Pine_Sweden(dominant_height = x,
#                                                                H100 = 20,
#                                                                stems = 2500,
#                                                                latitude = 58)))+
#   geom_function(fun=(\(x) Elfving_2010_GMax_Spruce_Pine_Sweden(dominant_height = x,
#                                                                H100 = 28,
#                                                                stems = 2500,
#                                                                latitude = 58)))
#
#
# ggplot()+
#   scale_x_continuous(limits=c(8,32),breaks = seq(8,32,4))+
#   scale_y_continuous(limits=c(0,70),breaks= seq(0,70,10),
#                      expand=expansion(mult=c(0,0)))+
#   geom_function(fun=(\(x) Elfving_2010_GMax_Spruce_Pine_Sweden(dominant_height = x,
#                                                                H100 = 36,
#                                                                stems = 2500,
#                                                                latitude = 58)))+
#   geom_function(fun=(\(x) Elfving_2010_GMax_Spruce_Pine_Sweden(dominant_height = x,
#                                                                H100 = 28,
#                                                                stems = 2500,
#                                                                latitude = 58)))

