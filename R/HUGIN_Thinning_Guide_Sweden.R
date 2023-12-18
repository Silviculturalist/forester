
#' Thinning Recommendations according to the HUGIN Forest model.
#'
#' @details Based on coefficients used in Heureka
#'  StandWise v. 2.19. e.g. 2022-09-17.
#'
#'  OBS! For Quercus robur, Fagus sylvatica and Deciduous,
#'  only two limits are available - unknown if these represent the lower or
#'  upper limit before and after thinning.
#'
#'
#'
#' @param H100 Site Index H100 (m) according to Hägglund 1972, 1973. e.g.
#' [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce],
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce],
#' [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param dominantHeight dominant height of the stand (m).
#' @param species One of "Pinus sylvestris","Picea abies","Pinus contorta",
#' "Quercus robur,"Fagus sylvatica" or "Deciduous".
#'
#' @return A list of the upper and lower limits for recommended basal area per
#' hectare before and after thinning.
#' @export
#' @examples
#'
#' data.frame(
#'   dominantHeight=seq(10,36,0.1)
#' ) %>% group_by_all() %>% mutate(
#'   data=purrr::map(.x=dominantHeight,.f=HUGIN_Thinning_Guide_Sweden,36,"Picea abies")
#' ) %>% unnest_wider(data) %>%
#'   ggplot(aes(x=dominantHeight))+
#'   geom_ribbon(aes(ymin=BeforeLowerLimit,ymax=BeforeUpperLimit),alpha=0.5)+
#'   geom_ribbon(aes(ymin=AfterLowerLimit,ymax=AfterUpperLimit),alpha=0.5)


HUGIN_Thinning_Guide_Sweden <- function(dominantHeight,H100,species)
{
  if(dominantHeight<10) warning("Under 10 meters.")
  if(dominantHeight>30) warning("Over 30 meters.")

  dominantHeight=dominantHeight*10 #OBS! height must
  #be in dm for calculations. SI in m.

  if(species=="Pinus sylvestris"){
    beforeLowerLimit=-1.5+0.5*H100+0.11*dominantHeight
    beforeUpperLimit=5+0.5*H100+0.075*dominantHeight
    afterLowerLimit=-1.8+0.35*H100+0.085*dominantHeight
    afterUpperLimit=0.4+0.35*H100+0.065*dominantHeight
  } else if(species=="Picea abies"){
    beforeLowerLimit=3+0.255*H100+0.135*dominantHeight
      beforeUpperLimit=14+0.25*H100+0.067*dominantHeight
      afterLowerLimit=0+0.2*H100+0.109*dominantHeight
      afterUpperLimit=7+0.18*H100+0.06*dominantHeight

  } else if(species=="Pinus contorta"){
    beforeLowerLimit=0+0.5*H100+0.11*dominantHeight
      beforeUpperLimit=5.5+0.5*H100+0.075*dominantHeight
      afterLowerLimit=-0.75+0.35*H100+0.085*dominantHeight
      afterUpperLimit=1.45+0.35*H100+0.07*dominantHeight

  } else if(species=="Fagus sylvatica"){
    beforeLowerLimit=11.5+0*H100+0.07*dominantHeight
      beforeUpperLimit=NA
      afterLowerLimit=8.5+0*H100+0.05*dominantHeight
      afterUpperLimit=NA
  } else if(species=="Quercus robur"){
    beforeLowerLimit=11.5+0*H100+0.05*dominantHeight
      beforeUpperLimit=NA
      afterLowerLimit=6.5+0*H100+0.04*dominantHeight
      afterUpperLimit=NA
  } else if(species=="Deciduous"){
    beforeLowerLimit=5+0.4*H100+0.05*dominantHeight
      beforeUpperLimit=NA
      afterLowerLimit=1.5+0.4*H100+0.01*dominantHeight
      afterUpperLimit=NA
  }

  return(
    list(
      "BeforeLowerLimit"=beforeLowerLimit,
      "BeforeUpperLimit"=beforeUpperLimit,
      "AfterLowerLimit"=afterLowerLimit,
      "AfterUpperLimit"=afterUpperLimit
    )
  )
}


