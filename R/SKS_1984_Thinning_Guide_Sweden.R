
#' Thinning Recommendations according to the Swedish Forest Agency 1984.
#'
#' @details Based on a regression of the original diagrams used in Heureka
#'  StandWise v. 2.19. e.g. 2022-09-17.
#'
#' @param H100 Site Index H100 according to Hägglund 1972, 1973. e.g.
#' [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce],
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce],
#' [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param dominantHeight dominant height of the stand.
#' @param northernSweden boolean. TRUE if stand is located in northern Sweden
#' (default).
#' @param species One of "Pinus sylvestris","Picea abies","Pinus contorta",
#' "Quercus robur,"Fagus sylvatica" or "Deciduous".
#'
#' @return A list of the maximum reccomended basal area per hectare before and
#' the lowest reccomended basal area after thinning.
#' @export
#'
#' @examples
#'
#'
#' data.frame(
#'   "dominantHeight"=seq(10,32,0.1)
#' ) %>% group_by_all() %>%
#'   mutate(data=purrr::map(.x=dominantHeight,
#'                         .f = SKS_1984_Thinning_Guide_Sweden,H100=36,
#'                         northernSweden = FALSE,
#'                         species="Picea abies")) %>%
#'   unnest_wider(data) %>%
#'   ggplot() +
#'   xlim(c(10, 32)) +
#'   ylim(c(18, 44)) +
#'   geom_line(aes(x=dominantHeight,y=Before))+
#'   geom_line(aes(x=dominantHeight,y=After))+
#'   ylab("Basal Area m^2/ha")

SKS_1984_Thinning_Guide_Sweden <- function(H100,dominantHeight,northernSweden=TRUE,species)
{
  southernSweden=ifelse(northernSweden,FALSE,TRUE)
  if(dominantHeight<10) warning("Under 10 meters.")
  if(dominantHeight>30) warning("Over 30 meters.")

  if(species=="Pinus sylvestris"){
    before=
    -25.90492203+
    0.53672045*H100+
    15.30465492*log(dominantHeight)+
    -0.094893472*dominantHeight+
    -0.050606076*southernSweden*H100+
    -0.001537977*southernSweden*H100*dominantHeight
    after=
      -17.66501787+
      0.368373957*H100+
      9.621388777*log(dominantHeight)+
      0.158986599*dominantHeight+
      0*southernSweden*H100+
      -0.003788667*southernSweden*H100*dominantHeight

  } else if(species=="Picea abies"){
    before=
      -43.4675985+
      0.1918048*H100+
      30.143285*log(dominantHeight)+
      -0.9531249*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight
    after=
      -24.1296749+
      0.1863528*H100+
      15.7701722*log(dominantHeight)+
      -0.1312119*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight

  } else if(species=="Pinus contorta"){
    before=
      -25.90492203+
      0.53672045*H100+
      15.30465492*log(dominantHeight)+
      -0.094893472*dominantHeight+
      -0.050606076*southernSweden*H100+
      -0.001537977*southernSweden*H100*dominantHeight
    after=
      -17.66501787+
      0.368373957*H100+
      9.621388777*log(dominantHeight)+
      0.158986599*dominantHeight+
      0*southernSweden*H100+
      -0.003788667*southernSweden*H100*dominantHeight

  } else if(species=="Fagus sylvatica"){
    before=
      -43.4675985+
      0.1918048*H100+
      30.143285*log(dominantHeight)+
      -0.9531249*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight
    after=
      -24.1296749+
      0.1863528*H100+
      15.7701722*log(dominantHeight)+
      -0.1312119*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight

  } else if(species=="Quercus robur"){
    before=
      -43.4675985+
      0.1918048*H100+
      30.143285*log(dominantHeight)+
      -0.9531249*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight
    after=
      -24.1296749+
      0.1863528*H100+
      15.7701722*log(dominantHeight)+
      -0.1312119*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight

  } else if(species=="Deciduous"){
    before=
      -54+
      0.1918048*H100+
      30.143285*log(dominantHeight)+
      -0.9531249*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight
    after=
      -33+
      0.1863528*H100+
      15.7701722*log(dominantHeight)+
      -0.1312119*dominantHeight+
      0*southernSweden*H100+
      0*southernSweden*H100*dominantHeight

  }

  return(
    list(
      "Before"=before,
      "After"=after
    )
  )
}



