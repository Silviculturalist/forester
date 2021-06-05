#' A stand-level growth and yield model for thinned and unthinned managed
#' Norway Spruce forests in Norway
#'
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: <https://doi.org/10.1080/02827581.2020.1773525>
#'
#' @param stand_age numeric. Age of stand, in years.
#' @param SI_40 numeric. Site index of stand at reference age 40.
#' @param stems numeric. Number of stems per hectare.
#' @param stems_after_thinning numeric. Number of stems per hectare.
#' @param basal_area numeric. Basal area at breast height (1.3 m) per hectare.
#' @param basal_area_after_thinning numeric. Basal area after thinning.
#' @param thinning_by default= "basal area quotient", "stems_percent"
#' @param thinning_strength If thinning_by=="basal area quotient, Basal Area after
#' thinning / Basal area before thinning. If thinning_by=="stems_percent": Stems
#' after thinning / Stems before thinning. For no thinning, 1.
#' @param dominant_height_at_thinning Dominant height of stand at thinning.
#' E.g. mean of tallest 10%.
#'
#' @return A data.frame. Stand description at stand age + 5 .
#' @export
#'
#' @examples
Allen_2020_P_abies_five_year_increment <- function(stand_age,
                                                   volume,
                                                   SI_40,
                                                   stems,
                                                   stems_after_thinning,
                                                   basal_area,
                                                   basal_area_after_thinning,
                                                   thinning_by="basal_area_quotient",
                                                   thinning_strength=1,
                                                   dominant_height_at_thinning
                                                   ){

  if(thinning_strength==1){
    basal_area_after_thinning <- basal_area
    stems_after_thinning <- stems
  }


  #if missing basal area after thinning but known thinning quotient.
  if(missing(basal_area_after_thinning) && !missing(basal_area) && thinning_by=="basal_area_quotient" && !missing(thinning_strength)){
    basal_area_after_thinning <- basal_area * thinning_strength
  }

  if(missing(stems_after_thinning) && !missing(stems) && thinning_by=="stems_percent" && !missing(thinning_strength)){
    stems_after_thinning <- stems * thinning_strength
  }


  #if missing basal area after thinning and stems_after_thinning is available
  if(missing(basal_area_after_thinning) && !missing(stems_after_thinning) && !missing(stems)){
    #Basal area reduction
    #5B

    basal_area_after_thinning <- ((log10(stems_after_thinning/stems) - -1.93267)/1.92953)*basal_area


  }

  if(thinning_by=="stems_percent" && missing(stems_after_thinning) && !missing(basal_area) && !missing(basal_area_after_thinning)){
    #Stem reduction
    #5A

    stems_after_thinning <- stems * (exp(-1.93267 + 1.92953*(basal_area_after_thinning/basal_area)))


  }


  #Future stems per hectare.

  stems2 <- ((stems_after_thinning^-1.0085) + 0.03675*(basal_area_after_thinning / basal_area)*
    ((SI_40/1000)^3.76228)*((stand_age+5)^2.5541 - stand_age*2.5541))^(1/-1.0097)



  #Dominant Height

  height2 <- Allen_2020_H2_Norway_spruce(dominant_height = dominant_height_at_thinning,
                                         stand_age=stand_age,
                                         ref_age= stand_age+5)




  #Hasenauer et al (1997) basal area

  TR <- (basal_area / basal_area_after_thinning)^(-0.1479*(dominant_height_at_thinning/height2))

  basal_area2 <- (basal_area_after_thinning^(dominant_height_at_thinning/height2)) * exp( (4.77696 * ((stems2/stems_after_thinning)^(0.30957)))* ((1-(dominant_height_at_thinning/height2))*TR) )





  #Schumacher 1939 volume equation

  V2 <- 0.24961 * (basal_area2^(1.15036)) * (height2*(1.01153)) * (exp(2.320398/(stand_age+5)))


  return(
    data.frame("Age"=stand_age+5,
               "Dominant height m"= height2,
               "Site Index 40"=SI_40,
               "Stems per ha"=stems2,
               "Basal area m2 ha" = basal_area2,
               "Volume m3 ha"= V2,
               "Thinned by"= thinning_by,
               "Thinning strength"=thinning_strength)
  )





}
