#' Height Development function for Norway Spruce in Norway, from Allen et al. 2020.
#'
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: \url{https://doi.org/10.1080/02827581.2020.1773525}
#'
#' @details N.B. Although the paper uses log10(), it should be ln()!
#' This is corrected in this function.
#'
#'
#'
#' N.B. The function fitted in the paper has several changes compared to the function from
#' Diéguez-Aranda, U.; Burkhart, H. E.; Rodríguez-Soalleiro, R. (2005). Modeling dominant
#' height growth of radiata pine (Pinus radiata D. Don) plantations in north-western Spain.
#' Forest Ecology and Management. Vol. 215. #1-3. pp. 271-284.
#' <https://doi.org/10.1016/j.foreco.2005.05.015>
#'
#' @param dominant_height numeric. Dominant height of stand in metres.
#' @param stand_age numeric. Stand age, years from planting.
#' @param ref_age numeric. Reference age. Age at output.
#'
#' @return numeric. Height at reference age in metres.
#' @export
Allen_2020_height_development_Norway_spruce <- function(dominant_height,
                                                        stand_age,
                                                        ref_age){

  beta1 <- 0.01605
  beta2 <- 0.61208
  beta3 <- 4.43722

  L <- log(1 - exp(-beta1*stand_age))

  X0 <- 0.5*((log(dominant_height) + beta2*L) + sqrt((log(dominant_height) - beta2*L)^2 - (4*beta3*L)))

  H2 <- dominant_height*((1-exp(-beta1*ref_age))/(1-exp(-beta1*stand_age)))^((beta2+beta3)/X0)

  return(H2)

}


#' Volume of Norway Spruce stand
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: \url{https://doi.org/10.1080/02827581.2020.1773525}
#'
#' @param basalArea Basal Area m2/ha
#' @param dominantHeight Dominant height of stand, m.
#' @param Age Stand Age (total).
#' @return Volume, m3/ha.
#' @export
Allen_2020_Volume_Norway_Spruce_Norway <- function(
  basalArea,
  dominantHeight,
  Age
){
  return(
    0.24961 * (basalArea^(1.15036)) * (dominantHeight*(1.01153)) * (exp(2.320398/(Age)))
  )
}

#Basal area at the end of a growth period
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: \url{https://doi.org/10.1080/02827581.2020.1773525}
#' @param basalAreaBeforeThinning Basal area at period start before thinning
#' @param basalAreaAfterThinning Basal area after thinning.
#' @param dominantHeight1 Dominant height at start of period
#' @param dominantHeightAtThinning Dominant height at thinning
#' @param dominantHeight2 Dominant height at end of period.
#' @param stemsBeforeThinning Number of stems per hectare before thinning.
#' @param stemsAfterThinning Number of stems per hectare after thinning.
#' @return Basal Area m2/ha at period end.
#' @export
Allen_2020_Basal_Area_Norway_Spruce_Norway <- function(
  basalAreaBeforeThinning,
  basalAreaAfterThinning,
  dominantHeight1,
  dominantHeightAtThinning,
  dominantHeight2,
  stemsBeforeThinning,
  stemsAfterThinning
){
  b1=4.77696
  b2=0.30957
  b3= -0.1479
  TR = (basalAreaAfterThinning/basalAreaBeforeThinning)^(b3*(dominantHeightAtThinning/dominantHeight2))

  return(
    (basalAreaAfterThinning^(dominantHeight1/dominantHeight2))*exp(b1*((stemsAfterThinning/stemsBeforeThinning)^b2)*(1-(dominantHeight1/dominantHeight2))*TR)
  )
}


#Calculate the number of surviving stems
#' @source Allen, M.G. II, Antón-Fernández, C., Astrup, R. (2020) "A stand-level
#' growth and yield model for thinned and unthinned managed Norway spruce forests
#' in Norway". Scandinavian Journal of Forest Research. Vol. 35. pp. 238-251.
#' DOI: \url{https://doi.org/10.1080/02827581.2020.1773525}
#' @param stemsAfterThinning Number of stems per hectare remaining after thinning
#' @param basalAreaBeforeThinning Basal Area per hectare before thinning (m2/ha)
#' @param basalAreaAfterThinning Basal area per hectare after thinning (m2/ha)
#' @param standAge Stand total age at period start.
#' @param standAge2 Stand total age at end of period.
#' @param SI40 SI40, e.g. [forester::Allen_2020_height_development_Norway_spruce()]
#' @param  IncrementPeriod Always 5 (default).
#' @return Number of surviving stems at the end of growth period.
#' @export
Allen_2020_surviving_stems_Norway_Spruce_Norway <- function(
    stemsAfterThinning,
    basalAreaBeforeThinning,
    basalAreaAfterThinning,
    standAge,
    standAge2,
    SI40,
    IncrementPeriod=5){
  return(
    ((stemsAfterThinning^-1.0085) + 0.03675*(basalAreaAfterThinning / basalAreaBeforeThinning)*
       ((SI40/1000)^3.76228)*((standAge+IncrementPeriod)^2.5541 - standAge*2.5541))^(1/-1.0097)
  )
}


#' Get the number of stems after thinning if thinning by basal area.
#' @details Get the number of stems after thinning if you are thinning by basal area.
#' @param basalAreaBeforeThinning Basal Area per hectare before thinning (m2/ha)
#' @param basalAreaAfterThinning Basal Area per hectare after thinning (m2/ha)
#' @param stemsBeforeThinning Number of stems per hectare before thinning.
#' @return Number of stems remaining after thinning per hectare.
#' @export
#' @seealso [forester::Allen_2020_basal_area_after_thinning_Norway_Spruce_Norway()]
Allen_2020_stems_after_thinning_Norway_Spruce_Norway <- function(
  basalAreaBeforeThinning,
  basalAreaAfterThinning,
  stemsBeforeThinning
){
  return(
    stemsBeforeThinning*(exp(-1.93267 + 1.92953*(basalAreaAfterThinning/basalAreaBeforeThinning)))
  )
}


#' Get the basal area after thinning if thinning by stem numbers.
#' @details Get the basal area after thinning if you are thinning by number of stems.
#' @param basalAreaBeforeThinning Basal Area per hectare before thinning (m2/ha)
#' @param stemsBeforeThinning Number of stems per hectare before thinning.
#' @param stemsAfterThinning Number of stems per hectare after thinning.
#' @return Number of stems remaining after thinning per hectare.
#' @export
#' @seealso [forester::Allen_2020_stems_after_thinning_Norway_Spruce_Norway()]
Allen_2020_basal_area_after_thinning_Norway_Spruce_Norway <- function(
    basalAreaBeforeThinning,
    stemsBeforeThinning,
    stemsAfterThinning
){
  return(
    ((log(stemsAfterThinning/stemsBeforeThinning)- -1.93267)/1.92953)*basalAreaBeforeThinning
  )
}
