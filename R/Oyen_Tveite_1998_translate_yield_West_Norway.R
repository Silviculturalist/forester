#' Stem volume yield between different tree species in West Norway from Øyen & Tveite 1998.
#'
#' @source Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32.
#'
#' @param species_from One of : "Picea abies","Pinus sylvestris","Betula pubescens","Picea sitchensis"
#' @param species_to One of : "Picea abies","Pinus sylvestris","Betula pubescens","Picea sitchensis"
#' @param potential_yield_m3_ha_yr Estimated maximum mean annual yield increment.
#'
#' @return Value.
#' @export
#'
#' @examples
#'Oyen_Tveite_1998_translate_yield_West_Norway(species_from = "Picea abies",species_to = "Picea sitchensis",potential_yield_m3_ha_yr = 5)
Oyen_Tveite_1998_translate_yield_West_Norway <- function(
  species_from,
  species_to,
  potential_yield_m3_ha_yr
){
  if(species_from==species_to){
    warning("species from == species_to. Returning potential_yield_m3_ha_yr")
    return(
      potential_yield_m3_ha_yr
    )
  }

  if(species_from=="Betula pubescens" & species_to=="Picea abies"){
    warning("Using Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32. p. 17.")
    return(
      3.93 + 1.687*potential_yield_m3_ha_yr
    )
  }

  if(species_from=="Picea abies" & species_to=="Betula pubescens"){
    warning("Using Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32. p. 17.")
    return(
      0.87 + 0.209*potential_yield_m3_ha_yr
    )
  }

  if(species_from=="Picea abies" & species_to=="Pinus sylvestris"){
    warning("Using Braastad (1983,1985) from Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32. p. 19.")
    return(1.984 + 0.6224*potential_yield_m3_ha_yr)
  }

  if(species_from=="Pinus sylvestris" & species_to=="Picea abies"){
    warning("Using Braastad (1983,1985) from Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32. p. 19.")
    return(-1.263 + 1.2195*potential_yield_m3_ha_yr)
  }

  if(species_from=="Pinus sylvestris" & species_to=="Betula pubescens"){
    warning("Using Braastad (1983,1985) from Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32. p. 19.")
    return(0.171 + 0.6206*potential_yield_m3_ha_yr)
  }

  if(species_from=="Betula pubescens" & species_to=="Pinus sylvestris"){
    warning("Using Braastad (1983,1985) from Øyen & Tveite 1998. En sammenligning av høydebonitet og produksjonsevne mellom ulika treslag på samme vokstested i Vest-Norge. A comparison of site index class and potential stem volume yield between different tree species growing on equal sites in West Norway. Rapport fra skogforskningen. Nr. 15. 1-32. p. 19.")
    return(1.209 + 1.0707*potential_yield_m3_ha_yr)
  }

  if(species_to=="Picea sitchensis" & species_from=="Picea abies"){
    warning("Little underlying material. A careful general conclusion is that Picea sitchensis produces 34% more than Picea abies")
    return(1.34*potential_yield_m3_ha_yr)
  }

  if(species_to=="Picea abies" & species_from=="Picea sitchensis"){
    warning("Little underlying material. A careful general conclusion is that Picea sitchensis produces 34% more than Picea abies")
    warning("The inverse is assumed to be true.")
    return(potential_yield_m3_ha_yr/(1.34))
  }

  if(species_to=="Picea sitchensis" & species_from=="Pinus sylvestris"){
    warning("Little underlying material. A careful general conclusion is that Pinus sylvestris produces 46% of that of Picea sitchensis")
    warning("The inverse is assumed to be true.")
    return(potential_yield_m3_ha_yr/0.46)
  }

  if(species_to=="Pinus sylvestris" & species_from=="Picea sitchensis"){
    warning("Little underlying material. A careful general conclusion is that Pinus sylvestris produces 46% of that of Picea sitchensis")
    warning("The inverse is assumed to be true.")
    return(potential_yield_m3_ha_yr*0.46)
  }

  if(species_to=="Picea sitchensis" & species_from=="Betula pubescens"){
    warning("Little underlying material. A careful general conclusion is that Betula pubescens produces 27% of that of Picea sitchensis")
    warning("The inverse is assumed to be true.")
    return(potential_yield_m3_ha_yr/0.27)
  }

  if(species_to=="Picea sitchensis" & species_from=="Betula pubescens"){
    warning("Little underlying material. A careful general conclusion is that Betula pubescens produces 27% of that of Picea sitchensis")
    warning("The inverse is assumed to be true.")
    return(potential_yield_m3_ha_yr*0.27)
  }

  return(
    warning("Conversion function not found between species")
  )
}
