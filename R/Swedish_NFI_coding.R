#' Swedish vegetation types (NFI).
#'
#' @param layer One of "field" or "ground".
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' Sweden_vegetation_types()
Sweden_vegetation_types <- function(layer="field"){
  if(layer=="field"){
  cat("Field Layer vegetation types according to the Swedish NFI\n")
  return(
    dplyr::tibble(Code=seq(1,18,1),Type=c("Tall herbs without dwarf shrubs",
                                 "Tall herbs with bilberry",
                                 "Tall herbs with cowberry",
                                 "Low herbs without dwarf shrubs",
                                 "Low herbs with bilberry",
                                 "Low herbs with cowberry",
                                 "No field layer",
                                 "Broad-leaved grasses",
                                 "Narrow-leaved grasses",
                                 "Sedge, tall",
                                 "Sedge, low",
                                 "Horsetail, Equisetum ssp.",
                                 "Bilberry",
                                 "Cowberry",
                                 "Crowberry",
                                 "Poor shrubs",
                                 "Lichen-rich",
                                 "Lichen-dominated"))
  )
  }

  if(layer=="ground"){
    cat("Ground layer vegetation types according to the Swedish NFI\n")
    return(
    dplyr::tibble(Code=seq(1,6,1),English=c(
      "Lichen type","Lichen-rich bogmoss type","Lichen rich","Bogmoss type","Swamp moss type","Fresh moss type"),
      Swedish=c("Lavtyp","Lavrik vitmosstyp","Lavrik typ","Vitmosstyp","Sumpmosstyp","Friskmosstyp"),
      Description=c("(>50 % of existing ground layer)",
       "(>25% lichen  +  >50% Sphagnum of existing ground layer)",
       "(>25% lichen & not >50% Sphagnum of existing ground layer)",
       "(Sphagnum > 50% of existing ground layer)",
       "(Polytrichum commune, P. gracile, P. strictum, Sphagnum, Depranocladus, Scorpidium, Paludella, Calliergon, Tomentyptum, Campylium)",
       "(Hylocomium splendens, Ptilium crista-castrensis, Dicranum ssp.)"
    ))
    )
  }
}

#' Coding and description of swedish categorical soil parameters.
#'
#' @param type One of "texture","moisture","water" or "depth"
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' Sweden_soil_types()
Sweden_soil_types <- function(type="texture"){
  if(type=="texture"){
    cat("Swedish soil textures\n")
    return(
      dplyr::tibble(
      Code=seq(1,9,1),SHORT_EN=c(
        "Boulder",
        "Gravel",
        "Course sand",
        "Medium sand",
        "Fine sand",
        "Coarse silt",
        "Fine silt",
        "Clay",
        "Peat"
      ),SWE_MORAINE=c(
        "Stenig/blockig morän",
        "Grusig morän",
        "Sandig morän",
        "SANDIG-moig morän",
        "Sandig-MOIG morän",
        "Moig morän",
        "Mjälig morän",
        "Lerig morän",
        "Torv"
      ),EN_MORAINE=c(
        "Boulder rich/stone till",
        "Gravelly till",
        "Sandy till",
        "Sandy-silty till",
        "Silty-sandy till",
        "Coarse silty till",
        "Fine silty till",
        "Clayey till",
        "Peat"
      ),SWE_SEDIMENT=c(
        "Sten/block",
        "Grus",
        "Grovsand",
        "Mellansand",
        "Grovmo",
        "Finmo",
        "Mjäla",
        "Lera",
        "Torv"
      ), EN_SEDIMENT=c(
        "Boulders/stones",
        "Gravel",
        "Coarse sand",
        "Medium sand",
        "Fine sand",
        "Coarse silt",
        "Fine silt",
        "Clay",
        "Peat"
      )
    )
    )
  }

  if(type=="depth"){
    cat("Swedish soil depth coding\nCorresponds to NFI-variable JORDDJUP. Coding not the same.\n")
    return(
      dplyr::tibble(
        Code=seq(1,4,1),
        Characteristic=c(">70 cm. No visible bedrock","20-70 cm","<20 cm, plenty bedrock visible.","Varying depth. Breaks in bedrock somewhat visible."),
        English=c("Deep","Rather shallow","Shallow","Varying"),
        Swedish=c("Mäktigt","Tämligen grunt jorddjup","Grunt jorddjup","Mycket varierande jorddjup.")
      )
    )
  }

  if(type=="moisture"){
    cat("Swedish soil moisture coding\nCorresponds to NFI-variable FUKTIGHET\n")
    return(
      dplyr::tibble(
        Code=seq(1,5,1),
        English=c("Dry","Mesic","Mesic-moist","Moist","Wet"),
        Swedish=c("Torr","Frisk","Frisk-fuktig","Fuktig","Blöt"),
        Characteristic=c("Subsoil water depth >2m",
                         "Subsoil water depth 1-2m",
                         "Subsoil water depth <1m",
                         "Subsoil water depth <1, pools in hollows.",
                         "Pools visible")
      )
    )
  }

  if(type=="water"){
    cat("Swedish soil water / lateral water coding\nCorresponds to NFI-variable RÖRLMVA\n")
    return(
      dplyr::tibble(
        Code=seq(1,3,1),
        English=c("Seldom/never","Shorter periods","Longer periods"),
        Swedish=c("Saknas","Kortare perioder","Längre perioder")
      )
    )
  }
}
