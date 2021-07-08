#' si_to_bonitet_integrated
#' @description Interface for Hägglund (1981) "Samband mellan ståndortsindex H100 och bonitet för tall och gran i Sverige", Report #26, Projekt Hugin. This function is separate from `forester::si_to_bonitet`. In`forester::si_to_bonitet_integrated` function choice is controlled by indirect parameter input.
#' @param H100 Estimated stand top height at 100 years age.
#' @param main_species Main species.
#' @param vegetation Vegetation class.
#' @param altitude Metres above sea level.
#' @param county Swedish county.
#' @return Estimates the mean volume growth in m3sk / ha yr-1 at the time of culmination (or if late culmination, at 150yrs of age)
#' @export
#' @details
#'
#'  Productive potential as a function of site index
#'
#'  a - Spruce, southern Sweden.
#'
#'  b - Spruce, middle Sweden, herb types, grass types and ground without field layer.
#'
#'  c - Spruce, middle Sweden, bilberry type and worse.
#'
#'  d - Spruce, northern Sweden, herb types, grass types and ground without field layer.
#'
#'  e - Spruce, northern Sweden, bilberry type and worse.
#'
#'  f - Pine, southern and middle Sweden as well as lowlands of northern Sweden.
#'
#'  g - Pine, northern Sweden, more than 200 meters above sea level.
#'
#' @examples
#' si_to_bonitet_integrated(H100=14, function="a")

Hagglund_1981_si_to_bonitet <- function(H100, main_species, vegetation, altitude, county){
  stopifnot(is.numeric(H100))
  stopifnot(H100>0)

  F1 <- 0.72 + (H100/130)
  F2 <- 0.70 + (H100/100)

 #Spruce
  if(main_species=="Picea abies"){
    #Northern Sweden
    if(county %in% c("Norrbottens lappmark",
                     "Norrbottens kustland",
                     "Västerbottens lappmark",
                     "Västerbottens kustland",
                     "Västernorrland - Ångermanlands landskap",
                     "Västernorrland - Medelpads landskap",
                     "Jämtland - Jämtlands landskap",
                     "Jämtland - Härjedalens landskap",
                     "Kopparberg - Sälen-Idre")){

      #herb types, grass types, ground without field layer.
      if(vegetation <=9){
        fun <- "d"
        #bilberry type and worse.
      } else if(vegetation>9){
        fun <- "e"
      }



      #Middle Sweden
    } else if(county%in% c("Kopparberg - övriga",
                           "Gävleborg - Hälsinglands landskap",
                           "Gävleborg - övriga",
                           "Kopparberg - övriga",
                           "Värmland")){
      #herb types, grass types and ground without field layer.
      if(vegetation <=9){
        fun <- "b"
        #bilberry type and worse.
      } else if(vegetation>9){
        fun <- "c"
      }


        #Southern Sweden
    } else {
      fun <- "a"

    }
  } else if(main_species=="Pinus sylvestris"){

    #Northern Sweden, altitude 200 masl and above.
    if(county %in% c("Norrbottens lappmark",
                     "Norrbottens kustland",
                     "Västerbottens lappmark",
                     "Västerbottens kustland",
                     "Västernorrland - Ångermanlands landskap",
                     "Västernorrland - Medelpads landskap",
                     "Jämtland - Jämtlands landskap",
                     "Jämtland - Härjedalens landskap",
                     "Kopparberg - Sälen-Idre") & altitude>=200){
      fun <- "g"
    } else {
      fun <- "f"
    }


  }





  if(fun=="a"){

    bon <- (0.57207 + (0.22166 * H100) + (0.0050164 * (H100^2)))*F1

  } else if (fun=="b"){

    bon <- (1.28417 + (0.31060* H100) + (0.0020048 * (H100^2)))*F1

  } else if (fun=="c"){

    bon <- (-0.42289 + (0.17735* H100) + (0.0050580 * (H100^2)))*F1

  } else if (fun=="d"){

    bon <- (-0.75761 + (0.24393* H100) + (0.0014564 * (H100^2)))*F2

  } else if (fun=="e"){

    bon <- (-0.59224 + (0.21765* H100) + (0.0011391 * (H100^2)))*F2

  } else if (fun=="f"){

    bon <- (-0.39456 + (0.16469* H100) + (0.0047191 * (H100^2)))*F2

  } else if (fun=="g"){

    bon <- (0.099227 + (0.067873* H100) + (0.0066316 * (H100^2)))*F2

  } else {
    stop(paste0(fun," is an unrecognised function. Please use one between a & g."), call.=F)
  }

  return(bon)

}
