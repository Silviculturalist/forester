#' Basal area for stand initiation from Hägglund (1981)
#'
#' @source Hägglund, Björn (1981) Samband mellan ståndortsindex H100 och bonitet för
#' tall och gran i Sverige. Projekt HUGIN. Rapport 26. Skogsvetenskapliga fakultet.
#' Sveriges Lantbruksuniversitet. Umeå.
#'
#' @param species "Picea abies" or "Pinus sylvestris".
#' @param region_code 1-5. 1: Lappmarken, 2: Norr-& Västerbottens kustland & Jämtland, Kopparberg - Sälen-Idre.
#' 3: Västernorrland, Gävleborg, Kopparberg -övrigt , Värmland.
#' 5: Gotland, Skåne, Halland, Blekinge, Göteborg & Bohus.
#' @param dominant_height_m Dominant height of stand, meters.
#' @param altitude_m Meters above sea level.
#' @param SI Site index H100 of species.
#' @param vegetation 1-18.
#'
#' @return list of 3 elements: 1. Basal Area m^2/ha.
#' 2. Multiple Coefficient of Correlation for used function.
#' 3. Spread about the function, m^2/ha.
#' @export
#'
#' @examples
#' Hagglund_1981_initial_basal_area(species="Picea abies",region_code = 5,dominant_height_m = 10,altitude_m = 0,SI = 36,vegetation = 1)
Hagglund_1981_initial_basal_area <- function(
  species,
  region_code,
  dominant_height_m,
  altitude_m,
  SI,
  vegetation
){
  dominant_height_dm <- 10*dominant_height_m
  SIdm <- 10*SI

  if(species=="Pinus sylvestris"){

  if(region_code==1){
    return(list(
      "Basal area m^2/ha"=
      (2.5543+
       +0.1166*(((dominant_height_dm)^2)/100)+
       +(((0.002566*(SIdm)^3))/10000)),
      "Multiple Coefficient of Correlation"= 0.63,
      "Spread about the function, m2/ha"=4.2

    )
    )
  } else if(region_code==2){

    lichen_type <- ifelse(vegetation%in%c(17,18),1,0)

    return(list(
      "Basal area m^2/ha"=
      (7.6519+
      +0.04771*dominant_height_dm*SIdm/100+
      -0.3317*dominant_height_dm+
      -0.2442*((dominant_height_dm^3)/10000)+
      +0.5985*((dominant_height_dm^2)/100)+
      -1.3719*lichen_type),
      "Multiple Coefficient of Correlation"= 0.72,
      "Spread about the function, m2/ha"=4.5
    )
    )
  } else if(region_code==3){

    herb_or_bilberry_type <- ifelse(vegetation%in%c(1:6,13),1,0)

    return(list(
    "Basal area m^2/ha"=
      (-6.8938+
         +0.2321*dominant_height_dm+
         +0.006336*((SIdm^2)/100)+
         +1.7985*herb_or_bilberry_type
      ),
    "Multiple Coefficient of Correlation"= 0.75,
    "Spread about the function, m2/ha"=5.3

    )
    )
  } else if(region_code%in%c(4,5)){

    herb_or_bilberry_type <- ifelse(vegetation%in%c(1:6,13),1,0)

    return(list(
      "Basal area m^2/ha"=(
        -0.05756+
          +0.2199*dominant_height_dm+
          -0.01812*altitude_m+
          +1.3883*herb_or_bilberry_type
      ),
      "Multiple Coefficient of Correlation"=0.71 ,
      "Spread about the function, m2/ha"=5.6

    )
    )
  }
  } else if(species=="Picea abies"){

    if(SI>=22){
      return(list(
        "Basal area m^2/ha"=(
          -15.6779+
            -0.08294*dominant_height_dm*SIdm/100+
            +0.4857*dominant_height_dm+
            +0.004742*((SIdm^3)/10000)
        )

          ,
        "Multiple Coefficient of Correlation"=0.75 ,
        "Spread about the function, m2/ha"=6.6

      )
      )
    } else if(SI<22){

      herb_or_bilberry_type <- ifelse(vegetation%in%c(1:6,13),1,0)

      return(
        list(
          "Basal area m^2/ha"=(
            4.0184+
              +0.1696*((dominant_height_dm^2)/100)+
              -0.004785*altitude_m+
              +1.1485*herb_or_bilberry_type
          ),
          "Multiple Coefficient of Correlation"=0.77 ,
          "Spread about the function, m2/ha"=5.6


        )
      )

    }

    }



}
