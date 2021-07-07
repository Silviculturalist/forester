#' Stem Count for Stand Initiation from Hägglund (1981)
#'
#' @source Hägglund, Björn (1981) Samband mellan ståndortsindex H100 och bonitet för
#' tall och gran i Sverige. Projekt HUGIN. Rapport 26. Skogsvetenskapliga fakultet.
#' Sveriges Lantbruksuniversitet. Umeå.
#'
#' @param SI Site Index H100 for species.
#' @param altitude_m Meters above sea level.
#' @param vegetation 1-18
#' @param species "Picea abies" or "Pinus sylvestris".
#' @param dominant_height_m Dominant height of stand, meters.
#' @param region_code 1-5. 1: Lappmarken, 2: Norr-& Västerbottens kustland & Jämtland, Kopparberg - Sälen-Idre.
#' 3: Västernorrland, Gävleborg, Kopparberg -övrigt , Värmland.
#' 5: Gotland, Skåne, Halland, Blekinge, Göteborg & Bohus.
#'
#' @return list of 3 elements: 1. Stem Count/ha.
#' 2. Multiple Coefficient of Correlation for used function.
#' 3. Spread about the function, m^2/ha.
#' @export
#'
#' @examples
Hagglund_1981_initial_stem_count <- function(
  SI,
  altitude_m,
  vegetation,
  species,
  dominant_height_m,
  region_code
){
  dominant_height_dm <- dominant_height_m*10
  SIdm <- SI*10

  if(species=="Pinus sylvestris"){
    if(region_code==1){

      basal_area <- Hagglund_1981_initial_basal_area(species="Pinus sylvestris",
                                                     region_code=region_code,
                                                     dominant_height_m = dominant_height_m,
                                                     altitude_m = altitude_m,
                                                     SI = SI,
                                                     vegetation = vegetation
                                                     )[[1]]

      stem_count <- 560.27+
        +80.6313*basal_area+
        -6.7352*(dominant_height_dm^3/10000)+
        -0.9525*altitude_m+
        +0.6203*(SIdm^2)/100

      return(
        list(
          "Stem Count"=stem_count,
          "Multiple Coefficient of Correlation"=0.73,
          "Spread about the function, n/ha"=367
        )
      )


    } else if(region_code==2){

      basal_area <- Hagglund_1981_initial_basal_area(species="Pinus sylvestris",
                                                     region_code=region_code,
                                                     dominant_height_m = dominant_height_m,
                                                     altitude_m = altitude_m,
                                                     SI = SI,
                                                     vegetation = vegetation
      )[[1]]

      stem_count <- 654.03+
        +100.2952*basal_area+
        -9.3082*dominant_height_m


        return(
          list(
            "Stem Count"=stem_count,
            "Multiple Coefficient of Correlation"=0.83,
            "Spread about the function, n/ha"=368
          )
        )

    } else if(region_code==3){

      basal_area <- Hagglund_1981_initial_basal_area(species="Pinus sylvestris",
                                                     region_code=region_code,
                                                     dominant_height_m = dominant_height_m,
                                                     altitude_m = altitude_m,
                                                     SI = SI,
                                                     vegetation = vegetation
      )[[1]]

      stem_count <- 385.70+
        +104.0408*basal_area+
        -13.5350*((dominant_height_dm^2)/100)+
        +1.8867*dominant_height_dm*SIdm/100

      return(
        list(
          "Stem Count"=stem_count,
          "Multiple Coefficient of Correlation"=0.80,
          "Spread about the function, n/ha"=492
        )
      )

    } else if(region_code%in%c(4,5)){

      basal_area <- Hagglund_1981_initial_basal_area(species="Pinus sylvestris",
                                                     region_code=region_code,
                                                     dominant_height_m = dominant_height_m,
                                                     altitude_m = altitude_m,
                                                     SI = SI,
                                                     vegetation = vegetation
      )[[1]]

      stem_count <- 164.78+
        +87.4356*basal_area+
        -0.1386*((dominant_height_dm^2)/100)+
        +0.3207*((SIdm^3)/10000)+
        -4.7445*(dominant_height_dm^2)*SIdm/10000+
        +0.6978*altitude_m

      return(
        list(
          "Stem Count"=stem_count,
          "Multiple Coefficient of Correlation"=0.76,
          "Spread about the function, n/ha"=450
        )
      )

    }

  } else if(species=="Picea abies"){

    if(SI<22){

    basal_area <- Hagglund_1981_initial_basal_area(species="Pinus sylvestris",
                                                   region_code=region_code,
                                                   dominant_height_m = dominant_height_m,
                                                   altitude_m = altitude_m,
                                                   SI = SI,
                                                   vegetation = vegetation
    )[[1]]

    stem_count <- 346.43+
      +99.2581*basal_area+
      -6.5267*((dominant_height_dm^3)/10000)

    return(
      list(
        "Stem Count"=stem_count,
        "Multiple Coefficient of Correlation"=0.80,
        "Spread about the function, n/ha"=491
      )
    )



    } else if(SI>=22){

      basal_area <- Hagglund_1981_initial_basal_area(species="Pinus sylvestris",
                                                     region_code=region_code,
                                                     dominant_height_m = dominant_height_m,
                                                     altitude_m = altitude_m,
                                                     SI = SI,
                                                     vegetation = vegetation
      )[[1]]

      stem_count <- -826.15+
        +107.7125*basal_area+
        +0.3578*((dominant_height_dm^2)/100)+
        +5.4616*SIdm+
        -4.1920*((dominant_height_dm^2)*(SIdm/10000))


      return(
        list(
          "Stem Count"=stem_count,
          "Multiple Coefficient of Correlation"=0.82,
          "Spread about the function, n/ha"=571
        )
      )


    }
  }

}
