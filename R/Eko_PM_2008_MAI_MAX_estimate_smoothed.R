#' MAI max as estimated by smoothing function in Ekö PM et al 2008.
#'
#' @source Ekö, P-M, Johansson, U., Petersson, N. Bergqvist, J., Elfving, B.,
#' Frisk, J. (2008) Current growth differences of Norway spruce (Picea abies),
#' Scots Pine (Pinus sylvestris) and birch (Betula pendula and Betula pubescens)
#' in different regions in Sweden. Scandinavian Journal of Forest Research Vol.
#' 23:4. p. 307-318. DOI: \url{https://doi.org/10.1080/02827580802249126}
#'
#' @param species One of : "Betula pendula", "Betula pubescens","Picea abies",
#' "Pinus sylvestris"
#' @param SI Site Index. \strong{OBSERVE: H100 for Picea abies,
#' H100 for Pinus sylvestris,
#' H50 for Betula spp.}
#'
#' For Picea abies northern Sweden
#' [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce()].
#' For Picea abies southern Sweden e.g.
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce()].
#' For Pinus sylvestris e.g.
#' [forester::Hagglund_1974_Sweden_height_trajectories_Pine()].
#' For Betula pendula / Betula pubescens e.g.
#' [forester::Eriksson_1997_height_trajectory_Sweden_Birch()].
#' If SIH not available, use Site factors: e.g. [forester::SIS_estimate],
#' for Birch: [forester::Eko_PM_2008_stand_factors_estimate_SI_Sweden_Birch()]
#' @param county Swedish county, e.g. [forester::county_sweden()]
#' @param altitude Meters above sea level.
#' @param vegetation Vegetation type according to code following Swedish
#' National forest inventory FALTSKIKT:
#' \tabular{ll}{
#' Code \tab Vegetation \cr
#' 1 \tab  Rich-herb without shrubs \cr
#' 2 \tab Rich-herb with shrubs/bilberry \cr
#' 3 \tab Rich-herb with shrubs/lingonberry \cr
#' 4 \tab Low-herb without shrubs \cr
#' 5 \tab Low-herb with shrubs/bilberry \cr
#' 6 \tab Low-herb with shrubs/lingonberry \cr
#' 7 \tab No field layer \cr
#' 8 \tab Broadleaved grass \cr
#' 9 \tab Thinleaved grass \cr
#' 10 \tab Sedge, high \cr
#' 11 \tab Sedge, low \cr
#' 12 \tab Horsetail, Equisetum ssp. \cr
#' 13 \tab Bilberry \cr
#' 14 \tab Lingonberry \cr
#' 15 \tab Crowberry \cr
#' 16 \tab Poor shrub \cr
#' 17 \tab Lichen, frequent occurrence \cr
#' 18 \tab Lichen, dominating \cr
#' }
#'
#' @return Numeric. MAI max.
#' @export
#'
#' @examples
#' Eko_PM_2008_MAI_MAX_estimate_smoothed(species="Picea abies",SI=36county="Kronoberg",altitude=100,vegetation=4)
Eko_PM_2008_MAI_MAX_estimate_smoothed <- function(species,
                                                  SI,
                                                  county,
                                                  altitude,
                                                  vegetation) {


  warning("This function will not stop you from entering unrealistic combinations of SI, vegetation, species or altitude! E.g. Inflexion point visible for Birch at low SI. ")
  stopifnot(
    species %in% c(
      "Picea abies",
      "Pinus sylvestris",
      "Betula pendula",
      "Betula pubescens"
    )
  )

  northern <- ifelse(
    county %in% c(
      "Norrbottens lappmark",
      "Norrbottens kustland",
      "Västerbottens lappmark",
      "Västerbottens kustland",
      "Jämtland - Jämtlands landskap",
      "Jämtland - Härjedalens landskap",
      "Västernorrland - Ångermanlands landskap",
      "Västernorrland - Medelpads landskap",
      "Kopparberg - Sälen-Idre"
    ),
    1,
    0
  )

  southern <- ifelse(
    county %in% c(
      "Stockholm",
      "Uppsala",
      "Västmanland",
      "Södermanland",
      "Örebro",
      "Östergötland",
      "Skaraborg",
      "Älvsborg - Västergötlands landskap",
      "Älvsborg -  Dalslands landskap",
      "Jönköping",
      "Kronoberg",
      "Kalmar",
      "Halland",
      "Kristianstad",
      "Malmöhus",
      "Blekinge",
      "Gotland",
      "Västra Götaland"
    ),
    1,
    0
  )

  central <- ifelse(
    county %in% c(
      "Kopparberg - övriga",
      "Gävleborg - Hälsinglands landskap",
      "Gävleborg - övriga",
      "Värmland"
    ),
    1,
    0
  )

  if (central == 0 & northern == 0 & southern == 0) {
    stop("Swedish county not found. Check spelling / position")
  }


  #Interpret 'poor field vegetation' as opposite of herbs, grasses, without field layer.

  c <-
    ifelse(
      species == "Betula pendula" | species == "Betula pubescens",
      1.70,
      ifelse(
        species == "Pinus sylvestris" &
          northern == 1 &
          altitude > 200 | species == "Pinus sylvestris" & vegetation > 9,
        0.385,
        ifelse(
          species == "Pinus sylvestris",
          0.127,
          ifelse(
            species == "Picea abies" & northern == 1 & vegetation < 10,
            -0.411,
            ifelse(
              species == "Picea abies" & northern == 1,
              -0.396,
              ifelse(
                species == "Picea abies" & central == 1 & vegetation < 10,
                -0.118,
                ifelse(
                  species == "Picea abies" & central == 1,
                  0.250,
                  ifelse(species == "Picea abies" &
                           southern == 1,
                         0.351,
                         stop("Species / Site not found"))
                )
              )
            )
          )
        )

      )
    )

  b1 <-
    ifelse(
      species == "Betula pendula" | species == "Betula pubescens",
      -0.269,
      ifelse(
        species == "Pinus sylvestris" &
          northern == 1 &
          altitude > 200 |
          species == "Pinus sylvestris" & vegetation > 9,
        -0.0122,
        ifelse(
          species == "Pinus sylvestris",
          0.0496,
          ifelse(
            species == "Picea abies" & northern == 1 & vegetation < 10,
            0.140,
            ifelse(
              species == "Picea abies" & northern == 1,
              0.147,
              ifelse(
                species == "Picea abies" & central == 1 & vegetation < 10,
                0.138,
                ifelse(
                  species == "Picea abies" & central == 1,
                  0.0460,
                  ifelse(species == "Picea abies" &
                           southern == 1,
                         0.0692,
                         stop("Species / Site not found"))
                )
              )
            )
          )
        )

      )
    )

  b2 <-
    ifelse(
      species == "Betula pendula" | species == "Betula pubescens",
      0.0196,
      ifelse(
        species == "Pinus sylvestris" &
          northern == 1 &
          altitude > 200 | species == "Pinus sylvestris" & vegetation > 9,
        0.00890,
        ifelse(
          species == "Pinus sylvestris",
          0.00796,
          ifelse(
            species == "Picea abies" & northern == 1 & vegetation < 10,
            0.00455,
            ifelse(
              species == "Picea abies" & northern == 1,
              0.00313,
              ifelse(
                species == "Picea abies" & central == 1 & vegetation < 10,
                0.00593,
                ifelse(
                  species == "Picea abies" & central == 1,
                  0.00824,
                  ifelse(species == "Picea abies" &
                           southern == 1,
                         0.00854,
                         stop("Species / Site not found"))
                )
              )
            )
          )
        )

      )
    )

  return(c + (b1 * SI) + (b2 * (SI ^ 2)))

}
