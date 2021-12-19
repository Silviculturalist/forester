#' Diameter at a height or height from diameter for Norway Spruce in Norway.
#'
#' @description Uses Kozak (1997) stem taper function (function 1995 in original publication) to describe the diameter of Norway Spruce in Norway.
#'
#' @source Pukkala, T., Holt Hanssen, K., Andreassen, K. (2019) Stem taper and bark functions for Norway spruce in Norway. Silva Fennica vol. 53 no. 3 article id 10187. \url{https://doi.org/10.14214/sf.10187}
#'
#' @details This function has two functions. i) If output is 'diameter', to find the diameter of a tree at the height 'get_d_at_height_m'. ii) If output is 'height', to find the height at which the tree is 'get_h_at_diameter_cm'.
#' Will return NA if the desired height is below 0 m or above the height of the tree.
#' Will return NA if the desired diameter is larger than the expected diameter at ground level (height==0) or the desired diameter is below 0.
#'
#' The function will, if no age or SIH40 is supplied, use the 'baseline' parameters from the source.
#' If age or SIH40 is supplied, it will prefer the parameters from each of these (table 3, 4 in publication).
#' If both age and SIH40 are supplied, function will prefer the site-index dependent fixed effects (table 3).
#'
#' @param height_m Height of tree in metres.
#' @param dbh_cm Diameter of tree at breast height.
#' @param get_d_at_height_m If output is 'diameter', the height at which diameter is desired. Otherwise NA.
#' @param get_h_at_diameter_cm  If output is 'height', the diameter at which height is desired. Otherwise NA.
#' @param age Default (NA). Otherwise (total?) age of tree.
#' @param SIH40 Default (NA). Otherwise Site Index H40 for Norway Spruce, e.g. [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]
#' @param output One of: "diameter" (default) or "height".
#'
#' @return If output is 'diameter', the diameter at height get_d_at_height_m; if output is 'height', the height when the tree will have the diameter 'get_h_at_diameter_cm'.
#' @export
#'
#' @examples
#' #Get the diameter at ground level for a Spruce with dbh==18 cm and height==14 m.
#' Pukkala_2019_form_quotient_Norway_Norway_Spruce(height_m = 14,dbh_cm = 18,output = "diameter",get_d_at_height_m = 0)

Pukkala_2019_form_quotient_Norway_Norway_Spruce <- function(height_m,
                                                            dbh_cm,
                                                            get_d_at_height_m = NA,
                                                            get_h_at_diameter_cm=NA,
                                                            age = NA,
                                                            SIH40 = NA,
                                                            output = "diameter") {

  #Height must be zero or larger.
  stopifnot(height_m>=0)

  #Desired height must not be taller than tree, and must not be smaller than 0.
  if(!(is.na(get_d_at_height_m)) & get_d_at_height_m > height_m | !(is.na(get_d_at_height_m)) & get_d_at_height_m<0){
    warning("Desired height is taller than tree height or less than 0. Returning 'NA'.")
    return(
      NA
    )
  }

  #Desired diameter must be zero or above.
  if(!is.na(get_h_at_diameter_cm) & get_h_at_diameter_cm<0){
    warning("Desired diameter must be zero or above. Returning 'NA'.")
    return(
      NA
    )
  }

    #Kozak 97
  #diameter at height..

  Kozak_1997 <-
    function(a0,
             a1,
             a2,
             b1,
             b2,
             b3,
             b4,
             b5,
             height_m,
             get_d_at_height_m,
             dbh_cm) {
      q <- get_d_at_height_m / height_m
      t <- 1.3 / height_m
      Y <- (1 - (q^0.5)) / (1 - (t^0.5))

      return(a0 * (dbh_cm ^ (a1)) * (height_m ^ (a2)) * Y ^ (
        b1 * (Y ^ 0.1) + b2 * (q ^ 4) + b3 * asin(sqrt(1 - sqrt(q))) + (b4 / exp(dbh_cm /
                                                                                   H)) + b5 * dbh_cm ^ Y
      ))
    }

  #Get appropriate coefficients
  if (is.na(age) & is.na(SIH40)) {
    #baseline
    coef_table <- list(
      a0 = 0.8795844,
      a1 = 0.9457765,
      a2 = 0.1061692,
      b1 = 0.6624965,
      b2 = 0.3781478,
      b3 = -0.4418430,
      b4 = -0.5154988,
      b5 = 0.0197963
    )
  }

  if (is.na(age) & !is.na(SIH40) | !is.na(age) & !is.na(SIH40)) {
    #SI dependent
    coef_table <- list(
      a0 = 1.0233871+-0.0074925 * SIH40,
      a1 = 1.1012260+-0.0084932 * SIH40,
      a2 = -0.1160389 + 0.0119276 * SIH40,
      b1 = 0.9717245+-0.0161964 * SIH40,
      b2 = 0.0787199 + 0.0153552 * SIH40,
      b3 = -1.4247046 + 0.0488699 * SIH40,
      b4 = -0.7021483 + 0.0105041 * SIH40,
      b5 = 0.0486118+-0.0014326 * SIH40
    )
  }

  #Age dependent.
  if (!is.na(age) & is.na(SIH40)) {
    coef_table <- list(
      a0 = 0.8294 + 0.0006427 * age,
      a1 = 0.8600 + 0.0008741 * age,
      a2 = 0.2179+-0.001193 * age,
      b1 = 0.5449 + 0.001253 * age,
      b2 = 0.5581+-0.001806 * age,
      b3 = -0.07002+-0.003486 * age,
      b4 = -0.4296+-0.001174 * age,
      b5 = (9.725E-03) + (8.524E-05) * age
    )
  }

  ### Output diameter.
  if (output == "diameter") {

    return(
      Kozak_1997(
        a0 = coef_table[["a0"]],
        a1 = coef_table[["a1"]],
        a2 = coef_table[["a2"]],
        b1 = coef_table[["b1"]],
        b2 = coef_table[["b2"]],
        b3 = coef_table[["b3"]],
        b4 = coef_table[["b4"]],
        b5 = coef_table[["b5"]],
        height_m = height_m,
        dbh_cm = dbh_cm,
        get_d_at_height_m = get_d_at_height_m
      )
    )
  }


  if (output == "height") {

    #Desired diameter must be less or equal to that at height=0.
    max_diameter <- Kozak_1997(
      a0 = coef_table[["a0"]],
      a1 = coef_table[["a1"]],
      a2 = coef_table[["a2"]],
      b1 = coef_table[["b1"]],
      b2 = coef_table[["b2"]],
      b3 = coef_table[["b3"]],
      b4 = coef_table[["b4"]],
      b5 = coef_table[["b5"]],
      height_m = height_m,
      dbh_cm = dbh_cm,
      get_d_at_height_m = 0
    )

    if(get_h_at_diameter_cm>max_diameter){
      warning("Desired diameter is larger than that at the ground. Returning 'NA'.")
      return(
        NA
      )
    }





    #Calculated diameter - desired diameter
    height_f <- function(get_d_at_height_m = get_d_at_height_m,
                         get_h_at_diameter_cm = get_h_at_diameter_cm,
                         a0 = coef_table[["a0"]],
                         a1 = coef_table[["a1"]],
                         a2 = coef_table[["a2"]],
                         b1 = coef_table[["b1"]],
                         b2 = coef_table[["b2"]],
                         b3 = coef_table[["b3"]],
                         b4 = coef_table[["b4"]],
                         b5 = coef_table[["b5"]],
                         height_m = height_m,
                         dbh_cm = dbh_cm
                         ){
      #Get diameter
      diameter_h <- Kozak_1997(
        a0 = coef_table[["a0"]],
        a1 = coef_table[["a1"]],
        a2 = coef_table[["a2"]],
        b1 = coef_table[["b1"]],
        b2 = coef_table[["b2"]],
        b3 = coef_table[["b3"]],
        b4 = coef_table[["b4"]],
        b5 = coef_table[["b5"]],
        height_m = height_m,
        dbh_cm = dbh_cm,
        get_d_at_height_m = get_d_at_height_m
      )

      #Compare to desired diameter.
      return(
        get_h_at_diameter_cm-diameter_h
      )

    }

    #Find when difference is 0.
    return(
      uniroot(
      f = height_f,
      lower = 0,
      upper = height_m,
      a0 = coef_table[["a0"]],
      a1 = coef_table[["a1"]],
      a2 = coef_table[["a2"]],
      b1 = coef_table[["b1"]],
      b2 = coef_table[["b2"]],
      b3 = coef_table[["b3"]],
      b4 = coef_table[["b4"]],
      b5 = coef_table[["b5"]],
      dbh_cm = dbh_cm,
      height_m=height_m,
      get_h_at_diameter_cm = get_h_at_diameter_cm
    )[[1]]
    )

  }


}

Pukkala_2019_form_quotient_Norway_Norway_Spruce <-
  Vectorize(Pukkala_2019_form_quotient_Norway_Norway_Spruce)

