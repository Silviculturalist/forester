#' Regional temperature and radiation indices
#'
#' @source Morén, Ann-Sofie; Perttu, Kurth L. (1994). Regional temperature and radiation indices and their adjustment to horizontal and inclined forest land. Studia Forestalia Suecica 194. 19 pp. ISSN 0039-3150. ISBN 91-576-4915-4. Url:https://pub.epsilon.slu.se/3910/1/SFS194.pdf
#'
#' @param Latitude Degrees North.
#' @param Altitude Meters above sea level
#' @param Correction Boolean. Apply Continental, Maritime Correction to temperature sum?
#' @param TmaxMonthly In case of correction. Maximum monthly mean temperature.
#' @param TminMonthly In case of correction. Minimum monthly mean temperature.
#' @name Moren1994
#' @return Temperature Sum exceeding 5 degrees Celsius.
#' @export
Moren_Perttu_1994_Sweden_temperature_sum_5C <- function(
  Latitude,
  Altitude,
  Correction=FALSE,
  TmaxMonthly=NULL,
  TminMonthly=NULL
){

  if(Altitude>1500){
    warning("Altitude set to maximum 1500 m")
    Altitude=1500
  }

  ifelse(Altitude<1000,
         assign(x = 'Tsum',4922-60.4*Latitude-0.837*Altitude),
         assign(x = 'Tsum',3635.3 - 12.18*Latitude - 0.444*Latitude^2 + 0.04041*Latitude*Altitude-3.343*Altitude-0.000040*Altitude^2)
  )

  if(Correction & !is.null(TmaxMonthly) & !is.null(TminMonthly)){
    #p. 7, eq. 10. Gorczynski
    C = 1.7*(Tmax-Tmin)/sin(Latitude)-20.4

    Corr = ifelse(
      C<12.5,-100,
      ifelse(
        C<18.5,-50,
        ifelse(
          C<27.5,0,
          ifelse(
            C<=33.5,50,100
          )
        )
      )
    )

    Tsum = Tsum+Corr

  }


  return(Tsum)



}


#' @rdname Moren1994
#' @return Length of growing season (> 5 degrees Celsius), days.
#' @export

Moren_Perttu_1994_Sweden_growing_season_length_5C <- function(
  Latitude,
  Altitude
  ){
    if(Altitude>1500){
      warning("Altitude set to maximum 1500 m")
      Altitude=1500
    }

  ifelse(Altitude<1000,
    return(597.6-6.823*Latitude-0.225*Altitude + 0.00268*Latitude*Altitude),
    return(474.5 - 2.601*Latitude-0.036*Latitude^2 + 0.00376*Latitude*Altitude - 0.275*Altitude-0.000029*Altitude^2)
  )
}


#' @param returnDate Return as a date?
#'
#' @return Date or numeric.
#' @export
#' @rdname Moren1994

Moren_Perttu_1994_Sweden_start_of_growing_season_5C <- function(
  Latitude,
  Altitude,
  returnDate=TRUE
){

  start = -35.760+2.489*Latitude-0.0637*Altitude+0.00132*Latitude*Altitude

  ifelse(returnDate==TRUE,
         return(as.Date(0)+start),
         return(start)
         )
}


#' @return Global Radiation Sum.
#' @rdname Moren1994
#' @export
Moren_Perttu_1994_Sweden_global_radiation_sum <- function(
  Latitude,
  Altitude
){
  return(7.2774-0.0782*Latitude-0.00072*Altitude)
}



