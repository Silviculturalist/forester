
#' Pettersson Structural Values related to Phi.
#' @details This is used in Henrik Pettersson growth and yield model to
#' describe stem distribution.
#'
#' @source Pettersson, H. 1955. Barrskogens volymproduktion (Die
#' Massenproduktion des Nadelwaldes). Reports of the forest research institute
#' of Sweden. Vol 45:1. Centraltryckeriet Esselte AB. Stockholm. Available
#' Online (07/08/2022): \href{PUB EPSILON SLU}{https://pub.epsilon.slu.se/9982/1/medd_statens_skogsforskningsinst_045_01_a.pdf}
#'
#' @param Phi Distance from the normal distributions upper boundary (cut-off),
#' at mean + 3 sigma.
#'
#' @description #Compare to table on p. 325.
#'
#' @return A list of values.
#' @export
PhiTable <- function(Phi){
  i <- function(phi) 3-phi

  A <- function(x) 1/sqrt(2*pi)*exp(-((abs(x)^2)/2))
  B <- function(x) (1/sqrt(2*pi))*integrate(f=function(t) exp(-(t^2)/2),
                                            lower = -Inf,abs(x))$value
  C <- function(x) ifelse(x<0,(1-B(x)),B(x))
  R <- A(3)
  S <- C(3)
  S-C(-3) #Area under distribution.

  #Area from top of distribution to phi.
  BigFPhi <- function(phi){
    return(
      (S-C(i(phi)))/(S-C(-3))
    )
  }


  #M 7.4.3. works!
  M <-function(Phi) (A(i(Phi))-R)/(S-C(i(Phi)))
  Mprim <- function(Phi) M(Phi)-i(Phi)

  #M 7.4.5. works.
  vPrim2 <- function(Phi) (i(Phi)*A(i(Phi))-3*R + S - C(i(Phi)))/(S-C(i(Phi)))

  #M 7.4.6 works..
  SigmaPrim <- function(Phi) sqrt(vPrim2(Phi) - M(Phi)^2)

  #Works.
  MprimBYSigmaPrim <- function(Phi) Mprim(Phi)/SigmaPrim(Phi)

  return(
    list(
      i = i(Phi),
      Fphi = BigFPhi(Phi),
      Mprim = Mprim(Phi),
      SigmaPrim = SigmaPrim(Phi),
      MprimBYSigmaPrim=MprimBYSigmaPrim(Phi)
    )
  )
}


## Stem quota through low-thinning
# uPrim is the quota by which we pull the left-most end of the normal distribution to the right.

stemquota <- function(uPrim){
  return(
    uPrim*exp(-(4.5*(1-uPrim))/(1+uPrim))
  )
}

#Abbreviations Definitions at p. 229.

#Effect of low thinning on total number of stems.. 12.3.3.
#Phi1 and Phi2 are the parts of the distribution to the right of i - which is set to be equal.
#Phi2 then becomes Phi1/uPrim. ... 12.2.3 p.93.
#PsiPrim is the through-thinning (genomgallring..) strength.
lowThinningStemQuota <- function(uPrim, Phi1, PsiPrim){

  stemquote <- stemquota(uPrim)

  BigFPhi1 <- PhiTable(Phi1)$Fphi

  Phi2 <- Phi1/uPrim

  BigFPhi2 <- PhiTable(Phi2)$Fphi
  return(
    stemquote*(BigFPhi2/BigFPhi1)*PsiPrim
  )
}


