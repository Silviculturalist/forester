#' Larix sibirica height trajectory in Finland.
#' @author Vuokila, Y., Gustavsen, H-G, Luoma, P. 1983.
#' Siperianlehtikuusikoiden kasvupaikkojen luokittelu ja harvennusmallit.
#' Folia Forestalia 554. s. 13. Metsäntutkimuslaitos. url: \url{URL}{http://urn.fi/URN:ISBN:951-40-0614-3}
#'
#' @description Included functions are reparametrised height trajectory function (GADA 2005),
#' BAI, VAI. For original height increment function and reparametrisation process see the example.
#'
#' @param height dominant height, in meters.
#' @param age total age.
#' @param age2 total age at requested time point.
#'
#' @return dominant height at total age== age2.
#' @export
#'
#' @examples
#' #Reparametrisation of Vuokila 1983 Larch Height Increment function to GADA 2005.
#'
#' Vuokila_1983_height_trajectory<-function(height,age,age2)
#' {
#'   Vuokila_1983_height_increment_Finland_Larix_sibirica <- function(height,age)
#'   {
#'     return(exp(1.874-1.00350*log(age)+1.10264*log(height)-0.0411495*height))
#'   }
#'
#'   Vuokila_1983_height_increment_Finland_Larix_sibirica<-Vectorize(Vuokila_1983_height_increment_Finland_Larix_sibirica)
#'
#'   FindPreviousHeight <- function(age,height)
#'   {
#'     return(
#'       optimise(f=function(x){return(((x+Vuokila_1983_height_increment_Finland_Larix_sibirica(height=x,age=(age-5)))-height)^2)},interval=c(0.0001,height))[[1]]
#'     )
#'   }
#'
#'   FindPreviousHeight <- Vectorize(FindPreviousHeight)
#'
#'   #Get number of steps
#'   ageDiff = (age2-age)/5
#'   height2=height
#'   tempAge = age
#'   if(ageDiff!=0)
#'   {
#'     if(ageDiff<0){
#'       for(i in -1:ageDiff)
#'       {
#'         height2= FindPreviousHeight(tempAge,height2)
#'         tempAge = tempAge-5
#'       }
#'     }
#'
#'     if(ageDiff>0){
#'       for(i in 1:ageDiff)
#'       {
#'         height2 = height2+Vuokila_1983_height_increment_Finland_Larix_sibirica(height2,tempAge)
#'         tempAge= tempAge+5
#'       }
#'     }
#'   }
#'
#'   return(height2)
#' }
#'
#' Vuokila_1983_height_trajectory  <- Vectorize(Vuokila_1983_height_trajectory)
#'
#' plot(NULL,
#'      xlim=c(0,120),
#'      ylim=c(0,40)
#'      )
#' for(i in 1:length(seq(8,36,2))){
#'   lines(seq(5,120,5),Vuokila_1983_height_trajectory(height=seq(8,36,2)[[i]],age=100,age2=seq(5,120,5)))
#' }
#'
#' plot(NULL,
#'      xlim=c(10,90),
#'      ylim=c(0,0.6)
#' )
#' for(i in 1:length(seq(24,36,2))){
#'   lines(seq(10,90,5),(Vuokila_1983_height_trajectory(seq(24,36,2)[[i]],age = 100,age2 = seq(10,90,5))-Vuokila_1983_height_trajectory(seq(24,36,2)[[i]],age = 100,age2 = seq(5,85,5)))/5)
#' }
#'
#'
#' ## GADA2005 implementation
#' library(tidyverse)
#'
#' LarixGrid <-data.frame(expand.grid(H1=seq(8,40,2),A1=100,A2=seq(5,100,5))) %>%
#'   mutate(
#'     H2=Vuokila_1983_height_trajectory(height =H1,age=A1,age2=A2)
#'   )
#'
#' LarixZero2 <- data.frame(expand.grid(H1=runif(min=8,max=32,n=200),A1=runif(min=20,110,n=200),A2=0,H2=0))
#' LarixZero <- LarixGrid
#' LarixZero$A2 <- 0
#' LarixZero$H2 <- 0
#'
#' LarixGrid3 <- dplyr::bind_rows(LarixGrid,LarixZero)
#' LarixGrid2 <- dplyr::bind_rows(LarixGrid3,LarixZero2)
#'
#' #Model
#' #B = (b-((age1^j)/H1))
#' #R = sqrt(((b-((age1^j)/H1))^2)-2*a(c+age1^j))
#' GADA=H2~((A2/A1)^j*H1*(b+sqrt(((b-((A1^j)/H1))^2)-2*a*(c+A1^j)))-A1^j)/(H1*a*(1-(A2/A1)^j)+((b-((A1^j)/H1)))+sqrt(((b-((A1^j)/H1))^2)-2*a*(c+A1^j)))
#'
#' #Model 2..
#'
#' #Brute force
#' mod1<-minpack.lm::nlsLM(
#'   GADA,
#'   start =c(a=-32,b=600,c=20000,j=1.14),
#'   lower = c(a=-50000,b=0,c=0.0001,j=0.0001),
#'   upper = c(a=4, b=1E6,c=2E7,j=5),
#'   data=LarixGrid,
#'   trace=TRUE,
#'   control=minpack.lm::nls.lm.control(maxiter = 10000),
#'   algorithm = "grid-search"
#'   )
#'
#' mod2<-minpack.lm::nlsLM(
#'   GADA,
#'   start =c(a=-32,b=600,c=20000,j=1.14),
#'   lower = c(a=-50000,b=0,c=0.0001,j=0.0001),
#'   upper = c(a=4, b=1E6,c=2E7,j=5),
#'   data=LarixGrid2,
#'   trace=TRUE,
#'   control=minpack.lm::nls.lm.control(maxiter = 10000),
#'   algorithm = "grid-search"
#' )
#'
#' mod3<-minpack.lm::nlsLM(
#'   GADA,
#'   start =c(a=-32,b=600,c=20000,j=1.14),
#'   lower = c(a=-50000,b=0,c=0.0001,j=0.0001),
#'   upper = c(a=4, b=1E6,c=2E7,j=5),
#'   data=LarixGrid3,
#'   trace=TRUE,
#'   control=minpack.lm::nls.lm.control(maxiter = 10000),
#'   algorithm = "grid-search"
#' )
#'
#' #Refit model 3 to original data.
#' mod32<-minpack.lm::nlsLM(
#'   GADA,
#'   start =coef(mod3),
#'   lower = c(a=-50000,b=0,c=0.0001,j=0.0001),
#'   upper = c(a=4, b=1E6,c=2E7,j=5),
#'   data=LarixGrid,
#'   trace=TRUE,
#'   control=minpack.lm::nls.lm.control(maxiter = 10000),
#'   algorithm = "grid-search"
#' )
#'
#' #Look at fit
#' plot(LarixGrid$A2,
#'      LarixGrid$H2,
#'      xlim=c(0,100),
#'      ylim=c(0,40),
#'      pch=16,
#'      cex=0.5
#' )
#' for(i in 1:length(seq(8,40,2))){
#'   #lines(seq(5,110,5),Vuokila_1983_height_trajectory(height=seq(8,40,2)[[i]],age=100,age2=seq(5,110,5)),col="blue")
#'   #lines(seq(0,110,0.1), predict(mod1,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="red")
#'   lines(seq(0,110,0.1), predict(mod3,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="blue",lty=2)
#'   #lines(seq(0,110,0.1), predict(mod32,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="green",lty=3)
#' }
#'
#'
#'
#' #Look at maximum for asympote behavior.
#' plot(LarixGrid$A2,
#'      LarixGrid$H2,
#'      xlim=c(0,500),
#'      ylim=c(0,55),
#'      pch=16,
#'      cex=0.5
#' )
#' for(i in 1:length(seq(8,40,2))){
#'   lines(seq(5,500,5),Vuokila_1983_height_trajectory(height=seq(8,40,2)[[i]],age=100,age2=seq(5,500,5)),col="blue")
#'   lines(seq(0,500,0.1), predict(mod1,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,500,0.1))),col="red")
#'   lines(seq(0,110,0.1), predict(mod3,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="blue",lty=2)
#'   #lines(seq(0,110,0.1), predict(mod32,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="green",lty=3)
#' }
#'
#'
#'
#' #Inspect the zero.
#' #Model 32 with both original data with age2 replaced with zero and h2==0 +
#' #random combinations to age2=0 gives best fit at lower values for original data.
#' #i.e. closest to origin. Still, maximum RSS is naïve model 1.
#' plot(NULL,
#'      xlim=c(-0.1,1.5),
#'      ylim=c(-0.2,0.1)
#' )
#' for(i in 1:length(seq(8,36,2))){
#'   #lines(seq(5,250,5),Vuokila_1983_height_trajectory(height=seq(8,40,2)[[i]],age=100,age2=seq(5,250,5)),col="blue")
#'   lines(seq(0,250,0.1), predict(mod1,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,250,0.1))),col="red")
#'   lines(seq(0,110,0.1), predict(mod3,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="blue",lty=2)
#'   lines(seq(0,110,0.1), predict(mod32,list(H1=seq(8,40,2)[[i]],A1=100,A2=seq(0,110,0.1))),col="green",lty=3)
#' }
#' points(0,0,pch=3)
#'
#'
#' plot(residuals(mod1)[1:nrow(LarixGrid)])
#' plot(residuals(mod3)[1:nrow(LarixGrid)])
#' plot(residuals(mod32)[1:nrow(LarixGrid)])
Vuokila_1983_height_trajectory_Finland_Larix_sibirica_GADA <- function(height,age,age2){
  a=-918.88187
  b=24579.54142
  c=11.43133
  j= 1.29114

  return(
    ((age2/age)^j*height*(b+sqrt(((b-((age^j)/height))^2)-2*a*(c+age^j)))-age^j)/(height*a*(1-(age2/age)^j)+((b-((age^j)/height)))+sqrt(((b-((age^j)/height))^2)-2*a*(c+age^j)))
  )
}
Vuokila_1983_height_trajectory_Finland_Larix_sibirica_GADA<-Vectorize(Vuokila_1983_height_trajectory_Finland_Larix_sibirica_GADA)



#' Basal Area Increment for 5 year period, Larix sibirica in Finland.
#'
#' @param SI100 Site Index H100, e.g. [forester::Vuokila_1983_height_trajectory_Finland_Larix_sibirica_GADA]
#' @param DominantHeightM Dominant height in meters.
#' @param BasalAreaM2 Basal Area, m^2 / ha.
#'
#' @return Mean Annual BAI during the future 5 year period, in percent (!)
#' @export
Vuokila_1983_BAI5_Larix_sibirica_Finland <- function(SI100, DominantHeightM, BasalAreaM2){
  return(
    0.09366 + 0.032306 * (SI100^1.7/(DominantHeightM^1.15*BasalAreaM2^0.6))
  )
}


#' Volume Increment in percent, Larix sibirica in Finland.
#'
#' @param volumeM3 Volume per ha, m^3/ha including bark.
#' @param dominantHeightM Dominant height of stand, meters.
#' @param SI100 Site index H100, e.g. [forester::Vuokila_1983_height_trajectory_Finland_Larix_sibirica_GADA]
#' @param period Length of calculation period, years.
#'
#' @return Mean annual volume increment percentage of the future calculation period.
#' @export
Vuokila_1983_volume_increment_Finland_Larix_sibirica <- function(volumeM3, dominantHeightM, SI100, period){
  return(
    0.28313 + 0.012757*SI100^2.02*dominantHeightM^-1.27*volumeM3^-0.38-321.80588*period*dominantHeightM^-3.5
  )
}
