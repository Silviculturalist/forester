# #' Dominant height trajectory for Norway Spruce in Bayern, Germany.
# #'
# #' @source Assmann, E. & Franz, F. 1965. Vorläufige Fichten-Ertragstafel für
# #' Bayern. Forstwissenschaftliches Centralblatt 84(1/2):13-43. Available online (2022-04-06):
# #' \url{https://www.waldwachstum.wzw.tum.de/fileadmin/publications/Assmann_1965_Vorlaeufige_Fichten-Ertragstafel.pdf}
# #'
# #' @param dominant_height Dominant height of the stand, m.
# #' @param age Age of the stand.
# #' @param age2 Desired age of the stand.
# #' @param output "Height" (default) or "Equation"
# #'
# #' @return Height of the stand in metres at the desired age.
# #' @name Assman1965
# Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce <- function(
#   dominant_height,
#   age,
#   age2,
#   output="Height"
# ){
#
#   #Find SI  function
#   if(age!=100){
#   find_SI <- function(SIH100,
#                       dominant_height,
#                       age){
#     a1 <- 0.4515343E01 + -0.2665320E-01*exp(log(SIH100)) + -0.2203244E-06 *exp((log(SIH100))^2)
#     a2 <- -0.8840923E00 + 0.2262338E-02*exp(log(SIH100)) + 0.8994708E-07 *exp((log(SIH100))^2)
#
#     a0 <- log10(SIH100) - a1*log10(100) - a2*((log10(100))^2)
#
#     return(
#       abs(dominant_height-10^(a0 + a1 *log10(age) + a2*(log10(age)^2)))
#     )
#   }
#
#   #Find SI by optimising for lowest discrepancy between dominant height, age pair and OH.
#   SI100 <- optimise(find_SI,lower=0,upper=100,dominant_height={dominant_height},age={age})[[1]]
#
#   }
#
#   if(age==100){
#     SI100 <- dominant_height
#   }
#
#   a1 <- 0.4515343E01 + -0.2665320E-01*exp(log(SI100)) + -0.2203244E-06 *exp((log(SI100))^2)
#   a2 <- -0.8840923E00 + 0.2262338E-02*exp(log(SI100)) + 0.8994708E-07 *exp((log(SI100))^2)
#
#   a0 <- log10(SI100) - a1*log10(100) - a2*((log10(100))^2)
#
#   if(output=="Height"){
#     return(
#       10^(a0 + a1 *log10(age2) + a2*(log10(age2)^2))
#     )
#   }
#
#   if(output=="Equation"){
#     return(
#       paste0("10^(",a0,"+",a1,"*log10(age2)+",a2,"*log10((age2)^2))")
#     )
#
#   }
#
# }
#
#
# Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce <- Vectorize(Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce)
#
#
# #' Height trajectory for Lorey's Mean Height from Assmann & Franz 1965.
# #' @rdname Assman1965
# #' @examples
# #' b1_seq = seq(0,-20,-0.5)
# #' b2_seq = seq(0,50,0.5)
# #'
# #' best_fit <- Inf
# #' best_b1 <- NULL
# #' best_b2 <- NULL
# #'
# #' for(b1 in b1_seq){
# #'   for(b2 in b2_seq){
# #'     solution = optim(par = c(b1,b2),
# #'                      fn = \(x) sum((c(6.1,8.7,11.2,13.7,16.1,18.3,20.3,22.2,23.9,25.5,26.9,28.3,29.5,30.6,31.5,32.4,33.3,34,34.6,35.2,35.7)-Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(initial_guess = x,SIH100 = 36,age=seq(20,120,5),YieldLevel = 1))^2+
# #'                                      (c(3.5,5.3,7.2,9.1,10.9,12.7,14.3,15.8,17.3,18.6,19.8,20.9,22,22.9,23.8,24.6,25.3,26,26.6,27.2,27.7)-Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(initial_guess = x,SIH100 = 28,age=seq(20,120,5),YieldLevel = 2))^2),
# #'                      method = 'Nelder-Mead'
# #'     )
# #'     if(solution$value<best_fit){
# #'       best_fit = solution$value
# #'       best_b1 = solution$par[1]
# #'       best_b2 = solution$par[2]
# #'     }
# #'   }
# #' }
# #'
# #' ggplot()+
# #'   xlim(c(1,120))+
# #'   ylim(c(0,40))+
# #'   geom_function(fun=(\(x) Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce(36,100,x)))+
# #'   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(age = x,SIH100 = 36,YieldLevel = 1)))+
# #'   geom_point(aes(x=seq(20,120,5),y=c(6.1,8.7,11.2,13.7,16.1,18.3,20.3,22.2,23.9,25.5,26.9,28.3,29.5,30.6,31.5,32.4,33.3,34,34.6,35.2,35.7)))
# #'
# #'
# #'
# #' ggplot()+
# #'   xlim(c(1,120))+
# #'   ylim(c(0,50))+
# #'   geom_function(fun=(\(x) Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce(28,100,x)))+
# #'   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(age = x,SIH100 = 28,YieldLevel = 2)))#+
# #'   geom_point(aes(x=seq(20,120,5),y=c(3.5,5.3,7.2,9.1,10.9,12.7,14.3,15.8,17.3,18.6,19.8,20.9,22,22.9,23.8,24.6,25.3,26,26.6,27.2,27.7)))
#
# Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce <- function(
#   SIH100,
#   age,
#   YieldLevel = 3
# ){
#
#   H0 = Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce(dominant_height = SIH100,age = 100,age2 = age)
#
#   # Now calculate the difference between the dominant height and the HL.
#   paramsB<- switch(
#     YieldLevel,
#     list("b00"=1.95,"b01"=0.019,"b10"=158.4,"b11"=-2.16,"b20"=0.02584,"b21"=0.01553),
#     list("b00"=2.3,"b01"=0.015,"b10"=160,"b11"=-2.5,"b20"=-0.0202,"b21"=0.01783),
#     list("b00"=2.7,"b01"=0.01,"b10"=160,"b11"=-2.75,"b20"=-0.06899,"b21"=0.02027)
#     )
#
#   y1 = exp(paramsB[['b20']]+paramsB[['b21']]*SIH100)
#   x1 = 20
#   y2 = paramsB[['b00']]+paramsB[['b01']]*SIH100
#   x2 = paramsB[['b10']]+paramsB[['b11']]*SIH100
#
#
#   #Solve for pars. (x2,y2) must be a maximum.
#   b2 = (log(y1)-log(y2))/(-2*log(x2)*(log(20)-log(x2)) + (log(20)*log(20)-log(x2)*log(x2)))
#   b1 = -2*b2*log(x2)
#   b0 = log(y1)+2*b2*log(x2)*log(20)-b2*log(20)*log(20)
#
#
#
#   return(
#    H0- exp(b0+b1*log(age)+b2*log(age)*log(age))
#   )
#
# }
#
# Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce <- Vectorize(Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce,vectorize.args = 'age')
#
# ggplot()+
#   xlim(c(0,150))+
#   geom_function(fun=(\(x) Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce(dominant_height = 40,age = 100,age2 = x)))+
#   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(SIH100 = 40,age = x,YieldLevel = 1)))+
#   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(SIH100 = 40,age = x,YieldLevel = 2)))+
#   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(SIH100 = 40,age = x,YieldLevel = 3)))
#
#
# # getHLfuncPoints <- function(
# #     SIH100,
# #     YieldLevel = 3){
# #
# #   # Now calculate the difference between the dominant height and the HL.
# #   paramsB<- switch(
# #     YieldLevel,
# #     list("b00"=1.95,"b01"=0.019,"b10"=158.4,"b11"=-2.16,"b20"=0.02584,"b21"=0.01553),
# #     list("b00"=2.3,"b01"=0.015,"b10"=160,"b11"=-2.5,"b20"=-0.0202,"b21"=0.01783),
# #     list("b00"=2.7,"b01"=0.01,"b10"=160,"b11"=-2.75,"b20"=-0.06899,"b21"=0.02027)
# #   )
# #
# #   y1 = paramsB[['b00']]+paramsB[['b01']]*SIH100
# #   A1 = paramsB[['b10']]+paramsB[['b11']]*SIH100
# #   y2 = exp(paramsB[['b20']]+paramsB[['b21']]*SIH100)
# #   A2 = 20
# #
# #   return(
# #     data.frame(
# #       x=c(A2,A1),
# #       y=c(y2,y1)
# #     )
# #   )
# # }
# #
# # ##Plot
# # ggplot()+
# #   xlim(c(0,130))+
# #   #ylim(c(0,40))+
# #   #geom_function(fun=(\(x) Assmann_Franz_1965_height_trajectory_Germany_Bayern_Spruce(40,age=100,age2=x)))+
# #   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(SIH100 = 30,age = x,YieldLevel = 3)))+
# #   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(SIH100 = 30,x,YieldLevel = 2)))+
# #   geom_function(fun=(\(x) Assmann_Franz_1965_Lorey_height_trajectory_Germany_Bayern_Spruce(SIH100 = 30,x,YieldLevel = 1)))+
# #   geom_point(data=getHLfuncPoints(30,1),aes(x=x,y=y))+
# #   geom_point(data=getHLfuncPoints(30,2),aes(x=x,y=y))+
# #   geom_point(data=getHLfuncPoints(30,3),aes(x=x,y=y))
#
#
#
#
#
# ###Sonja excel
# Assmann_Franz_1965_maximumBasalArea_Germany_Bayern_Spruce <- function(
#     SIH100,
#     age,
#     YieldLevel){
#   paramsC <- switch(
#     YieldLevel,
#     list('c00'=3.11613, 'c01'=0.02699, 'c10'=4.93692,	'c11'=-0.03359,	'c20'=8.89123, 'c21'=-3.33521),
#     list('c00'=3.03803, 'c01'=0.03128, 'c10'=5.04091,	'c11'=-0.07097,	'c20'=8.39441, 'c21'=-3.1666),
#     list('c00'=2.96995, 'c01'=0.03532, 'c10'=5.14594,	'c11'=-0.10871,	'c20'=7.67280, 'c21'=-2.93375)
#     )
#   c2 = 1/(paramsC['c20']+paramsC['c21']*log(SIH100))
#   Gmax = exp(paramsC['c00']+paramsC['c01']*SIH100)
#   AgeMax = exp(paramsC['c10']+paramsC['c11']*log(SIH100))
#   c1 = -2*c2*log(AgeMax)
#   c0 = log(Gmax)-c1*log(AgeMax)-c2*(log(AgeMax))^2
#
#   return(
#     exp(c0+c1*log(age)+c2*log(Age)*log(Age))
#   )
#
# }
#
#
# Assmann_Franz_1965_optimal_BA_Germany_Bayern_Spruce <- function(
#   SIH100,
#   age,
#   YieldLevel
#   ){
#   paramsD <- switch(
#     YieldLevel,
#     list('d00'=-16.46746, 'd01'=4.04473, 'd10'=4.26535,	'd11'=-0.17377,	'd20'=2.14530, 'd21'=-0.80894),
#     list('d00'=-14.27555, 'd01'=3.45943, 'd10'=4.23321,	'd11'=-0.16146,	'd20'=-0.91213, 'd21'=-0.01154),
#     list('d00'=-12.81379, 'd01'=3.07166, 'd10'=4.07376,	'd11'=-0.10434,	'd20'=-7.54358, 'd21'=1.72779)
#   )
#
#   y1 = paramsD[['d00']]+paramsD[['d01']]*log(SIH100)
#   x1 = exp(paramsD[['d10']] + paramsD[['d11']]*log(SIH100))
#   d2 = paramsD[['d20']]+paramsD[['d21']]*log(SIH100)
#
#   d1 = -d2*(1+2*log(x1))
#   d0 = log(1-y1)+d2*log(x1)
#
#   return(
#     1-exp(d0 + d1*log(age)+d2*log(age)*log(age))
#   )
#
#
# }
#
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce <- function(
#  age,
#  SIH100,
#  quotientOfOptimalBasalArea=1,
#  YieldLevel=1
#){
#  stopifnot(quotientOfOptimalBasalArea>0)
#  stopifnot(quotientOfOptimalBasalArea<=1)
#
#  paramsE <- switch(
#    YieldLevel,
#    list('e00'=-69.03514, 'e01'=29.28671, 'e11'=0.12074,	'e20'=16.52127, 'e21'=-4.53916),
#    list('e00'=-70.66391, 'e01'=29.86379, 'e11'=0.11941,	'e20'=17.06969, 'e21'=-4.72403),
#    list('e00'=-72.29268, 'e01'=30.44087, 'e11'=0.11810,	'e20'=20.43591, 'e21'=-5.86625)
#  )
#
#  e1 = (paramsE[['e00']]+paramsE[['e01']]*log(SIH100))/(100-exp(paramsE[['e20']]+paramsE[['e21']]*log(SIH100)))
#  e0 = paramsE[['e00']] + paramsE[['e01']]*log(SIH100)-(100*(paramsE[['e00']]+paramsE[['e01']]*log(SIH100)))/(100-exp(paramsE[['e20']]+paramsE[['e21']]*log(SIH100)))
#  e10 = log(e0+100*e1)-log(1.1-quotientOfOptimalBasalArea)*paramsE[['e11']]
#
#  e1p = exp(e10+paramsE[['e11']]*log(1.1-quotientOfOptimalBasalArea))/(100-exp(paramsE[['e20']]+paramsE[['e21']]*log(SIH100)))
#  e0p = exp(e10+paramsE[['e11']]*log(1.1-quotientOfOptimalBasalArea))-(100*exp(e10+paramsE[['e11']]*log(1.1-quotientOfOptimalBasalArea)))/(100-exp(paramsE[['e20']]+paramsE[['e21']]*log(SIH100)))
# return(
#    e0p+e1p*age
#  )
#
#}
#
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce <- Vectorize(Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce)
#
#
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(age=100,SIH100 = 40,quotientOfOptimalBasalArea = 0.79,YieldLevel = 2)
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(age=100,SIH100 = 40,quotientOfOptimalBasalArea = 1,YieldLevel = 2)
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(age=100,SIH100 = 40,quotientOfOptimalBasalArea = 0.94,YieldLevel = 2)
#
#
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(age=100,SIH100 = 30,quotientOfOptimalBasalArea = 0.92,YieldLevel = 2)
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(age=100,SIH100 = 30,quotientOfOptimalBasalArea = 1,YieldLevel = 2)
#Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(age=100,SIH100 = 30,quotientOfOptimalBasalArea = 0.98,YieldLevel = 2)
#
#
#ggplot()+
#  xlim(c(0,120))+
#  geom_function(fun=(\(x) Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(x,40,YieldLevel = 2)))+
#  geom_function(fun=(\(x) Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(x,40,YieldLevel = 2,quotientOfOptimalBasalArea = 0.79)))+
#  geom_function(fun=(\(x) Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(x,40,YieldLevel = 2,quotientOfOptimalBasalArea = 0.94)))+
#  geom_function(fun=(\(x) Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(x,30,YieldLevel = 2)))+
#  geom_function(fun=(\(x) Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(x,30,YieldLevel = 2,quotientOfOptimalBasalArea = 0.92)))+
#  geom_function(fun=(\(x) Assmann_Franz_1965_diameter_age_Germany_Bayern_Spruce(x,30,YieldLevel = 2,quotientOfOptimalBasalArea = 0.98)))
#
#
