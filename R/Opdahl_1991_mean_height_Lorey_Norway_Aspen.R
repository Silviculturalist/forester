Opdahl_1991_mean_height_Lorey_Norway_Aspen <- function(
  dominant_height,
  BA_mean_diameter,
  stems_per_ha_before_thinning,
  Basal_area_m2_ha_before_thinning,
  stand_age_at_breast_height,
  correction=TRUE
){

  correction <- ifelse(isTRUE(correction),0.75,0)

  return(
       #Observe, contains typo in source. Missing a closing bracket.
       ((dominant_height-correction)-(13.7396 + ((dominant_height-correction)*158.856)-(((dominant_height-correction)^2)*3.78E-05)-(BA_mean_diameter*0.41154)+((BA_mean_diameter*(dominant_height-correction))*0.001406)+((stems_per_ha_before_thinning*(dominant_height-correction))*7e-04)-((Basal_area_m2_ha_before_thinning^2)*0.0152)+(stand_age_at_breast_height*0.001254))/(1E+05))
     )


}

#
# #Find the right values.
#
#
# #output grid
# output_grid<- matrix(nrow=0,ncol=10)
# colnames(output_grid) <- c("a_val","b_val","c_val","d_val","e_val","f_val","g_val","h_val","k_val","RMSE")
#
# #iteration zero
# i <- 0
#
# a <- 137.396
# b <- 0.158856
# c <- 0.0378
# d <- 4.1154
# e <- 0.01406
# f <- 0.00007
# g <- 0.00152
# h <- 0.01254
# k <- 100
#
# a_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# b_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# c_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# d_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# e_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# f_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# g_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# h_val <- c(1E-2,1E-1,1E0,1E1,1E2)
# k_val <- c(1E-2,1E-1,1E0,1E1,1E2)
#
# test_grid <- expand_grid(a_val,b_val,c_val,d_val,e_val,f_val,g_val,h_val,k_val)
#
#
# correction=0.75
#
# correctframe <-
#   data.frame(
#     ages=c(40,45,50,55,60,65,70,75,80),
#     H0=c(14,14.9,15.7,16.4,17,17.6,18.1,18.6,19),
#     stems=c(1400,1372, 1345,1318,702,688,674,661,648),
#     QMD=c(10.8,11.9,12.9,14,16.7,17.8,19,20.2,21.3),
#     H1=c(13.2,14.1,14.6,15.1,16.3,16.8,17.2,17.7,18.2),
#     G1=c(12.9,15.2,17.7,20.2,15.3,17.2,19.1,21.1,23.1)
#   )
#
# correctframe2 <-
#   data.frame(
#     ages=c(15,20,25,30,35,40,45,50,55,60,65,70,75,80),
#     H0=c(14.1,17.5,20.3,22.5,24.4,26,27.3,28.5,29.5,30.4,31.2,31.9,32.5,33),
#     stems=c(1900,1862,813,797,781,765,370,363,356,349,342,335,328,321),
#     QMD=c(9.8,11.9,16.8,19.4,22,24.5,30.4,33.3,36.1,38.8,41.5,44.1,46.6,49.1),
#     H1=c(13.2,15,18.4,20.7,22.9,25.1,26.5,27.7,28.8,29.6,30.4,31.1,31.7,32.3),
#     G1=c(14.4,20.7,18,23.6,29.7,36.1,26.9,31.6,36.4,41.3,46.2,51.1,56,60.8)
#   )
#
# correctframe_combine <- rbind(correctframe,correctframe2)
#
# dom_h <- function(dominant_height,BA_mean_diameter,stems_per_ha_before_thinning,Basal_area_m2_ha_before_thinning,stand_age_at_breast_height,a_val,b_val,c_val,d_val,e_val,f_val,g_val,h_val,k_val,a,b,c,d,e,f,g,h,k){
#   correction=0.75
#   ((dominant_height-correction)-(a*a_val + ((dominant_height-correction)*b*b_val)-(((dominant_height-correction)^2)*c*c_val)-(BA_mean_diameter*d*d_val)+((BA_mean_diameter*(dominant_height-correction))*e*e_val)+((stems_per_ha_before_thinning*(dominant_height-correction))*f*f_val)-((Basal_area_m2_ha_before_thinning^2)*g*g_val)+(stand_age_at_breast_height*h*h_val))/(k*k_val))
# }
#
# Rmse_f <- function(a_val,b_val,c_val,d_val,e_val,f_val,g_val,h_val,k_val,a,b,c,d,e,f,g,h,k){
#   stand_age_at_breast_height=correctframe_combine$ages
#   dominant_height=correctframe_combine$H0
#   stems_per_ha_before_thinning=correctframe_combine$stems
#   BA_mean_diameter=correctframe_combine$QMD
#   H1=correctframe_combine$H1
#   Basal_area_m2_ha_before_thinning=correctframe_combine$G1
#
#   H2 <- dom_h(dominant_height = dominant_height,BA_mean_diameter = BA_mean_diameter,stems_per_ha_before_thinning = stems_per_ha_before_thinning,Basal_area_m2_ha_before_thinning = Basal_area_m2_ha_before_thinning,stand_age_at_breast_height = stand_age_at_breast_height,a_val = a_val,b_val = b_val,c_val = c_val,d_val = d_val,e_val = e_val,f_val = f_val,g_val = g_val,h_val = h_val,k_val = k_val,a=a,b=b,c=c,d=d,e=e,f=f,g=g,h=h,k=k)
#   return(sqrt(((H2-H1)^2)/23))
# }
#
#
# for(i in 1:3){
#
# svMisc::progress(i,3)
#
# test_grid2 <-
#   test_grid %>% mutate(
#     RMSE = Rmse_f(
#       a_val = a_val,
#       b_val = b_val,
#       c_val = c_val,
#       d_val = d_val,
#       e_val = e_val,
#       f_val = f_val,
#       g_val = g_val,
#       h_val = h_val,
#       k_val = k_val,
#       a=a,
#       b=b,
#       c=c,
#       d=d,
#       e=e,
#       f=f,
#       g=g,
#       h=h,
#       k=k
#     ),
#     i=i
#   )
#
# out_vals <- test_grid2 %>% arrange(desc=FALSE,RMSE) %>% slice_head()
#
# #output best RMSE
# output_grid <- rbind(output_grid,out_vals)
#
# #update values
# a <- a * out_vals[["a_val"]]
# b <- b * out_vals[["b_val"]]
# c <- c * out_vals[["c_val"]]
# d <- d * out_vals[["d_val"]]
# e <- e * out_vals[["e_val"]]
# f <- f * out_vals[["f_val"]]
# g <- g * out_vals[["g_val"]]
# h <- h * out_vals[["h_val"]]
# k <- k * out_vals[["k_val"]]
#
# }
#
#
#
#
# #0.00000655
# H2 <- ((dominant_height-correction)-(137.396 + ((dominant_height-correction)*1.58856*b_val)-(((dominant_height-correction)^2)*0.378*c_val)-(BA_mean_diameter*0.41154*d_val)+((BA_mean_diameter*(dominant_height-correction))*0.001406*e_val)+((stems_per_ha_before_thinning*(dominant_height-correction))*0.0000000000007*f_val)-((Basal_area_m2_ha_before_thinning^2)*0.0152*g_val)+(stand_age_at_breast_height*0.1254*h_val))/(100))
#
# #0.00000000286
# H2 <- ((dominant_height-correction)-(13.7396 + ((dominant_height-correction)*158.856)-(((dominant_height-correction)^2)*3.78E-05)-(BA_mean_diameter*0.41154*d_val)+((BA_mean_diameter*(dominant_height-correction))*0.001406)+((stems_per_ha_before_thinning*(dominant_height-correction))*7e-04*f_val)-((Basal_area_m2_ha_before_thinning^2)*0.0152*g_val)+(stand_age_at_breast_height*0.001254*h_val))/(1E+05))
#
#
# #View results
# correctframe_combine %>% mutate(H2=((H0-0.75)-(13.7396 + ((H0-0.75)*158.856)-(((H0-0.75)^2)*3.78E-05)-(QMD*0.41154)+((QMD*(H0-0.75))*0.001406)+((stems*(H0-0.75))*7e-04)-((G1^2)*0.0152)+(ages*0.001254))/(1E+05)))
