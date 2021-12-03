#' Shift a height trajectory function to age at breast height 1.3m
#'
#' @param height_trajectory A height-trajectory function containing the arguments 'dominant_height', 'age','age2','output'.
#' @param dominant_height Dominant height
#' @param reference_age Age
#' @param age2 Age at which to return height.
#' @param interval_low Default=0,Lowest possible time to breast height. (If a function already has an intercept at breast height)
#' @param interval_high Default=10, highest possible time to breast height.
#' @param ... A list of additional arguments passed to height_trajectory.
#'
#' @return Height at breast height age2.
#' @export
#'
#' @examples
#' ggplot()+
#'xlim(c(0,100))+
#'  geom_function(
#'    aes(linetype="Shift"),
#'    fun=function(x) shift_to_breast_height(height_trajectory = Eriksson_1997_height_trajectory_Sweden_Birch,dominant_height = 17,reference_age = 40,age2 = x)
#'  )+
#'  geom_function(
#'    aes(linetype="Normal"),
#'    fun=function(x) Eriksson_1997_height_trajectory_Sweden_Birch(dominant_height = 17,age = 40,age2 = x,output = "Height")
#'  )+
#'  geom_point(
#'   aes(x=40,y=17)
#' )
#'
shift_to_breast_height <- function(
  height_trajectory,
  dominant_height,
  age,
  age2,
  interval_low=0,
  interval_high=10,
  ...
){

  x <- suppressWarnings(optimize(function(x) (1.3-height_trajectory(dominant_height=dominant_height,age=age+x,age2=x,...))^2,interval=c(interval_low,interval_high))[[1]])

  return(
    suppressWarnings(height_trajectory(dominant_height=dominant_height,age=age+x,age2=age2+x,...))
  )

}
