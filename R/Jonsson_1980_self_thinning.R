#' Jonsson (1980) self thinning function
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 120.
#'
#' @details
#' Standard deviation = 30.3
#' Multiple correlation coefficient = 0.60
#' Material: Untouched parcels on permanent trial sites, 127 revisions.
#'
#' @param stem_number_per_ha Number of stems per hectare.
#' @param jonson_bonitet numeric. Jonson site classes 1-7.
#' @param annual_basal_area_increment_percent  Annual basal area increment, in percent.
#'
#' @return Self thinned volume in percent of the annual volume increment.
#' @export
#'
#' @examples
#'self_thinning_df <- data.frame("Bonitet"=c(1,1,1,1,1,
# 5,5,5,5,5,
# 6,6,6,6,6),
# "Stems per ha"=c(500,1000,2000,4000,6000,
#                  500,1000,2000,4000,6000,
#                  500,1000,2000,4000,6000)
# )
#
# self_thinning_df2 <- self_thinning_df %>% rowwise() %>%  mutate(percent_self_thinned = Jonsson_1980_self_thinning(stem_number_per_ha = Stems.per.ha,
#                                                                                                                   jonson_bonitet = Bonitet,
#                                                                                                                   annual_basal_area_increment_percent = 1.23))
#
# self_thinning_df2 %>% ggplot(aes(x=Stems.per.ha,y=percent_self_thinned))+geom_line(aes(group=Bonitet))+geom_label(aes(label=Bonitet))
#'
#'
#'
#'
#'
Jonsson_1980_self_thinning <- function(
  stem_number_per_ha,
  jonson_bonitet,
  annual_basal_area_increment_percent
){
  if(!(is.numeric(jonson_bonitet))){
    stop("Jonson bonitet must be numeric.")
  }

  if(!is.numeric(stem_number_per_ha)){
    stop("Stems per ha must be numeric.")
  }

  if(!is.numeric(annual_basal_area_increment_percent)){
    stop("Annual basal area increment in percent must be numeric.")
  }


  if(jonson_bonitet%in%c(1,2,3,4)){
    a <- 11975E-6*stem_number_per_ha
  } else if(jonson_bonitet==5){
    a <- 90206E-7*stem_number_per_ha
  } else if(jonson_bonitet>=6){
    a <- 72101E-7*stem_number_per_ha
  }

  constant <- 88985E-3

  growth_factor <- -49519E-3*annual_basal_area_increment_percent

  self_thinning_percent <- constant + growth_factor + a

  return(self_thinning_percent)


}



