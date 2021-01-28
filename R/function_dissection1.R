#' ID injured plants
#'
#' @param df A data frame containing tidy field trial records, i.e. one row per plant and year.
#' @param plantid Unique identifier for each plant.
#' @param year The year of observation, or other sortable time.
#' @param injuries A vector list of variables containing potential injury categories.
#' @param forbidden_values A vector list of injury severities which should be marked as injured.
#' @param monotonic_increase Logical, uses the is_monotonic function to in a new variable 'monotonic_growth' mark all before a potential first break as TRUE.
#' @param height Numeric variable containing the height observations of plants.
#'
#' @return Returns the original data frame with an additional column for:
#'     the individual plants first observered year of injury, 'firstinjury'
#'     if the current observation is before or after the first observed injury, 'afterfirstinjury', 1  is equal to or after, 0 for strictly before.
#' @export
#' @import dplyr
#' @import rlang
#' @import tidyr
#'
#' @examples #testframe expected
#' plantid <- rep(c(1,2,3,4,5), times=c(3,3,3,3,3))
#' year <- rep(1:3, length.out=length(plantid))
#' set.seed(42)
#' PrimaryInjury <- sample(c(NA,NA,NA,"Rust","Insect","Snow break"), 15, replace=TRUE)
#' SecondaryInjury <- rep(NA, length.out=length(plantid))
#' OtherInjury <- rep(NA, length.out=length(plantid))
#' height <- c(1,2,3,1,2,3,3,2,1,1,2,3,1,3,2)
#'
#'
#' expected <- data.frame(plantid,year,PrimaryInjury,SecondaryInjury, OtherInjury,height)
#'
#' rm(OtherInjury)
#' rm(plantid)
#' rm(PrimaryInjury)
#' rm(SecondaryInjury)
#' rm(year)
#' rm(height)
#'
#'
#' selectlist(df=expected,plantid=plantid, year=year, injuries=c("PrimaryInjury","SecondaryInjury","OtherInjury"),
#'           forbidden_values=c("Rust","Insect","Snow break"), monotonic_increase = TRUE, height=height)

id_injured <- function(df, plantid, year, injuries, forbidden_values, monotonic_increase=FALSE,height){
  #Converting variables in selectlist to character
  df <- df %>% mutate_at(injuries,as.character)

  #Parsing unquoted strings.

  plantid <- enquo(plantid)
  year <- enquo(year)
  height <- enquo(height)

  #Mark injured as 1.
  Dataplantid  <- df %>% mutate(is_injured = purrr::pmap_int(select(.,!!injuries), ~any(c(...) %in% !!forbidden_values)))

  #Find time of first observed injury.
  injuredlist <- Dataplantid %>% group_by(!!plantid) %>%  filter(is_injured == 1)  %>% summarise(firstinjury = min(!!year))  %>% ungroup()


  #In plantid group, all years older than first injury are marked 1, else 0.
  joinedinjured <- dplyr::left_join(x=Dataplantid, y=injuredlist, by = rlang::as_name(plantid))

  joinedinjured <- joinedinjured %>% group_by(!!plantid) %>% mutate(afterfirstinjury = case_when(!!year >= firstinjury ~ 1,
                                                                                                                     !!year< firstinjury ~ 0))

  returnvalues <- joinedinjured %>% select(!!plantid, !!year, firstinjury, afterfirstinjury) %>% ungroup()


  ##c() does not support rlang bangs (!!) but takes a string. Solution follows below approach
  # from Lionel Henry


  returnvalues <- left_join(df, returnvalues, by=c(rlang::as_name(plantid),rlang::as_name(year)))

#Mark all prior to a break in monotonic growth as true.
 if(monotonic_increase==TRUE){

    returnvalues<- returnvalues %>% group_by(!!plantid) %>% arrange(!!plantid, !!year) %>%
      tidyr::fill(!!height, .direction="downup") %>%
      mutate(monotonic_growth = cumall(c(TRUE, diff(!!height) >= 0))) %>% ungroup()
  returnvalues
 }
  returnvalues
}

