## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(forester)
library(tidyverse)
library(robustbase)

## ----echo=TRUE----------------------------------------------------------------
forester::Silver_fir_1921

## ----echo=TRUE, eval=TRUE, out.width="100%", dpi=300--------------------------
Silver_fir_1921 %>% ggplot(aes(x=`Age`,y=`Mean height m`,color=factor(Site_class)))+geom_line()+theme(legend.position = "bottom")

## ----echo=TRUE, eval=TRUE, out.width="100%", warning=FALSE, dpi=300-----------
Silver_fir_1921 %>% ggplot(aes(x=`Mean height m`,y=`Total stem wood m3`,color=factor(Site_class)))+geom_line()+theme(legend.position = "bottom")

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
alldata <- Silver_fir_1921 %>% mutate(Site_class="All")
sitedata <- rbind(Silver_fir_1921, alldata)

#We need to remove any NA's in Siteclass, volume or height.
sitedata <- sitedata %>% filter(!is.na(Site_class), !is.na(`Total stem wood m3`), !is.na(`Mean height m`)) %>% rename(Volume=`Total stem wood m3`, height=`Mean height m`)
sitedata <- sitedata %>% group_by(Site_class) %>% nest()
sitedata

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
modfit <- function(df){
  nls(Volume ~ a * (height^b), data=df, start=c(a=1,b=1))
}

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
sitedata_model <- sitedata %>% 
  mutate(Model= purrr::map(.x=data, .f= modfit)) %>% 
  # extract formula from each model, convert to one-sided form, &
  # replace coefficients with fitted values, & store in dataframe
  # as character string 
  rowwise() %>%
  mutate(func = formula(Model) %>% 
           as.character() %>% 
           magrittr::extract(3) %>%
           gsub("height", ".x", ., fixed = T) %>%
           gsub("a", Model$m$getPars()[1], .) %>%
           gsub("b", Model$m$getPars()[2], .) %>%
           paste("~", ., collapse = "")) %>%
  ungroup()

print(sitedata_model)

## ----echo=TRUE, eval=TRUE, out.width="100%", dpi=300--------------------------
ggplot(sitedata_model$data[[6]])+
  theme(legend.position = "bottom")+
  geom_point(aes(x=height,y=Volume)) + #All the data
    # add formula in each row as a separate geom_function layer
  lapply(seq(1, nrow(sitedata_model)),
         function(i) geom_function(fun = rlang::as_function(formula(sitedata_model$func[i])),
                                   aes(colour = sitedata_model$Site_class[i]))) +
  
  # change legend name (can also change palette / labels / etc.)
  scale_colour_discrete(name = "Site class")


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
anova(sitedata_model$Model[[1]],sitedata_model$Model[[2]],sitedata_model$Model[[3]],sitedata_model$Model[[4]],sitedata_model$Model[[5]],sitedata_model$Model[[6]])

