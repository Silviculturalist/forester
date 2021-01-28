## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(forester)
library(tidyverse)

## -----------------------------------------------------------------------------
norm_distr <- data.frame(rnorm(200, mean=6, sd=2.5))
norm_distr <- norm_distr %>% rename(height = rnorm.200..mean...6..sd...2.5.)


## ---- out.width="75%", dpi=300------------------------------------------------
norm_distr %>% ggplot(aes(x=height)) + geom_density()+theme(legend.position="bottom")

## -----------------------------------------------------------------------------
norm_distr_summary <- norm_distr %>% summarise(mean_stand_height=mean(height), dominant_height=quantile(height,probs=0.9, names=FALSE))
norm_distr_summary

## -----------------------------------------------------------------------------
norm_thinned <- norm_distr %>% filter(height > quantile(height, probs=0.3))

norm_thinned_summary <- norm_thinned %>% summarise(mean_stand_height=mean(height), dominant_height=quantile(height,probs=0.9,names=FALSE))
norm_thinned_summary

## ----out.width="75%", dpi=300-------------------------------------------------
#Create identifying variables
norm_distr <- norm_distr %>% mutate(type="Normal")
norm_thinned <- norm_thinned %>% mutate(type="Thinned")

# Append the tables.
distr_joined <- rbind(norm_distr, norm_thinned)

#Plot the distributions of the stand before and after the low-grading.
distr_joined %>% ggplot(aes(x=height,fill=type))+geom_histogram(alpha=0.5, bins=50)+theme(legend.position="bottom")


## -----------------------------------------------------------------------------
#mean stand height
((norm_thinned_summary[1,1]/norm_distr_summary[1,1])-1)*100

## -----------------------------------------------------------------------------
#dominant stand height
((norm_thinned_summary[1,2]/norm_distr_summary[1,2])-1)*100

## ----echo=FALSE, eval=TRUE----------------------------------------------------
#Näslund 1936 - y-1.3~I(dia^2)/I((a + b*dia)^2)
#Spruce a=19.0425458
#Spruce b= 0.2302333
hmod <- function(data){
 1.3 + I(data^2)/I((19.0425458+ 0.2302333*data)^2)
} 
diametersequence <- seq(1,300,15)

diametersequence <- data.frame(diameter=diametersequence)

diametersequence <- diametersequence %>% mutate(height=hmod(diameter))
rm(hmod)
#add randomness
diametersequence <- diametersequence %>% group_by(diameter) %>% mutate(noise= runif(1,min=-0.08*height, max=0.08*height)) %>% mutate(height2=height+noise)

Spruce_height_diameter <- diametersequence %>% select(diameter, height2) %>% rename(height=height2)

## ----echo=FALSE, results='asis',warning=FALSE---------------------------------
library(knitr)
kable(Spruce_height_diameter[1:5,],caption="A number of Spruce heights and diameters from a stand")

## ----echo=TRUE, out.width="100%",dpi=300--------------------------------------
Spruce_height_diameter %>% ggplot()+geom_point(aes(x=diameter,y=height))


## -----------------------------------------------------------------------------

Spruce_height_diameter <- Spruce_height_diameter %>% mutate(species="Spruce") %>% group_by(species) %>% nest() %>%  mutate(
  model= map(data, ~nls(height ~ 1.3 + I(diameter^2)/I((a+ b*diameter)^2), start=list(a=1,b=1),data=.))) %>% 
  # extract formula from each model, convert to one-sided form, &
  # replace coefficients with fitted values, & store in dataframe
  # as character string 
  rowwise() %>%
  mutate(func = formula(model) %>% 
           as.character() %>% 
           magrittr::extract(3) %>%
           gsub("diameter", ".x", ., fixed = T) %>%
           gsub("a", model$m$getPars()[1], .) %>%
           gsub("b", model$m$getPars()[2], .) %>%
           paste("~", ., collapse = "")) %>%
  ungroup()

print(Spruce_height_diameter)

## ----echo=TRUE, eval=TRUE, out.width="100%", dpi=300--------------------------
ggplot(Spruce_height_diameter[[2]][[1]])+
  theme(legend.position = "bottom")+
  geom_point(aes(x=diameter,y=height)) + #All the data
    # add formula in each row as a separate geom_function layer
  lapply(seq(1, nrow(Spruce_height_diameter)),
         function(i) geom_function(fun = rlang::as_function(formula(Spruce_height_diameter$func[i])),
                                   aes(colour = Spruce_height_diameter$species[i]))) +
  
  # change legend name (can also change palette / labels / etc.)
  scale_colour_discrete(name = "Spruce")


## -----------------------------------------------------------------------------
forester::plant_spacing(stems_per_ha = 4400)

## -----------------------------------------------------------------------------
#We assume normal diameter distribution
diameters <- data.frame(diameters= rnorm(4000, mean=130, sd=50))

#The stand has been heavily thinned from below (40%)
diameters <- diameters %>% filter(diameters > quantile(diameters, probs=0.4))


## -----------------------------------------------------------------------------
diameters <- diameters %>% mutate(height= 1.3 + diameters^2/(19.1265151455858 + 0.229470870878339 * diameters)^2)

## -----------------------------------------------------------------------------
diameters <- diameters %>% mutate(volume= tree.volume(dbh.cm=diameters/10, height.m=height,species.latin="Picea abies", latitude = 65))

## -----------------------------------------------------------------------------
diameters %>% summarise("Total volume" = sum(volume))

## -----------------------------------------------------------------------------
diameters %>% mutate(volume_m3 = volume / 1000) %>% summarise("Total volume"= sum(volume_m3))

