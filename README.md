<p align="center">
  <img width="497" height="700" src="https://user-images.githubusercontent.com/52540997/168085155-6f80ea54-b34d-47c3-a067-d5cc2674a93e.png">
</p>

# forester

## TL;DR

Old stand simulators are difficult to replicate, incur a language barrier, are difficult to access, use different units and names for variables and factors. By standardizing we can easily swap out data, functions, simulation models or run them side-by-side.

For style guide on R code, <https://adv-r.hadley.nz>

For git interaction with R, <https://happygitwithr.com/index.html>

As a GIT client, I use: <https://www.gitkraken.com>

## News:

Ongoing: Variable renaming, e.g. issue #39. Common abbreviations can be useful. Variables names should include unit. New naming convention where name is followed by '\<varname\>.\<unit\>' .

Saturday 9 July: Analogues to previous change on Friday 8 July also for Hägglunds function for Scots Pine (1974).

Friday 8 July: Strong clarifications of Hägglund 1972 & 1973 functions, which took age at breast height and reported Site Index as total age.

Thursday 07 July 2022: SIS_est() for calculating site index from stand factors (Hägglund, B. & Lundmark, J-E. 1979) is no longer exported due to large errors in calculation).

Tuesday 14 June 2022: Example vignette introducing the Hoffmann_1982 dataset for constructing secondary volume functions (Tariff functions).

Saturday 04 June 2022 : Massive restructuring of Söderbergs 1986, 1992 functions for individual tree species.

## Downloading and Installing from Github

I have started a git repository on Github from which anyone can download my R-package (in development), suggest edits or point out issues: <https://github.com/Silviculturalist/forester>

To install the latest development version of this package, use devtools.

```{r}
devtools::install_github("Silviculturalist/forester")
```

## Current Cooperations

PrognAus (PROGNosis for AUStria), an Austrian individual tree growth and yield model is being rewritten for R concurrently, <https://github.com/Silviculturalist/Prognaus> as a fork from Sonja Vospernik's main modules from the original PrognAus programme <https://github.com/SonjaVospernik/Prognaus> to maintain the same programming style, parameter names as Forester.

## Visualisation Module

A separate goal is to develop an open-ended visualisation module for forester written in C++ which is based on Khronos Groups royalty free open standards OpenGL and/or Vulcan. A simple API can be written for access through R - handing a suitable data.frame with stand information over.

## Goal

I hope that this will become a good digital centre for maintaining our valuable research heritage in the digital era, and will work hand-in-hand with digitization efforts from the SLU library to ensure that source material is not only referenced in the documentation, but in the future actively linked to.

The goal of this github repository is to be a centre to:

-   Digitalise

-   Review, discuss, validate and update functions.

-   Maintain version control and good documentation;

I am continuing to develop object structures for which R will recognize attributes which are related to trees, stand information, site information, and treatment information, such that this may grow into a stand simulator on its' own.

As work with the package progresses, it is my full intent that other repositories should open, which are not be limited to Nordic conditions, but should eventually form an open online repository for functions related to forest science, forestry and related data from all over the world with the same variable names, naming routines and general philosophy.

Plenty functions from Norway are already included.

## Why?

As such, it will remedy several of the issues I have noticed with earlier stand simulators :

-   Documentation of R packages is stricter and more structured.

-   Issues can be recognized and solved open-source.

-   The user will have full control over all processes (no black-box functionality!)

-   The material is less at risk to become obsolete through technical issue.

-   Trust in the collection is gradually increased and maintained by its' transparency.

## What's included?

The package includes:

1.  Vignettes (lab-instructions and examples for how to use)

2.  Functions

3.  Documentation (every single function or data is well documented, indexed and searchable and given with examples of use).

4.  Data , so far mostly older published.

## Data and Language Style

### Performance

Overhead loops create a large overhead in R. Through Rccp, we can seamlessly integrate C++ functions in the R package to maintain the speed of C++ and the readability and useability of R for functions or programmes which might slow things down.

### Functions

To maintain order and clarity, functions should as far as possible be named as follows:

Author \_ Year \_ Dependent Variable \_ Locality \_ Species

*E.g. Eko_PM_1985_basal_area_5\_year_increment_northern_Sweden_Spruce*

When a species name is used in a function name, it should be colloquial, e.g. Pine, Spruce. When a species name is used in an argument, it should be latin binomial: "Pinus sylvestris", "Picea abies".

All functions must include a LaTeX preamble from roxygen2 which writes the help documentation. All functions must include their source.

**It is preferable that functions are vectorised.** Work is ongoing to enforce this standard on committed work.

### Variables

Variables should follow the form:

variable . unit

*E.g.* diameter.cm

A non-exhaustive list of variable names follows, taken from the treelistData file. These have been based on previous work from the Heureka project, Indelningspaketet, Projekt HUGIN, and other stand simulators by the forest science faculty at SLU. This does not hinder additions from expanding the expected structure, for example vegetation types, although it would be preferable to introduce new variables.

Variables should be named so as to as clearly as possible convey their meaning. *Avoid abbreviations*. Export standards for software can be written, but should not be included in this package, such as for the **StanForD 2010 standard**, used by forestry machines.

Optional arguments should take the default value NULL, see SO answer : <https://stackoverflow.com/a/28370496/11550980>

## Common function types

### Top Height functions

Some of the most strictly defined functions, it is strongly preferable that as far as possible, they should be formulated following the schematic form. This was implemented in commit `03ed15c4a47d3cac49b8d2ae3e80c7a922751522` (3rd december 2021). This ensures that higher level functions, e.g. `shift_to_breast_height()` can interact properly with the height trajectory functions.

*Author_year_locale_height_trajector(y/-ies)\_commonName* is a function with the following arguments:

-   dominant height : observed dominant height (meters)

-   age : observed age (meters)

-   age2 : Age at output (in case of Height)

-   output : one of 'Height' (default), 'SI100' or 'Equation'.

-   additional arguments attempt to follow argument naming conventions with sensible default.

-   *Output must be in meters.* Many works commonly report height in decimeters. This must be transformed before it is returned by the function.

This is appropriate in relation to common usage, e.g. for predicting height at another age along the same curve; or for plotting (equation output). Site Index is only a convenience output, and may be deprecated in the future as typical base ages are different between countries (e.g. *Norway* SI40; *Sweden* SI100).

#### Height trajectories other than Top Height.

Other measures than dominant height / top height have commonly been used, most notably Lorey's mean height. In terms of naming for such functions, we attempt to follow the above recommendations as closely as possible, e.g. `Tveite_1967_Loreys_mean_height_Norway_Pine()` .

#### Height relationships other than age-height.

In some cases, e.g. when the equation form uses Näslund 1936 (eq. 7, p. 43), where y is the height of the tree, x corresponding to the diameter at breast height and a & b are estimated constants.

$$
y-1.3 = \frac{x^2}{(a+bx)^2}
$$

These functions should probably be categorized under a different name than height_trajectory for clarity.

### Categorical Coding- avoiding help-file cluttering.

#### Factor variables with many categories

#### Vegetation

Typical example : **vegetation classes**. Classes are different between national forest inventories and forest practice in countries. For this reason, the *variable* is set to **vegetation** across functions, but a helper function is defined to distinguish between many factors that would otherwise clutter the help documentation. E.g. `Finland_vegetation_types()` . Function-specific behavior ensures that unexpected input simply doesn't work.

#### Incline, Aspect

Both incline and aspect are typical continuous variables - different nomenclatures exist between countries and between individual reports in how these are intended to be handed to functions. Because of these large discrepances, I suggest any categorical coding is handled *inside* the function, and the arguments are handed a continuous variable, e.g.

Incline: Percentage (Ratio?).

Aspect: Degrees, 0-360.

Many functions utilise a wide variety of different categorical variables which are not translatable to a continuous variable or better expressed in a more general way. E.g. vegetation, soil texture or moisture classes. Not uncommonly these are standards given by the national forest inventory of the country.

These may include a large number of variables which are not easily renderable/viewable in a normal help-file context. They may also have to be repeated in many places. Therefore a separate function is suitable and avoids local errors/typos.

Examples include `Finland_vegetation_types()`, `Sweden_vegetation_types()` , `Sweden_soil_types()` .

Typically they include a short header printed by `cat` , followed by a subsettable `tibble`, which must include at least the coding and english/latin definition. In case of closely related categorical variables, e.g. soil moisture, soil depth, soil texture - these can be included in the same function with a conditional return clause, e.g. `Sweden_soil_types(type = "texture")` .

Some cases with very few categories can be questionable but are possible to include anyway, if it fits well with other group variables or are very commonly used, e.g. `Sweden_soil_types(type = "water")` .

## Very large or slow functions

Functions containing very many cases/switches/if else statements, slow blocks, iterations or wrappers containing entire models should probably be written up in C++ and called via Rcpp (Not yet implemented).
