<p align="center">
  <img width="497" height="700" src="https://user-images.githubusercontent.com/52540997/168085155-6f80ea54-b34d-47c3-a067-d5cc2674a93e.png">
</p>

# forester

## TL;DR

Old stand simulators are difficult to replicate, incur a language barrier, are difficult to access, use different units and names for variables and factors. By standardizing we can easily swap out data, functions, simulation models or run them side-by-side.

For style guide on R code, <https://adv-r.hadley.nz>

## Downloading and Installing from Github

I have started a git repository on Github from which anyone can download my R-package (in development), suggest edits or point out issues: <https://github.com/Silviculturalist/forester>

To install the latest development version of this package, use devtools.

```{r}
devtools::install_github("Silviculturalist/forester")
```

## Current Cooperations

PrognAus (PROGNosis for AUStria), an Austrian individual tree growth and yield model is being rewritten for R concurrently, <https://github.com/Silviculturalist/Prognaus> as a fork from Sonja Vospernik's main modules from the original PrognAus programme <https://github.com/SonjaVospernik/Prognaus> to maintain the same programming style, parameter names as Forester.

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


### Functions

To maintain order and clarity, functions should as far as possible be named as follows:

Author \_ Year \_ Dependent Variable \_ Locality \_ Species

*E.g. Eko_PM_1985_basal_area_5\_year_increment_northern_Sweden_Spruce*

When a species name is used in a function name, it should be colloquial, e.g. Pine, Spruce. When a species name is used in an argument, it should be latin binomial: "Pinus sylvestris", "Picea abies".

All functions must include a LaTeX preamble from roxygen2 which writes the help documentation. All functions must include their source.

### Variables

Variables should follow the form:

variable . unit

*E.g.* diameter.cm

A non-exhaustive list of variable names follows. These have been based on previous work from the Heureka project, Indelningspaketet, Projekt HUGIN, and other stand simulators by the forest science faculty at SLU. This does not hinder additions from expanding the expected structure, for example vegetation types, although it would be preferable to introduce new variables.

Variables should be named so as to as clearly as possible convey their meaning. *Avoid abbreviations*. Export standards for software can be written, but should not be included in this package, such as for the **StanForD 2010 standard**, used by forestry machines.

Optional arguments should take the default value NULL, see SO answer : <https://stackoverflow.com/a/28370496/11550980>

