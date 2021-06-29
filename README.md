# forester

## TL;DR

Old stand simulators are difficult to replicate, incur a language barrier, are difficult to access, use different units and names for variables and factors. By standardizing the structure in which data is stored in forestry and enforcing a modular approach we can easily swap out data, functions, simulation models or run them simultaneously.

For style guide on R code, <https://adv-r.hadley.nz>

For git interaction with R, <https://happygitwithr.com/index.html>

As a GIT client, I use: <https://www.gitkraken.com>

## Downloading and Installing from Github

I have started a git repository on Github from which anyone can download my R-package (in development), suggest edits or point out issues: <https://github.com/Silviculturalist/forester>

To install the latest development version of this package, use devtools.

```{r}
devtools::install_github("Silviculturalist/forester")
```

## Goal

I hope that this will become a good digital centre for maintaining our valuable research heritage in the digital era, and will work hand-in-hand with digitization efforts from the SLU library to ensure that source material is not only referenced in the documentation, but in the future actively linked to.

The goal of this github repository is to be a centre to:

-   Digitalise

-   Review, discuss, validate and update functions.

-   Maintain version control and good documentation;

I am continuing to develop object structures for which R will recognize attributes which are related to trees, stand information, site information, and treatment information, such that this may grow into a stand simulator on its' own.

As work with the package progresses, it is my full intent that this should not be limited to Swedish conditions, but should eventually form an open online repository for functions related to forest science, forestry and related data from all over the world. Some models from Norway, e.g. Allen et al. (2020) are already incorporated.

## Why?

As such, it will remedy several of the issues I have noticed with earlier stand simulators :

-    Documentation of R packages is stricter and more structured.

-    Issues can be recognized and solved open-source.

-    The user will have full control over all processes (no black-box functionality!)

-    The material is less at risk to become obsolete through technical issue.

-    Trust in the collection is gradually increased and maintained by its' transparency.

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

All functions must include a LaTeX preamble from roxygen2 which writes the help documentation. All functions must include their source.

### Variables

Variables should follow the form:

variable . unit

*E.g.* diameter.cm

A non-exhaustive list of variable names follows, taken from the treelistData file. These have been based on previous work from the Heureka project, Indelningspaketet, Projekt HUGIN, and other stand simulators by the forest science faculty at SLU. This does not hinder additions from expanding the expected structure, for example vegetation types, although it would be preferable to introduce new variables.

#### Tree Level Variables

| Variable             | Description                                                                                                  |
|----------------------|--------------------------------------------------------------------------------------------------------------|
| treeID               | The unique ID of a tree.                                                                                     |
| origin_year          | Year of planting                                                                                             |
| observation_year     | Year of observation.                                                                                         |
| generation           | Does this tree belong to the current generation of management? 1==current, 2==previous, 3..                  |
| species              | Tree species, in Latin, excl. auktor. e.g. "Picea abies", "Pinus sylvestris".                                |
| age                  | Tree total age.                                                                                              |
| age_at_breast_height | Tree age at 1.3 m. Total age - time to breast height.                                                        |
| height.m             | Tree height in metres.                                                                                       |
| diameter.cm          | Tree diameter in centimetres.                                                                                |
| bark.mm              | Bark thickness, in mm.                                                                                       |
| double_bark.mm       | Tree bark thickness, in mm.                                                                                  |
| crown.base.height.m  | Height to the lower living green branch not separated from the rest of the crown by more than 2 (3?) whorls. |
| basal_area           | Basal area of the tree in m^2.                                                                               |
| volume               | Tree volume, m3sk.                                                                                           |
| coord_x              | X coordinate.                                                                                                |
| coord_y              | Y coordinate.                                                                                                |

#### Site Level Variables

| Variable             | Description                                                                                                                                              |
|----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| stand_id             | Stand ID                                                                                                                                                 |
| main_species         | Main species in latin, e.g. "Picea abies" "Pinus sylvestris".                                                                                            |
| distance_to_coast.km | Distance to the nearest Swedish coast. If missing, will be calculated by forester::coast_distance.                                                       |
| aspect_main          | Main aspect (N, E, S, W, or 0).                                                                                                                          |
| incline_percent      | If missing, 0.                                                                                                                                           |
| altitude             | Meters above sea level                                                                                                                                   |
| latitude             | Latitude decimal WGS84 EPSG:4326.                                                                                                                        |
| longitude            | Longitude decimal WGS84 EPSG:4326.                                                                                                                       |
| polygon              | Optional list of boundary conditions. Warning siteData does not check validity of polygon geometry.                                                      |
| county               | County in Sweden. If not supplied, will calculate through forester::county_sweden. N.B. Due to the age of functions, these counties are not the current. |
| local_climate        | Swedish Local Climate Code, cf. Ångström 1958. If not supplied will attempt to calculate through forester::climate_code_sweden                           |
| vegetation           | 1-18. Variable indicating field layer vegetation type (NFI vegetation code FSkod). See below.                                                            |
| ground_layer         | 1-6. Variable indicating ground layer vegetation. See below.                                                                                             |
| soil_texture         | 1-9. See below.                                                                                                                                          |
| soil_moisture        | 1-5. See below.                                                                                                                                          |
| soil_depth           | 1-4. See below.                                                                                                                                          |
| lateral_water        | 1-5. See below.                                                                                                                                          |
| ditched              | TRUE/FALSE if site affected by ditching.                                                                                                                 |
| temperature_sum.c    | Degrees celsius. If not supplied, will calculate from forester::odin_tsum_1983.                                                                          |

| Field Layer Code | Description                 |
|------------------|-----------------------------|
| 1                | Tall herbs w/o dwarf shrubs |
| 2                | Tall herbs with bilberry    |
| 3                | Tall herbs with cowberry    |
| 4                | Low herbs w/o dwarf shrubs  |
| 5                | Low herbs with bilberry     |
| 6                | Low herbs with cowberry     |
| 7                | No field layer              |
| 8                | broad-leaved grasses        |
| 9                | narrow-leaved grasses       |
| 10               | Sedge, tall                 |
| 11               | Sedge, low                  |
| 12               | Horsetail                   |
| 13               | Bilberry                    |
| 14               | Cowberry                    |
| 15               | Crowberry                   |
| 16               | Poor shrubs                 |
| 17               | Lichen-rich                 |
| 18               | Lichen-dominated.           |

| Ground Layer Code | Description               |
|-------------------|---------------------------|
| 1                 | Lichen type               |
| 2                 | Lichen-rich bogmoss type  |
| 3                 | Lichen rich               |
| 4                 | Bogmoss type (*Sphagnum*) |
| 5                 | Swamp moss type           |
| 6                 | Fresh moss type.          |

Soil texture same as from the Heureka project, see : <https://www.heurekaslu.se/wiki/Definition:SoilTextureCode>

| Soil Moisture Code | Description                |
|--------------------|----------------------------|
| 1                  | Dry / torr                 |
| 2                  | Mesic / frisk              |
| 3                  | Mesic-moist / frisk-fuktig |
| 4                  | Moist / fuktig             |
| 5                  | Wet / blöt.                |

| Soil Depth Code | Description              |
|-----------------|--------------------------|
| 1               | Deep, \>70 cm            |
| 2               | Rather shallow, 20-70 cm |
| 3               | Shallow \<20 cm          |
| 4               | Varying                  |

| Lateral Water Code | Description     |
|--------------------|-----------------|
| 1                  | Missing         |
| 2                  | Seldom          |
| 3                  | Shorter Periods |
| 4                  | Longer periods  |
| 5                  | Slope           |
