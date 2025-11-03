# Development Models in East Asia

## Overview
Code for the paper: **"Beyond the Developmental State: Exploring the Variety of Development Models in East Asia"** (2025) by Jakob Heibel, Jonas Dominy, and Jakob Kapeller.

This study employs a data-driven approach based on a multidimensional cluster
analysis of 15 East Asian economies across 12 macroeconomic dimensions for the period
2000-2019 to develop a concise typology of development models in East Asia.

The [code](https://github.com/dominyj/EconomicPolarizationEU2025.git) for this project is based on the code from the paper [**"Economic Polarization in the European Union: Development Models in the Race for the Best Location"** (2025) by Jonas Dominy, Claudius Gräbner-Radkowitsch, Philipp Heimberger, and Jakob Kapeller](https://www.uni-due.de/imperia/md/content/soziooekonomie/ifsowp46_dghk2025.pdf).

## Installation
To use this project, first clone the repository and restore the required R packages using `renv`:

```r
renv::restore()
```

If `renv` is not installed, you can install it with:

```r
install.packages("renv")
```

## Usage

To reproduce the datasets, as well as the tables and figures of the study, run the following script in the main folder.

```r
source("Run_Analysis.R")
```

All figures are stored in the folder ../output.
The main dataset macro_world.RData is stored in the folder ../data
The packages required to run the code are listed in the script ../packages.R

## Authors
- **Jakob Heibel**
- [**Jonas Dominy**](https://github.com/dominyj)
- [**Jakob Kapeller**](https://jakob-kapeller.org/)