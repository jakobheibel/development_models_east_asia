# Development Models in East Asia

## Overview
Code for the paper: **"Beyond the Developmental State: Exploring the Variety of Development Models in East Asia"** (2025) by Jakob Heibel, Jonas Dominy, and Jakob Kapeller.

This study employs a data-driven approach based on a multidimensional cluster
analysis of 15 East Asian economies across 12 macroeconomic dimensions for the period
2000-2019 to develop a concise typology of development models in East Asia.

The code for this project is based on the [code](https://github.com/dominyj/EconomicPolarizationEU2025.git) from the paper [**"Economic Polarization in the European Union: Development Models in the Race for the Best Location"** (2025) by Jonas Dominy, Claudius Gräbner-Radkowitsch, Philipp Heimberger, and Jakob Kapeller](https://www.uni-due.de/imperia/md/content/soziooekonomie/ifsowp46_dghk2025.pdf).


## Usage

First, clone the repository. The packages required to run the code are listed in the script `../packages.R`.

This project makes use of the [`here`](https://here.r-lib.org/) package to easily reference all file locations. You can install it with:

```r
install.packages("here")
```

To reproduce the datasets, as well as the tables and figures of the study, run the script `Run_Analysis.R` in the main directory of the repository, e.g. by typing in the console:

```r
source(here("Run_Analysis.R"))
```

All figures and other outputs are stored in the folder `../output`. The main dataset `macro_world.RData` is stored in the folder `../data`.

## Authors
- **Jakob Heibel**
- [**Jonas Dominy**](https://github.com/dominyj)
- [**Jakob Kapeller**](https://jakob-kapeller.org/)