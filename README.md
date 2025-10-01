# censuspyrID <img src="https://www.r-project.org/Rlogo.png" align="right" height="120"/>

**An R package for Indonesian Population Pyramids from Harmonized and Unharmonized Census Data**

---

## Overview

`censuspyrID` provides harmonized and unharmonized population pyramid datasets
from the Indonesian population censuses (1971–2020).
It includes functions for loading, filtering, and visualizing population data,
as well as an **interactive Shiny application** for exploring demographic structures
across provinces and census years.

The harmonized dataset (`hpop5`) uses consistent province codes across censuses
to enable long-term trend analysis despite administrative expansion
(*pemekaran*), while the unharmonized dataset (`ypop5`) retains original province
codes as published in each census year.

Data sources include:

- **IPUMS International (1971–2010)** — [IPUMS International](https://doi.org/10.18128/D020.V7.6)
- **Population Census 2020 (BPS Indonesia)** — [BPS Census Portal](http://sensus.bps.go.id/topik/tabular/sp2020/3)

Both datasets were processed with steps including aggregation into 5-year age groups,
pro-rata redistribution for missing attributes, and demographic smoothing
(Arriaga and Karup–King–Newton methods).

---

## Installation

You can install the development version of `censuspyrID` from GitHub using [`remotes`](https://cran.r-project.org/package=remotes):

```r
# install remotes if not already installed
install.packages("remotes")

# install censuspyrID from GitHub
remotes::install_github("aripurwantosp/censuspyrID")
```
To start the explorer, from R do
```r
library(censuspyrID)
censuspyrID_explorer()

```
This will open the interface in your default web browser.
