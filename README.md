# censuspyrID <img src="https://www.r-project.org/Rlogo.png" align="right" height="120"/>

**Explorer of Indonesian Population Pyramids from Harmonized and Non-harmonized Census Data**

---

## Overview

`censuspyrID` R package provides harmonized and unharmonized population pyramid datasets
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

## Installation & Run

You can install the development version of `censuspyrID` from GitHub using [`remotes`](https://cran.r-project.org/package=remotes):

```r
# install remotes if not already installed
install.packages("remotes")

# install censuspyrID from GitHub
remotes::install_github("aripurwantosp/censuspyrID")
```
The core feature of this package is the `censuspyrID_explorer()`, an interactive Shiny application that allows you to visually explore population pyramids, age profiles, and demographic trends across different provinces, census years, and smoothing methods., from R do
```r
# load package
library(censuspyrID)

# launch the interactive application
censuspyrID_explorer()

```
The `censuspyrID_explorer()` function launches the application in your default web browser. See the Help menu within the application for a detailed navigation guide.

---

## Functions

Besides the interactive Shiny application, **censuspyrID** also provides several 
functions that you can use directly in your R scripts for data processing and visualization:

| Function             | Description |
|----------------------|-------------|
| `load_pop_data()`    | Loads the main population datasets (`hpop5` or `ypop5`) with optional demographic smoothing applied (Arriaga or Karup–King–Newton). |
| `pop_data_by_year()` | Filters the data for a specific census year. |
| `pop_data_by_reg()`  | Filters the data for a specific province ID. |
| `pyr_single()`       | Creates a single population pyramid plot for a given region and year. |
| `pyr_trends()`       | Generates trend plots of population pyramids over multiple census years. |
| `area_trends()`      | Plots population proportions across three broad age groups (young, working-age, old) over time. |
| `pop_summary()`      | Prints a formatted statistical summary, including sex ratio and dependency ratios. |

### Example Usage

```r
library(censuspyrID)

# Load harmonized population data with Arriaga smoothing
pop_data <- load_pop_data(harmonized = TRUE, smoothing = "arriaga")

# Filter data for the 2020 census
pop_2020 <- pop_data_by_year(pop_data, year = 2020)

# Filter data for a specific province (e.g., DKI Jakarta, province_id = 31)
pop_jakarta <- pop_data_by_reg(pop_2020, reg = 31)

# Create a single population pyramid for Jakarta in 2020
pyr_single(pop_jakarta, reg_code = 31, year = 2020)

# Generate pyramid trends for Jakarta across all census years
pyr_trends(pop_data, reg_code = 31)

# Plot age-structure trends (0-14, 15-64, 65+) for Jakarta
area_trends(pop_data, reg_code = 31)

# Print a summary with sex ratio and dependency ratios
pop_summary(pop_jakarta)
```
---

## Authors

### Ari Purwanto Sarwo Prasojo
📧 ari.prasojo18@gmail.com | arip003@brin.go.id
[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--4862--5523-brightgreen?logo=orcid&logoColor=white)](https://orcid.org/0000-0002-4862-5523)

Ari is a Junior Researcher at the Research Center for Population, National Research and Innovation Agency (Badan Riset dan Inovasi Nasional – BRIN). His research focuses on quantitative and computational social demography, particularly on resilience and human development.

He holds a **Bachelor of Science in Statistics** from Brawijaya University, Malang (2016), and a **Master’s degree in Population and Labor Economics (M.E.K.K.)** from the Faculty of Economics and Business, University of Indonesia (2022).

---

### Puguh Prasetyoputra
📧 pprasetyoputra@gmail.com | pugu004@brin.go.id
[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--5494--7003-brightgreen?logo=orcid&logoColor=white)](https://orcid.org/0000-0001-5494-7003)

Puguh is a Senior Researcher at the Research Center for Population, National Research and Innovation Agency (BRIN). He describes himself as an "analytical wordsmith, crafting stories with data as the pen and curiosity as the ink."

He holds a **Master of Health Economics** from The University of Queensland, Australia (2012), and a **Bachelor of Economics** from Bogor Agricultural University (2010). His research interests include Health Demography, Health Economics, Health and Development, Nutrition and Health, and Household Surveys.

---

### Nur Fitri Mustika Ayu
📧 nurfitrimustikaayu@gmail.com

Nur is a Business Statistics student at the Sepuluh Nopember Institute of Technology (ITS), currently in her sixth semester. She has a strong background in data analysis, statistical methods, and their application in business decision-making.

Her skills include quantitative analysis, big data processing, and the use of statistical tools such as R, Python, and SQL. **censuspyrID** was developed as part of her internship project at the Research Center for Population, National Research and Innovation Agency (BRIN).

---

### Citation

**APA style**:
Prasojo, A. P. S., Prasetyoputra, P., & Ayu, N. F. M. (2025). *censuspyrID: Harmonized and Unharmonized Indonesia Population Pyramid from Census Data*.

**BibTeX**:
```bibtex
@misc{censuspyrid2025,
  author       = {Prasojo, Ari Purwanto Sarwo and Prasetyoputra, Puguh and Ayu, Nur Fitri Mustika},
  title        = {censuspyrID: Harmonized and Unharmonized Indonesia Population Pyramid from Census Data},
  year         = {2025}
}
