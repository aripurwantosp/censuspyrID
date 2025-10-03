## Data Sources
The application relies on two main data sources:
- [**IPUMS International**](https://doi.org/10.18128/D020.V7.7): Indonesian population census samples for the years 1971–2010.
- [**Statistics Indonesia (Badan Pusat Statistik, BPS)**](http://sensus.bps.go.id/topik/tabular/sp2020/3): Official data on population counts by province, sex, and age from the 2020 Population Census.

## Data Pre-processing
The preparation steps to generate population counts by province, sex, and age were as follows:

- Aggregation of population counts by sex and age groups, accounting for sampling weights in the census sample data;
- Redistribution (prorating) of counts where sex or age information was missing, see [Shyrock & Siegel (1976)](https://doi.org/10.1016/B978-0-12-641150-8.50031-7);
- Final datasets were provided in three versions:
  - **Raw (non-smoothed)**
  - **Smoothed using the Arriaga method**
  - **Smoothed using the Karup-King Newton method**

The smoothing procedures were implemented using the `smooth_age_5` function from the [**DemoTools**](https://timriffe.github.io/DemoTools/articles/smoothing_with_demotools.html) package.

## Data, Code, and Labels
The datasets used in this application are accessible in R as follows:
- `data("hpop5")` for the harmonized version
- `data("ypop5")` for the non-harmonized version
- `load_pop_data()` function

Descriptions of each dataset can be accessed through:
- `?hpop5`
- `?ypop5`
- `?load_pop_data`

Reference codes and labels are available at:
- `get_code_label(1)` for census year and label
- `get_code_label(2)` for sex code and label
- `get_code_label(3)` for age (5-year group) code and label
- `get_code_label(4)` for harmonized province code and label
- `get_code_label(4)` for non-harmonized province code and label
