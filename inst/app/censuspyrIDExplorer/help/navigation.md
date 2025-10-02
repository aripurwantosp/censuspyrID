## Sidebar Menu

The sidebar menu contains widgets to control the main parameters of the explorer:

- **Mode and Province Selector**
  Two modes are available: *Harmonized* and *Non-harmonized*.
  - *Harmonized* means that provinces that were split are aggregated back into their parent province, ensuring consistent boundaries across all censuses from 1971 to 2020.
  - The list of provinces will adapt based on the selected mode.

- **Expand Phase**
  This widget appears only if *Non-harmonized* mode is selected and the chosen province has experienced a split. Options are *Before* or *After* the split.

- **Census Year Slider**
  Allows users to select the census year. If *Non-harmonized* mode is selected and the province has split, available census years will depend on the chosen *Expand Phase*.

- **Smoothing Option**
  Defines how population counts are displayed:
  - *None*: raw population data.
  - *Arriaga*: smoothed data using the Arriaga method.
  - *Karup-King-Newton*: smoothed data using the Karup-King-Newton method.

- **Color Palette**
  Provides color theme options for visualizations, using palettes from the `ggthemes` package (inspired by Canva).

---

## Pyramids Tab

This tab visualizes the population pyramid and provides a statistical summary for a selected province and year.

- Pyramid visualization can be displayed as **absolute counts** or **relative counts** (proportion/percentage).
- The population pyramid shows the age-sex structure.
- The statistical summary includes:
  - Population by sex
  - Population by age groups (0–14, 15–64, 65+)
  - Sex ratio
  - Dependency ratio

---

## Age-Profile Tab

This tab displays **age profiles**, or line curves of population distribution by age and sex.
It serves as an alternative to the pyramid for visualizing age structure and demographic dynamics.

---

## Trends Tab

This tab visualizes trends in the population structure for a selected province. Two types of visualizations are available:

- **Pyramid**: displayed either as a *grid* or *overlay*.
- **Area**: displayed by sex, with options:
  - *All* (male and female together)
  - *Male*
  - *Female*
  - *Male + Female*

Visualization can use either absolute counts or relative counts (proportion/percentage).

If *Non-harmonized* mode is selected and the province has split, the visualization will follow the chosen *Expand Phase* (before or after the split).

---

## Data Tab

This tab provides population data in a dynamic table format for the selected province and year.
Users can export the data as a `.csv` file by clicking the **Download** button.

---

## Help Tab

The *Help* section contains four sub-tabs:

- **Navigation**: This page (overview of menus and features).
- **Metadata**: Explains data sources, processing methods, reference codes, and labels.
- **Province Trajectory**: Visualizes the trajectory of provinces that experienced splits using a Sankey diagram.
- **About**: Provides information about the application, acknowledgements, authors, and citation details.
