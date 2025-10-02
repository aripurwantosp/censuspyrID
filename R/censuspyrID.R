utils::globalVariables('censuspyrID_data_env')

#' Explore Harmonized and Non-harmonized Population Pyramids from Indonesia’s Censuses (1971–2020)
#'
#' @description
#' Launches **censuspyrID Explorer**, a Shiny application for visualizing harmonized and
#' non-harmonized population pyramids from Indonesia’s population censuses (1971–2020).
#'
#' @details
#' The application provides interactive tools to explore demographic structures across provinces
#' and census years. See the *Help* menu within the application for a navigation guide.
#'
#' @param host Character string passed to \code{\link[shiny:runApp]{runApp}}.
#'   Default is `"0.0.0.0"`.
#' @param ... Additional arguments passed to \code{\link[shiny:runApp]{runApp}}.
#'
#' @return
#' The function launches the Shiny application. It does not return a value.
#'
#' @examples
#' \dontrun{
#' censuspyrID_explorer()
#' }
#'
#' @export
censuspyrID_explorer <- function(host = NULL, ...){
  if(missing(host)) host <- getOption("shiny.host", "0.0.0.0")
  shiny::runApp(system.file("app", "censuspyrIDExplorer", package="censuspyrID"),
                host = host, ...)
}


# Main ----

## For sidebar or parameter components ----

#' Create region (province) list
#'
#' Internal helper function to generate a list of provinces,
#' depending on whether harmonized or non-harmonized coding is used.
#'
#' @param harmonized Logical. If `TRUE` (default), use harmonized province codes.
#'   If `FALSE`, use non-harmonized province codes.
#'
#' @return A named vector, where values are province IDs and names are
#'   corresponding province labels.
#'
#' @details
#' - If `harmonized = TRUE`, the function uses `ref_label$provinceh_label`.
#' - If `harmonized = FALSE`, the function uses `ref_label$province_label`.
#'
#' @keywords internal
reg_list <- function(harmonized = TRUE){
  # harmonized <- ifelse(harmonized == 1, TRUE, FALSE)
  if(harmonized){
    lst <- structure(
      ref_label$provinceh_label$province_id_h,
      names = ref_label$provinceh_label$label
    )
  }else{
    lst <- structure(
      ref_label$province_label$province_id_y,
      names = ref_label$province_label$label
    )
  }
  return(lst)
}

#' Check Province Expansion Status
#'
#' This function checks whether a given province code corresponds
#' to a province that has been expanded (i.e., administratively split
#' or modified).
#'
#' @param reg_code non-harmonized province code (character or numeric).
#'
#' @return A logical value:
#' \itemize{
#'   \item `TRUE` if the province is marked as expanded,
#'   \item `FALSE` otherwise.
#' }
#'
#' @details
#' The function looks up the internal dataset \code{prov_coverage}.
#' Expansion status is determined by the field \code{expanded}.
#'
#' @seealso [get_code_label()]
#'
#' @examples
#' # Example: check expansion status of a province
#' get_code_label(5) #returns list of non-harmonized province code
#' is_expanded(1400)   # returns TRUE/FALSE for Riau province
#'
#' @export
is_expanded <- function(reg_code){
  if(length(reg_code) == 0){
    isexpanded <- FALSE
  }else{
    info <- prov_coverage |> filter(code == reg_code)
    isexpanded <- ifelse(!is.na(info$expanded), TRUE, FALSE)
  }
  return(isexpanded)
}

#' Get Census Year Coverage for a Province
#'
#' This function determines the range of census years available for a given
#' province. Coverage depends on whether harmonized or non-harmonized codes are
#' used, and in the case of non-harmonized data, whether the province has
#' experienced administrative expansion (pemekaran).
#'
#' @param reg_code Character or numeric. Province code. Required if
#'   \code{harmonized = FALSE}.
#' @param harmonized Logical. If \code{TRUE} (default), returns harmonized
#'   coverage (1971–2020). If \code{FALSE}, uses non-harmonized coverage.
#' @param before_expand Logical. Only relevant if \code{harmonized = FALSE}
#'   and the province has expanded. If \code{TRUE} (default), returns coverage
#'   before expansion; if \code{FALSE}, returns coverage after expansion.
#'
#' @return An integer vector of census years, with labels as names.
#'
#' @details
#' \itemize{
#'   \item For harmonized data (\code{harmonized = TRUE}), the full coverage
#'         of 1971–2020 is returned.
#'   \item For non-harmonized data (\code{harmonized = FALSE}), coverage is
#'         determined based on the internal dataset \code{prov_coverage}.
#'   \item If the province has expanded, coverage depends on
#'         \code{before_expand}.
#'   \item Census year labels are retrieved from
#'         \code{ref_label$census_label}.
#' }
#'
#' @seealso [is_expanded()], [get_code_label()]
#'
#' @examples
#' \dontrun{
#' # Harmonized coverage (1971–2020)
#' year_range(harmonized = TRUE)
#'
#' # non-harmonized coverage for a province (before expansion)
#' get_code_label(5) #returns list of non-harmonized province code
#' year_range(reg_code = 1400, harmonized = FALSE, before_expand = TRUE)
#'
#' # non-harmonized coverage for a province (after expansion)
#' year_range(reg_code = 1400, harmonized = FALSE, before_expand = FALSE)
#' }
#'
#' @export
year_range <- function(reg_code=NULL, harmonized = TRUE, before_expand = TRUE){
  if(harmonized){
    year <- structure(
      c(1971, 2020),
      names = c("min","max")
      )
  }else{
    covinfo <- prov_coverage |> filter(code == reg_code)
    if(is_expanded(reg_code)){
      # before <- ifelse(expand_phase == 1, TRUE, FALSE)
      if(before_expand){
        year <- structure(
          c(covinfo$year_min1, covinfo$year_max1),
          names = c("min","max")
          )
      }else{
        year <- structure(
          c(covinfo$year_min2, covinfo$year_max2),
          names = c("min","max")
          )
      }
    }else{
      year <- structure(
        c(covinfo$year_min1, covinfo$year_max1),
        names = c("min","max")
        )
    }
  }

  years <- structure(
    ref_label$census_label$year,
    names = ref_label$census_label$label
  )
  years <- years[years >= year[1] & years <= year[2]]

  return(years)
}

## function for select (load) data ----

#' Load Population Data
#'
#' @description
#' Load census population data with options for harmonization and smoothing.
#' Returns population counts by year, province, sex, and five-year age group,
#' with raw or smoothed estimates depending on the selected method.
#'
#' @details
#' Data are retrieved from internal census datasets:
#' - `hpop5`: harmonized census data
#' - `ypop5`: non-harmonized census data
#'
#' Smoothing methods are applied to the population counts:
#' - `1`: none (raw, default)
#' - `2`: Arriaga method
#' - `3`: Karup–King–Newton (KKN) method
#'
#' @param harmonized Logical. If `TRUE` (default), load harmonized data
#'   (`hpop5`). If `FALSE`, load non-harmonized data (`ypop5`).
#' @param smoothing Integer. Smoothing method applied to population counts:
#'   * `1`: none (raw)
#'   * `2`: Arriaga
#'   * `3`: Karup–King–Newton (KKN)
#'
#' @return A tibble with columns:
#' - `year`: census year
#' - `province_id`: province identifier (harmonized or non-harmonized)
#' - `sex`: sex code
#' - `age5`: five-year age group code
#' - `pop`: population count (raw or smoothed)
#'
#' @seealso [pop_data_by_year()], [pop_data_by_reg()]
#'
#' @examples
#' \dontrun{
#' # Load harmonized, raw (unsmoothed) population data
#' load_pop_data(harmonized = TRUE, smoothing = 1)
#'
#' # Load non-harmonized, Arriaga-smoothed population data
#' load_pop_data(harmonized = FALSE, smoothing = 2)
#' }
#'
#' @export
load_pop_data <- function(harmonized = TRUE, smoothing = 1){
  pop <- smooth_name(smoothing)
  if(harmonized){
    data <- censuspyrID_data_env$hpop5 |>
      dplyr::select(
        year, province_id = province_id_h, sex, age5,
        pop = all_of(pop)
        )
  }else(
    data <- censuspyrID_data_env$ypop5 |>
      dplyr::select(
        year, province_id = province_id_y, sex, age5,
        pop = all_of(pop)
        )
  )
  return(data)
}

#' Filter Population Data by Year
#'
#' @description
#' Filter population data for a specific census year. This function is intended
#' for use with population datasets loaded via [load_pop_data()], but can work
#' with any data frame that contains a `year` column.
#'
#' @param data A data frame or tibble containing population data.
#'   Must include a column named `year`.
#' @param yr Integer or numeric. The census year to filter by.
#'
#' @return A tibble (or data frame) containing only rows from the specified year.
#'
#' @seealso [load_pop_data()], [pop_data_by_reg()]
#'
#' @export
#'
#' @examples
#' # Load harmonized data first
#' dat <- load_pop_data(harmonized = TRUE, smoothing = 1)
#'
#' # Filter for the 2000 census year
#' pop_data_by_year(dat, 2000)
pop_data_by_year <- function(data, yr){
  data <- data |> filter(year == yr)
  return(data)
}

#' Filter Population Data by Province
#'
#' @description
#' Filter population data based on a specified province ID. This function is
#' intended for use with population datasets loaded via [load_pop_data()],
#' but can work with any data frame that includes a `province_id` column.
#'
#' @param data A data frame or tibble containing population data.
#'   Must include a column named `province_id`.
#' @param reg Integer or character. The province ID to filter by.
#'
#' @return A tibble (or data frame) containing only rows for the specified province.
#'
#' @seealso [load_pop_data()], [pop_data_by_year()]
#'
#' @export
#'
#' @examples
#' # Load harmonized data
#' dat <- load_pop_data(harmonized = TRUE, smoothing = 1)
#'
#' # Filter data for province ID 0 (Indonesia)
#' pop_data_by_reg(dat, reg = 0)
pop_data_by_reg <- function(data, reg){
  data <- data |> filter(province_id == reg)
  return(data)
}

## function for returning label ----

#' Get Province Name from Code
#'
#' @description
#' Internal helper function to retrieve the province name corresponding to
#' a given province code. Works with either harmonized or non-harmonized codes.
#'
#' @param code Integer or character. The province code to look up.
#' @param harmonized Logical. If `TRUE` (default), the function searches using
#'   harmonized province codes. If `FALSE`, it uses non-harmonized province codes.
#'
#' @return A character string with the corresponding province name.
#'
#' @details
#' This function relies on the internal object `ref_label`, which must contain
#' the reference tables:
#' * `provinceh_label` for harmonized codes (`province_id_h`, `label`).
#' * `province_label` for non-harmonized codes (`province_id_y`, `label`).
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' reg_name(31)          # returns harmonized province name
#' reg_name(3100, FALSE)   # returns non-harmonized province name
#' }
reg_name <- function(code, harmonized = TRUE){
  if(harmonized){
    name <- ref_label$provinceh_label |>
      filter(province_id_h == code) |>
      pull(label)
  }else{
    name <- ref_label$province_label |>
      filter(province_id_y == code) |>
      pull(label)
  }
  # print(name)
  return(name)
}

#' Get Smoothing Method Name
#'
#' Internal helper function to return the name of the smoothing method
#' based on the provided smoothing code.
#'
#' @param smoothing A numeric value indicating the smoothing method:
#'   * `1` = non-smoothed (`"ns"`)
#'   * `2` = Arriaga (`"arriaga"`)
#'   * `3` = Karup–King–Newton (`"kkn"`)
#'
#' @return A character string with the name of the smoothing method.
#'
#' @keywords internal
smooth_name <- function(smoothing = 1){
  name <- switch(
    smoothing,
    "1" = "ns",
    "2" = "arriaga",
    "3" = "kkn"
  )
  return(name)
}


# Pyramids ----

#' Build a Single Population Pyramid
#'
#' Create a population pyramid for a given dataset (specific province and year),
#' either in absolute counts or in proportions, with customizable color palettes.
#'
#' @param data A data frame containing population data for a specific province
#'   and year. Must include variables `sex`, `age5`, and `pop`.
#' @param use_prop Logical, default `FALSE`. If `TRUE`, the pyramid will be shown
#'   in proportions instead of absolute counts.
#' @param color Character string indicating the color palette name to use for
#'   the pyramid. Available palettes come from
#'   [ggthemes::canva_palettes], e.g., `"Fresh and bright"`.
#'
#' @return A `ggplot` object representing the population pyramid.
#'
#' @seealso [ageprof()], [pyr_trends()], [load_pop_data()], [pop_data_by_reg()], [pop_data_by_year()], [get_code_label()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data for Indonesia, 2020
#' data_idn <- pop_data_by_year(load_pop_data(), 2020) |>
#'   pop_data_by_reg(0) #Indonesia
#'
#' # Absolute count pyramid
#' pyr_single(data_idn)
#'
#' # Proportional pyramid with different palette
#' pyr_single(data_idn, use_prop = TRUE, color = "Professional and modern")
#' }
pyr_single <- function(data, use_prop = FALSE, color = "Fresh and bright"){

  # make sex and age as factor
  # age <- unique(data$age5)
  # len_age <- length(age)
  # lab_age <- c(ref_label$age5_label$label[1:(len_age-1)], paste0(age[len_age],"+"))

  data <- data |>
    mutate(
      sex = factor(sex, levels = ref_label$sex_label$sex,
                   labels = ref_label$sex_label$label)
      )

  # value label and scale
  y_lab <- "Count (in thousands)"
  scl_fun <- function(y) abs(y)/1000

  # if use_prop
  if(use_prop){
    data <- data |>
      group_by(sex) |>
      mutate(pop = pop/sum(pop)) |>
      ungroup()

    y_lab <- "Proportion"
    scl_fun <- function(y) abs(y)
  }

  data <- data |>
    mutate(pop = ifelse(sex == "Male", -1*pop, pop))

  # make a plot
  plt <- data |>
    ggplot(aes(x = age5, y = pop, group = interaction(sex, age5))) +
    geom_bar(aes(fill = sex), stat = "identity") +
    labs(x = "Age", y = y_lab, fill = "") +
    scale_y_continuous(labels = scl_fun, breaks = pretty_breaks()) +
    coord_flip() +
    scale_fill_canva(palette = color) + # canva color palette
    theme(
      legend.position = "bottom"
    )

  return(plt)

}

#' Print Population Summary Statistics
#'
#' @description
#' Generate and print a formatted summary of population counts, percentages,
#' sex ratio, and dependency ratios from a given dataset of population data
#' for a specific province and year.
#'
#' @details
#' The function calculates:
#' \itemize{
#'   \item Total population
#'   \item Male and female population counts and percentages
#'   \item Age group distribution: 0–14, 15–64, and 65+ (counts and percentages)
#'   \item Sex ratio (males per 100 females)
#'   \item Dependency ratios (0–14, 65+, and total dependency ratio relative to 15–64)
#' }
#'
#' Results are printed directly to the console in a formatted table.
#'
#' @param data A data frame of population data for a specific province and year,
#'   containing at least the variables:
#'   \code{pop} (population count),
#'   \code{sex} (coded as 1 = male, 2 = female),
#'   \code{age5} (5-year age groups).
#'
#' @return
#' This function does not return an object. It prints formatted summary statistics
#' to the console.
#'
#' @seealso [load_pop_data()], [pop_data_by_reg()], [pop_data_by_year()], [get_code_label()]
#'
#' @examples
#' \dontrun{
#' # Example: population summary for Indonesia, 2020
#' data_idn <- pop_data_by_year(load_pop_data(), 2020) |>
#'   pop_data_by_reg(0) # Indonesia
#' pop_summary(data_idn)
#' }
#'
#' @export
pop_summary <- function(data){

  # pop count
  pop_total <- sum(data$pop)
  pop_male <- data |> filter(sex == 1) |>
    pull(pop) |> sum()
  pop_female <- data |> filter(sex == 2) |>
    pull(pop) |> sum()
  pop_014 <- data |> filter(age5 %in% seq(0,10,5)) |>
    pull(pop) |> sum()
  pop_1564 <- data |> filter(age5 %in% seq(15,60,5)) |>
    pull(pop) |> sum()
  pop_65p <- data |> filter(age5 >= 65) |>
    pull(pop) |> sum()

  # pop percent
  perc_pop_total <- pop_total/pop_total*100
  perc_pop_male <- pop_male/pop_total*100
  perc_pop_female <- pop_female/pop_total*100
  perc_pop_014 <- pop_014/pop_total*100
  perc_pop_1564 <- pop_1564/pop_total*100
  perc_pop_65p <- pop_65p/pop_total*100

  # sex ratio
  sex_ratio <- pop_male/pop_female*100

  # dependency ratio
  dr_014 <- pop_014/pop_1564*100
  dr_65p <- pop_65p/pop_1564*100
  dr_total <- dr_014 + dr_65p

  # formatter
  fmt_num <- function(x) formatC(x, format = "d", big.mark = ",")
  fmt_perc <- function(x) formatC(x, format = "f", digits = 2, width = 6)
  fmt_pct <- function(x) paste0(fmt_perc(x), "%")

  # print
  cat("===============================\n")
  cat("Population summary\n")
  cat("===============================\n\n")

  cat("Population count:\n")
  cat("-------------------------------\n")
  cat(sprintf("Total   : %10s (%6s)\n", fmt_num(pop_total),  fmt_pct(perc_pop_total)))
  cat(sprintf("Male    : %10s (%6s)\n", fmt_num(pop_male),   fmt_pct(perc_pop_male)))
  cat(sprintf("Female  : %10s (%6s)\n", fmt_num(pop_female), fmt_pct(perc_pop_female)))
  cat(sprintf("0-14    : %10s (%6s)\n", fmt_num(pop_014),    fmt_pct(perc_pop_014)))
  cat(sprintf("15-64   : %10s (%6s)\n", fmt_num(pop_1564),   fmt_pct(perc_pop_1564)))
  cat(sprintf("65+     : %10s (%6s)\n", fmt_num(pop_65p),    fmt_pct(perc_pop_65p)))
  cat("\n")

  cat("Sex ratio:\n")
  cat("-------------------------------\n")
  cat(sprintf("SR      : %6s\n\n", fmt_pct(sex_ratio)))

  cat("Dependency ratio:\n")
  cat("-------------------------------\n")
  cat(sprintf("0-14    : %6s\n", fmt_pct(dr_014)))
  cat(sprintf("65+     : %6s\n", fmt_pct(dr_65p)))
  cat(sprintf("Total   : %6s\n", fmt_pct(dr_total)))
}


# Age-profile ----

#' Build Age-Profile Plot by Sex
#'
#' @description
#' Create a line plot of population age profiles (5-year age groups) for a given
#' province and year, with optional logarithmic scale. The plot is faceted by sex.
#'
#' @details
#' The function produces an age-profile line chart where:
#' \itemize{
#'   \item X-axis: Age (5-year groups).
#'   \item Y-axis: Population counts (in thousands by default).
#'   \item Separate lines are drawn for males and females.
#'   \item Users can choose logarithmic scaling of the Y-axis.
#' }
#'
#' @param data A data frame of population data for a specific province and year,
#'   containing at least the variables:
#'   \code{pop} (population count),
#'   \code{sex} (coded as 1 = male, 2 = female),
#'   \code{age5} (5-year age groups).
#' @param log_scale Logical; whether to use a logarithmic scale for the Y-axis.
#'   Default is \code{FALSE}.
#' @param color Character; the name of a Canva color palette available in
#'   \code{ggthemes::canva_palettes}. Default is \code{"Fresh and bright"}.
#'
#' @return
#' A \code{ggplot2} object representing the age-profile plot, faceted by sex.
#'
#' @seealso [pyr_single()], [load_pop_data()], [pop_data_by_reg()], [pop_data_by_year()], [get_code_label()]
#'
#' @examples
#' \dontrun{
#' # Example: age profile for Indonesia, 2020
#' data_idn <- pop_data_by_year(load_pop_data(), 2020) |>
#'   pop_data_by_reg(0) # Indonesia
#' ageprof(data_idn)
#'
#' # Example with log scale
#' ageprof(data_idn, log_scale = TRUE)
#' }
#'
#' @export
ageprof <- function(data, log_scale = FALSE, color = "Fresh and bright"){

  # make sex and age as factor
  # age <- unique(data$age5)
  # len_age <- length(age)
  # lab_age <- c(ref_label$age5_label$label[1:(len_age-1)], paste0(age[len_age],"+"))

  data <- data |>
    mutate(
      sex = factor(sex, levels = ref_label$sex_label$sex,
                   labels = ref_label$sex_label$label)
    )

  # value label and scale
  y_lab <- "Count (in thousands)"
  trans_fun <- trans_new(
    name = "trans_fun",
    transform = function(y) y/1000,
    inverse = function(y) y*1000
  )
  lab_fun <- function(y) round(y/1000, 2)

  if(log_scale){
    y_lab <- "Log of count"
    trans_fun <- log_trans(base = exp(1))
    lab_fun <- function(y) round(log(y), 2)
  }

  # make a plot
  plt <- data |>
    ggplot(aes(x = age5, y = pop, group = 1)) +
    geom_line(aes(color = sex), linewidth = 0.7) +
    scale_y_continuous(
      trans = trans_fun,
      labels = lab_fun,
      breaks = pretty_breaks()
      ) +
    labs(x = "Age", y = y_lab, color = "") +
    scale_color_canva(palette = color) + # canva color palette
    guides(color = "none") +
    facet_grid(sex ~ .)

  return(plt)

}


# Trends ----

#' Build Population Pyramid Trends
#'
#' @description
#' Create trend plots of population pyramids over multiple census years for a
#' given region. Users can choose between a grid layout of pyramids or an
#' overlay of age profiles across years.
#'
#' @details
#' Two visualization modes are available:
#' \itemize{
#'   \item \code{mode = 1}: Grid of population pyramids (faceted by year).
#'   \item \code{mode = 2}: Overlayed age profiles with separate lines by year.
#' }
#'
#' Population counts can be displayed either as absolute numbers (default, in
#' thousands) or as proportions (\code{use_prop = TRUE}).
#'
#' @param data A data frame of population data for a specific region across
#'   census years, containing at least:
#'   \code{year}, \code{age5}, \code{sex}, and \code{pop}.
#' @param mode Integer; visualization mode: \code{1} for grid pyramids, \code{2}
#'   for overlayed age profiles. Default is \code{1}.
#' @param use_prop Logical; whether to show proportions instead of absolute
#'   counts. Default is \code{FALSE}.
#' @param color Character; the name of a Canva color palette available in
#'   \code{ggthemes::canva_palettes}. Default is \code{"Fresh and bright"}.
#'
#' @return
#' A \code{ggplot2} object representing the population pyramid trend plot.
#'
#' @seealso [area_trends()], [load_pop_data()], [pop_data_by_reg()], [get_code_label()]
#'
#' @examples
#' \dontrun{
#' # Example: pyramid trends for Indonesia
#' data_idn <- load_pop_data(harmonized = TRUE, smoothing = 1) |>
#'   pop_data_by_reg(0) #Indonesia
#' pyr_trends(data_idn, mode = 1)  # grid layout
#'
#' # Overlay mode with proportions
#' pyr_trends(data_idn, mode = 2, use_prop = TRUE)
#' }
#'
#' @export
pyr_trends <- function(data, mode = 1, use_prop = FALSE, color = "Fresh and bright"){

  # make year and sex as factor
  yr <- unique(data$year)
  n_yr <- length(yr)

  yr_lab <- ref_label$census_label |>
    filter(year %in% yr) |>
    pull(label)

  data <- data |>
    mutate(
      year = factor(year, labels = yr_lab),
      sex = factor(sex, levels = ref_label$sex_label$sex,
                   labels = ref_label$sex_label$label)
    )

  # value label and scale
  y_lab <- "Count (in thousands)"
  scl_fun <- function(y) abs(y)/1000

  # if use_prop
  if(use_prop){
    data <- data |>
      group_by(year, sex) |>
      mutate(pop = pop/sum(pop)) |>
      ungroup()
    y_lab <- "Proportion"
    scl_fun <- function(y) abs(y)
  }

  data <- data |>
    mutate(pop = ifelse(sex == "Male", -1*pop, pop))

  # make plot
  if(mode == 1){
    plt <- data |>
      ggplot(aes(x = age5, y = pop, group = interaction(sex, age5))) +
      geom_bar(aes(fill = sex), stat = "identity") +
      labs(x = "Age", y = y_lab, fill = "") +
      scale_y_continuous(labels = scl_fun, breaks = pretty_breaks()) +
      coord_flip() +
      scale_fill_canva(palette = color) + # canva color palette
      facet_wrap(~year)

  }else{
    plt <- data |>
      ggplot(aes(x = age5, y = pop, group = interaction(year, sex))) +
      geom_line(aes(color = year), linewidth = .7) +
      geom_hline(yintercept = 0, color = "black", linewidth = .5) +
      labs(x = "Age", y = y_lab, color = "") +
      scale_y_continuous(labels = scl_fun, breaks = pretty_breaks()) +
      coord_flip() +
      guides(color = guide_legend(nrow = 1))
      # scale_color_canva(palette = color) + # canva color palette
      # scale_color_tableau() +

    if(n_yr > 4){
      base_col <- ggthemes::canva_palettes[[color]]
      col <- grDevices::colorRampPalette(base_col)(n_yr)
      plt <- plt +
        scale_color_manual(values = col)
    }else{
      plt <- plt +
        scale_color_canva(palette = color)
    }

  }

  plt <- plt +
    theme(
      legend.position = "bottom"
    )

  return(plt)

}

#' Plot Population Area Trends by Age Group and Sex
#'
#' This function builds an area plot showing the proportion of population
#' distributed across three broad age groups (young, working-age, old)
#' over census years. The plot can be displayed separately by sex or combined.
#'
#' @param data A data frame containing population trends data for a specific
#'   region over years. Must include variables \code{year}, \code{sex},
#'   \code{age5}, and \code{pop}.
#' @param sex Integer indicating which sex to include in the plot:
#'   \itemize{
#'     \item 1 = All sexes
#'     \item 2 = Male
#'     \item 3 = Female
#'     \item 4 = Male+Female
#'   }
#'   Default is 1 (all sexes).
#' @param color Character string specifying the palette name from
#'   \code{ggthemes::canva_palettes}. Default is \code{"Fresh and bright"}.
#'
#' @details
#' The function aggregates population into three age groups:
#' \itemize{
#'   \item 0–14 years (Young)
#'   \item 15–64 years (Working age)
#'   \item 65+ years (Old)
#' }
#' It then calculates the proportion of each age group within each sex and year.
#' The result is plotted as a stacked area chart, optionally faceted by sex.
#'
#' @return A \code{ggplot2} object showing the population area trends.
#'
#' @seealso [pyr_trends()], [load_pop_data()], [pop_data_by_reg()], [get_code_label()]
#'
#' @examples
#' \dontrun{
#' # Example: area trends for Indonesia
#' data_idn <- load_pop_data(harmonized = TRUE, smoothing = 1) |>
#'   pop_data_by_reg(0) #Indonesia
#' area_trends(data_idn, sex = 1) #All sexes
#' area_trends(data_idn, sex = 2) #Male
#' area_trends(data_idn, sex = 3) #Female
#' area_trends(data_idn, sex = 4) #Male+Female
#' }
#'
#' @export
area_trends <- function(data, sex = 1, color = "Fresh and bright"){

  # identifying years
  yr <- unique(data$year)
  yr_lab <- ref_label$census_label |>
    filter(year %in% yr) |>
    pull(label)

  # aggregate 3 population groups
  data_by_sex <- data |>
    mutate(
      age_grp3 = case_when(
        age5 %in% c(0,10) ~ 1,
        age5 %in% c(15,60) ~ 2,
        TRUE ~ 3
        ),
      age_grp3 = factor(age_grp3, labels = censuspyrID_data_env$ref_age_grp3)
      ) |>
    group_by(year, sex, age_grp3) |>
    summarise(pop = sum(pop), .groups = NULL)

  data_total <- data_by_sex |>
    group_by(year, age_grp3) |>
    summarise(pop = sum(pop), .groups = NULL) |>
    mutate(sex = 3) |>
    relocate(sex, .after = "year")

  data <- rbind(data_by_sex, data_total) |>
    mutate(sex = factor(sex, levels = ref_label$sex_label$sex,
                         labels = ref_label$sex_label$label)) |>
    group_by(year, sex) |>
    mutate(pop = pop/sum(pop)) |>
    ungroup()

  # filter dataset based on view option
  if(sex != 1){
    filt_sex <- switch(sex-1, "Male", "Female", "Male+Female")
    data <- data |> filter(sex == filt_sex)
  }

  # make a plot
  plt <- data |>
    ggplot(aes(year, pop)) +
    geom_area(aes(fill = age_grp3)) +
    labs(x = "Census year", y = "Proportion", fill = "") +
    scale_fill_canva(palette = color) +
    scale_x_continuous(breaks = yr, labels = yr_lab) +
    facet_grid(sex ~ .) +
    theme(
      legend.position = "bottom"
    )

  return(plt)

}


# Data ----

#' Prepare Population Data for Tabular Display
#'
#' @description
#' Prepares population data for tabular display (e.g., in reports or Shiny apps).
#' The function reshapes the data by sex, adds total population, and computes the
#' sex ratio, while also attaching province names and labels.
#'
#' @param data A data frame containing population data for a specific province
#'   and year. Must include columns: `year`, `province_id`, `sex`, `age5`, and `pop`.
#' @param reg_code Integer or character. Province code used to retrieve
#'   the province name.
#' @param harmonized Logical. If `TRUE` (default), province codes are treated as
#'   harmonized; if `FALSE`, non-harmonized codes are used.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Adds the province name using [reg_name()].
#'   \item Relabels `sex` and `age5` using reference tables in `ref_label`.
#'   \item Reshapes data into wide format with separate columns for `Male` and `Female`.
#'   \item Adds a `Male+Female` total population column.
#'   \item Computes the sex ratio (`Male/Female * 100`).
#' }
#'
#' @return A data frame in wide format with columns:
#' \itemize{
#'   \item `province_id` — province identifier
#'   \item `province` — province name
#'   \item `year` — census year
#'   \item `age5` — five-year age group label
#'   \item `Male` — male population
#'   \item `Female` — female population
#'   \item `Male+Female` — total population
#'   \item `sex_ratio` — ratio of males to females (per 100 females)
#' }
#'
#' @seealso [load_pop_data()], [pop_data_by_year()], [get_code_label()]
#'
#' @examples
#' \dontrun{
#' data_idn <- pop_data_by_year(load_pop_data(), 2020) |>
#'   pop_data_by_reg(0) #Indonesia
#' tab <- data_for_table(data_idn, reg_code = 0, harmonized = TRUE)
#' head(tab)
#' }
#'
#' @export
data_for_table <- function(data, reg_code, harmonized = TRUE){
  prov_name <- reg_name(reg_code, harmonized)
  df <- data |>
    mutate(province = prov_name,
                  sex = factor(sex, levels = ref_label$sex_label$sex,
                               labels = ref_label$sex_label$label),
                  age5 = factor(age5, levels = ref_label$age5_label$age5,
                                labels = ref_label$age5_label$label)) |>
    relocate(province, .after = "province_id") |>
    pivot_wider(id_cols=year:age5,names_from = "sex",
                values_from = "pop") |>
    mutate(`Male+Female` = Male + Female,
                  `sex_ratio` = Male/Female*100)

  return(df)
}

#' Population Counts in 5-Year Age Groups from Indonesian Censuses
#'
#' @description
#' Population counts in 5-year age groups at the provincial level (subnational level 1),
#' derived from a series of Indonesian population censuses. Data are available in two
#' versions:
#' \itemize{
#'   \item \code{hpop5} — Harmonized province codes across census years.
#'   \item \code{ypop5} — Original (non-harmonized) province codes as reported in each census.
#' }
#'
#' Both datasets are processed from census samples provided by IPUMS International
#' (1971–2010) and the Population Census 2020. Data processing steps include prorating
#' to allocate missing attributes and smoothing using multiple demographic methods
#' (Arriaga and Karup–King–Newton).
#'
#' @format
#' Each dataset is a tibble (data frame) with the following variables:
#' \describe{
#'   \item{\code{year}}{Census year.}
#'   \item{\code{province_id_h}}{Harmonized province identifier (in \code{hpop5}).}
#'   \item{\code{province_id_y}}{non-harmonized province identifier (in \code{ypop5}).}
#'   \item{\code{sex}}{Sex code.}
#'   \item{\code{age5}}{Age group in 5-year intervals.}
#'   \item{\code{ns}}{Unsmoothed population count.}
#'   \item{\code{arriaga}}{Population count smoothed with the Arriaga method.}
#'   \item{\code{kkn}}{Population count smoothed with the Karup–King–Newton method.}
#' }
#'
#' - \code{hpop5}: 5,500 observations.
#' - \code{ypop5}: 6,146 observations.
#'
#' @source
#' Steven Ruggles, Lara Cleveland, Rodrigo Lovaton, Sula Sarkar, Matthew Sobek,
#' Derek Burk, Dan Ehrlich, Quinn Heimann, Jane Lee, and Nate Merrill.
#' *Integrated Public Use Microdata Series, International: Version 7.6* (dataset).
#' Minneapolis, MN: IPUMS, 2025. \doi{10.18128/D020.V7.6}
#'
#' Badan Pusat Statistik (BPS). *Jumlah Penduduk Menurut Wilayah, Kelompok Umur,
#' dan Jenis Kelamin, di INDONESIA – Sensus Penduduk 2020*. Retrieved September 4, 2025,
#' from \url{http://sensus.bps.go.id/topik/tabular/sp2020/3}
#'
#' @references
#' Steven Ruggles, Lara Cleveland, Rodrigo Lovaton, Sula Sarkar, Matthew Sobek,
#' Derek Burk, Dan Ehrlich, Quinn Heimann, Jane Lee, and Nate Merrill.
#' *Integrated Public Use Microdata Series, International: Version 7.6* (dataset).
#' Minneapolis, MN: IPUMS, 2025. \doi{10.18128/D020.V7.6}
#'
#' #' Badan Pusat Statistik (BPS). *Jumlah Penduduk Menurut Wilayah, Kelompok Umur,
#' dan Jenis Kelamin, di INDONESIA – Sensus Penduduk 2020*. Retrieved September 4, 2025,
#' from \url{http://sensus.bps.go.id/topik/tabular/sp2020/3}
#'
#' @examples
#' library(dplyr)
#'
#' # Harmonized data
#' data(hpop5)
#' glimpse(hpop5)
#' head(hpop5)
#'
#' # non-harmonized data
#' data(ypop5)
#' glimpse(ypop5)
#' head(ypop5)
#'
#' @name pop5
#' @aliases hpop5 ypop5
#' @docType data
#' @keywords datasets
NULL


#' Retrieve Reference Codes and Labels
#'
#' This function returns reference tables for codes and labels used in the
#' package. It can provide mappings for census years, sex, age groups, and
#' province codes (harmonized or non-harmonized).
#'
#' @param what Integer indicating which reference table to return:
#'   \itemize{
#'     \item 1 = Census year and label
#'     \item 2 = Sex code and label
#'     \item 3 = Age (5-year group) code and label
#'     \item 4 = Harmonized province code and label
#'     \item 5 = Non-harmonized province code and label
#'   }
#'
#' @details
#' The function retrieves data from internal reference object
#' \code{re_label}, which stores standardized coding schemes and
#' their associated labels.
#'
#' @return A data frame (or tibble) containing codes and labels for the
#'   selected reference category.
#'
#' @examples
#' # Get harmonized province codes and labels
#' get_code_label(4)
#'
#' # Get sex codes and labels
#' get_code_label(2)
#'
#' @export
get_code_label <- function(what = 4){
  ref <- ref_label[[what]]
  return(ref)
}

# Help ----
