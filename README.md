# occupationRiskPack

<!-- badges: start -->
[![R-CMD-check](https://github.com/jwklee/occupationRiskPack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jwklee/occupationRiskPack/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/occupationRiskPack)](https://CRAN.R-project.org/package=occupationRiskPack)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A unified R package for accessing and analyzing occupation-level risk variables commonly used in labor economics, political economy, and sociological research. The package consolidates data from multiple authoritative sources into a consistent framework built around standardized occupation classification systems.

## Key Features

- **Standardized occupation codes**: All data organized by ISCO or SOC codes
- **Multiple risk dimensions**: Automation risk, offshorability, skill specificity, AI exposure, environmental transition exposure, and more
- **Cross-classification support**: Crosswalks between ISCO-88, ISCO-08, SOC 2010, and SOC 2018
- **Analysis utilities**: Functions for ranking occupations, summarizing by groups, and imputing missing values
- **Proper citations**: Each data loader displays source information to facilitate attribution

## Installation

### From CRAN (when available)

```r
install.packages("occupationRiskPack")
```

### Development version from GitHub

```r
# install.packages("devtools")
devtools::install_github("jwklee/occupationRiskPack")
```

## Quick Start

```r
library(occupationRiskPack)
library(dplyr)

# Load ISCO-08 baseline table (427 4-digit codes)
isco <- base_isco()

# Load routine task intensity data
rti <- load_rti()

# Merge and analyze
data <- isco %>%
  left_join(rti, by = "isco08_code")

# Find top 10 occupations by automation risk
get_top_bottom_occupations(data, var = "rti_mihaylov_2019", n = 10)
```

## Available Data Loaders

### Classification Systems

| Function | Description | Classification |
|----------|-------------|----------------|
| `base_isco()` | ISCO-08 occupations (427 codes) | ISCO-08 |
| `base_isco_detail()` | ISCO-08 with ILO definitions | ISCO-08 |
| `base_soc()` | SOC 2018 occupations (867 codes) | SOC 2018 |

### Risk Variables

| Function | Description | Source |
|----------|-------------|--------|
| `load_rti()` | Routine Task Intensity | Mihaylov et al. (2019) |
| `load_aioe()` | AI Occupational Exposure | Felten et al. (2021) |
| `load_computerization()` | Computerization probability | Frey & Osborne (2017) |
| `load_offshorability()` | Offshorability index | Blinder (2007) |
| `load_mahutga()` | RTI & Offshorability by country | Mahutga et al. (2018) |
| `load_skill_specificity()` | Skill specificity | Pardos-Prado & Xena (2019) |
| `load_brownness()` | Greenness/Brownness exposure | Cavallotti et al. (2025) |
| `load_scholl_green_brown()` | Green/Brown/Grey occupations | Scholl et al. (2023) |
| `load_prestige()` | Occupational prestige | Hughes et al. (2024) |
| `load_hazard()` | Hazardous working conditions | O*NET |
| `load_employment_projections()` | Employment projections | BLS |

### Crosswalks

| Function | Description |
|----------|-------------|
| `crosswalk_isco_88_08()` | ISCO-88 to ISCO-08 |
| `crosswalk_soc_10_18()` | SOC 2010 to SOC 2018 |
| `crosswalk_isco_08_soc_10()` | ISCO-08 to SOC 2010 |

### Analysis Functions

| Function | Description |
|----------|-------------|
| `get_top_bottom_occupations()` | Rank occupations by variable |
| `summarize_risks()` | Summary stats by ISCO prefix |
| `lookup_isco()` | Look up occupation information |
| `impute_by_isco()` | Impute missing values by ISCO hierarchy |
| `impute_by_soc()` | Impute missing values by SOC hierarchy |

## Example Workflows

### Analyzing Automation Risk

```r
library(occupationRiskPack)
library(dplyr)

# Load and merge data
data <- base_isco() %>%
  left_join(load_rti(), by = "isco08_code") %>%
  mutate(major_group = substr(isco08_code, 1, 1))

# Summarize by major group
data %>%
  group_by(major_group) %>%
  summarize(
    mean_rti = mean(rti_mihaylov_2019, na.rm = TRUE),
    n = n()
  )
```

### Working with SOC Data

```r
# Load SOC-based computerization data
comp <- load_computerization()

# Crosswalk to SOC 2018
soc_xwalk <- crosswalk_soc_10_18()
comp_2018 <- comp %>%
  left_join(soc_xwalk, by = c("soc2010_code" = "soc2010_code"))
```

### Imputing Missing Values

```r
# Create sample data with brownness values
data <- base_isco() %>%
  left_join(load_brownness(), by = "isco08_code")

# Impute missing brownness values using ISCO hierarchy
imputed <- impute_by_isco(
  data,
  var = "brownness",
  new_var = "brownness_i",
  isco_key = "isco08_code"
)
```

## Documentation

For comprehensive documentation with detailed examples, see the package vignette:

```r
vignette("occupationRiskPack")
```

## Data Sources

All data sources are documented in `inst/COPYRIGHTS`. The package includes data from:

- International Labour Organization (ILO)
- U.S. Bureau of Labor Statistics (BLS)
- O*NET OnLine
- OECD Working Papers
- Academic publications (properly cited)

## Citation

If you use this package in research, please cite:

```
Lee, J. (2025). occupationRiskPack: Occupation-Level Risk Data for Labor Market Research. 
R package version 0.1.0. https://github.com/jwklee/occupationRiskPack
```

## License

MIT License. See LICENSE file for details.
