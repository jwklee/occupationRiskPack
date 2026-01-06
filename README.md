# occupationRiskPack

<!-- badges: start -->
[![R-CMD-check](https://github.com/jwklee/occupationRiskPack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jwklee/occupationRiskPack/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/occupationRiskPack)](https://CRAN.R-project.org/package=occupationRiskPack)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An R package for analyzing occupational risk variables at the ISCO-08 4-digit level. Combine and analyze automation risk (RTI), environmental exposure (greenness/brownness), and other occupation-level variables with ease.

## Features

✨ **Simple API**: Load, merge, and analyze occupation-level risk variables with just a few functions

📊 **Complete Data**: Pre-packaged occupation reference and risk variables keyed to ISCO-08 4-digit codes

🔍 **Flexible Analysis**: Rank occupations, lookup codes ↔ titles, compute aggregated statistics, and impute missing values

🎯 **ISCO-08 Based**: Everything is organized around the standardized ISCO-08 4-digit classification for occupations

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

# Load all risk variables merged together
merged <- merge_occup_risks()

# Find top occupations by automation risk
top_rti <- get_top_bottom_occupations(
  merged, 
  var = "rti_mihaylov_2019", 
  n = 15
)

# 3. Add occupation titles for reference
isco <- base_isco()
titles <- lookup_isco(
  isco,
  lookup_values = top_rti$isco08_code,
  lookup_col = "isco08_code",
  return_col = "isco08_title"
)

# Combine and display
result <- merge(top_rti, titles, by = "isco08_code")
print(result)
```

## Main Functions

### Data Loading

- **`base_isco()`**: Load ISCO-08 baseline reference table with codes and titles
- **`load_rti()`**: Load routine task intensity (automation risk) data
- **`load_brownness()`**: Load greenness/brownness (environmental exposure) data
- **`merge_occup_risks()`**: Merge all risk variables into a single dataset

### Analysis

- **`get_top_bottom_occupations()`**: Rank occupations by risk variable, optionally grouped by ISCO prefix level
- **`lookup_isco()`**: Quick lookup between ISCO codes and occupation titles (bidirectional, case-insensitive)
- **`summarize_risks()`**: Compute summary statistics (mean, SD, missing %) by ISCO aggregation level
- **`impute_by_isco()`**: Fill missing values using hierarchical ISCO group means

## Documentation

### Get Started

📖 **[USAGE_GUIDE.md](USAGE_GUIDE.md)** - Comprehensive guide with examples for all functions

📚 **[Getting Started Vignette](vignettes/getting_started.Rmd)** - R Markdown vignette showing complete workflows

### Reference

🔧 **[PACKAGE_DEVELOPMENT.md](PACKAGE_DEVELOPMENT.md)** - Technical documentation for developers

📋 **[README.dev.md](README.dev.md)** - Developer setup and testing instructions

### Examples in R

```r
# Function-specific help
?load_isco
?get_top_bottom_occupations
?lookup_isco
?summarize_risks
?impute_by_isco

# Or browse the USAGE_GUIDE
# vignette("getting_started", package = "occupationRiskPack")
```

## Common Use Cases

### Find High-Risk Occupations

```r
merged <- merge_occup_risks()

# Top 20 by automation risk
top_rti <- get_top_bottom_occupations(merged, var = "rti_mihaylov_2019", n = 20)
```

### Compare Risk Across ISCO Groups

```r
# Top/bottom 10 occupations by major ISCO group (1-digit)
by_major <- get_top_bottom_occupations(
  merged, 
  var = "greenness", 
  n = 10, 
  by_group = 1
)
```

### Batch Code ↔ Title Lookups

```r
isco <- base_isco()

# Look up titles for codes
lookup_isco(
  isco,
  lookup_values = c("1111", "2111", "3323"),
  lookup_col = "isco08_code",
  return_col = "isco08_title"
)

# Or codes for titles (case-insensitive)
lookup_isco(
  isco,
  lookup_values = c("engineers", "legislators"),
  lookup_col = "isco08_title",
  return_col = "isco08_code"
)
```

### Combine with Your Own Data

```r
my_data <- data.frame(
  isco08_code = c("1111", "2111"),
  employees = c(100, 50),
  wage = c(80000, 95000)
)

merged <- merge_occup_risks()
result <- merge(my_data, merged, by = "isco08_code")
```

### Aggregated Statistics by ISCO Level

```r
# Summary by 2-digit ISCO groups
summary_2dig <- summarize_risks(
  merged,
  vars = c("rti_mihaylov_2019", "greenness"),
  by_level = 2
)
```

## Data Overview

The package includes:

- **427 ISCO-08 4-digit occupations** with standardized titles
- **RTI (Routine Task Intensity)** from Mihaylov (2019) - measures automation risk
- **Greenness & Brownness** - measures environmental exposure for post-carbon transition

All data is keyed by `isco08_code` (4-digit ISCO code as character) for easy combining and merging.

## Package Structure

```
occupationRiskPack/
├── R/
│   ├── loaders.R              # Data loading functions
│   ├── merge_core_api.R       # Merge function
│   ├── summarize.R            # Aggregation statistics
│   ├── impute.R               # Imputation functions
│   └── utils_analysis.R       # Analysis helpers
├── inst/extdata/              # Data files (source_*.csv)
├── tests/testthat/            # Unit tests
├── vignettes/
│   └── getting_started.Rmd    # Getting started guide
├── USAGE_GUIDE.md             # Detailed usage guide
├── PACKAGE_DEVELOPMENT.md     # Technical design docs
└── README.md                  # This file
```

## Development

### Running Tests

```r
# Run all tests
devtools::test()

# Run tests for specific function
devtools::test(filter = "lookup_isco")
```

### Regenerate Documentation

```r
# Update man pages and NAMESPACE from roxygen comments
devtools::document()
```

### Check Package

```r
# Run R CMD check
devtools::check()
```

For more development info, see [README.dev.md](README.dev.md).

## Contributing

Bugs? Ideas? Questions?

- Check [PACKAGE_DEVELOPMENT.md](PACKAGE_DEVELOPMENT.md) for technical details
- Review [USAGE_GUIDE.md](USAGE_GUIDE.md) for examples
- Look at test files in `tests/testthat/` for additional usage patterns

## License

TBD

## Citation

If you use this package in research, please cite:

```
TBD
```

## Acknowledgments

- ISCO-08 occupation data
- RTI measure from Mihaylov (2019)
- Greenness/brownness data from [source TBD]

---

**Get started**: Read [USAGE_GUIDE.md](USAGE_GUIDE.md) or run `vignette("getting_started")` to see examples!
