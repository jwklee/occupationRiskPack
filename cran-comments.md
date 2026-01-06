# CRAN Submission Comments

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* Local: macOS (aarch64-apple-darwin20), R 4.2.3
* GitHub Actions (planned):
  - macOS-latest (release)
  - windows-latest (release)
  - ubuntu-latest (devel, release, oldrel-1)

## Package Description

This package provides standardized occupation-level risk measures for labor 
market research, including:

- Automation risk (Routine Task Intensity)
- Offshorability
- AI exposure
- Computerization probability
- Environmental transition exposure (greenness/brownness)
- Occupational prestige
- Hazardous working conditions

Data supports both ISCO-08 and SOC classification systems with crosswalks.

## Data Sources

All data sources are properly attributed in `inst/COPYRIGHTS`. Sources include:

- Academic publications (with proper citations)
- Government statistical agencies (BLS, O*NET, OECD)
- All sources permit academic/non-commercial redistribution

## Downstream dependencies

None (new package).
