# occupationRiskPack 0.1.0

## Initial CRAN Release

* Core data loaders for occupation-level risk measures:
  - `load_rti()`: Routine Task Intensity scores (Mihaylov et al.)
  - `load_offshorability()`: Offshorability (Blinder)
  - `load_skill_specificity()`: Skill Specificity measures
  - `load_aioe()`: AI Occupational Exposure (Felten et al.)
  - `load_computerization()`: Computerization probability (Frey-Osborne)
  - `load_brownness()`: Greenness/Brownness exposure (Cavallo et al.)
  - `load_scholl_green_brown()`: Green/Brown/Grey occupations (Scholl et al. 2023)
  - `load_hazard()`: Hazardous working conditions (O-NET)
  - `load_mahutga()`: RTI and Offshorability (Mahutga et al.)
  - `load_prestige()`: Occupational Prestige (Hughes et al.)
  - `load_employment_projections()`: BLS Employment Projections

* Crosswalk functions for bridging classification systems:
  - `crosswalk_isco_88_08()`: ISCO-88 to ISCO-08
  - `crosswalk_soc_10_18()`: SOC 2010 to SOC 2018
  - `crosswalk_isco_08_soc_10()`: ISCO-08 to SOC 2010

* Base classification data:
  - `base_isco()`: ISCO-08 structure
  - `base_isco_detail()`: ISCO-08 with detailed labels
  - `base_soc()`: SOC 2018 structure

* Imputation functions for hierarchical aggregation:
  - `impute_by_isco()`: ISCO-based hierarchical imputation
  - `impute_by_soc()`: SOC-based hierarchical imputation

* Analysis utilities:
  - `get_top_bottom_occupations()`: Extract extreme risk occupations
  - `lookup_isco()`: Look up occupation names
  - `summarize_risks()`: Compute risk measure summaries
