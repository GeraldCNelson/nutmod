The nutrient modeling R project
================
Gerald C. Nelson
December 14, 2018

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2280474.svg)](https://doi.org/10.5281/zenodo.2280474)

The goal of this project is to provide estimates of the nutritional
consequences of changes arising in scenarios that combine socioeconomic
and climate futures to 2050. It integrates data from

  - FAO’s Food Balance Sheets,
  - the Shared Socioeconomic Profiles used in the IPCC’s Fifth
    Assessment Report and many other studies,
  - quantitative scenario modeling results from IFPRI’s IMPACT model,
  - a lookup table that is used to convert food consumption by a
    representative consumer in a country (including optionally a measure
    of nutrient loss during food preparation) to it macro and micro
    constituents, and
  - age- and gender-adjusted nutrient requirements from the Institute of
    Medicine of the US National Academy of Sciences
    [1](https://ods.od.nih.gov/Health_Information/Dietary_Reference_Intakes.aspx)
    using a U.S. diet to correct for dietary effects on bioavailability.

The results from this project are published in the following paper

> Paper reference: Nelson, Gerald, Jessica Bogard, Keith Lividini,
> Joanne Arsenault, Malcolm Riley, Timothy B. Sulser, Daniel
> Mason-D’Croz, Brendan Power, David Gustafson, Mario Herrero, Keith
> Wiebe, Karen Cooper, Roseline Remans, Mark Rosegrant, 2018. “Income
> Growth and Climate Change Effects on Global Nutrition Security to
> Mid-Century.” Nature Sustainability 1 (12).
> <doi:10.1038/s41893-018-0192-z>.

The `automate.R` script executes each script in the order needed to
generate output used in the Nature Sustainability paper. Each script can
also be run standalone. It assumes that file generated in scripts
earlier in the automate script are available.

# Directory structure

The directory hierarchy of the project is as follows. Second level
directories are in parentheses:

  - data (IMPACTData) — contains .xlsx and .rds files generated by the
    dataPrep and dataManagement R scripts
  - data-raw (FBSData, IMPACTData, NutrientData, SSPData) — data files
    from FAO, SSP, IMPACT, the nutrient lookup table
  - graphics — graphic outputs from the analysis
  - R — R scripts
  - Results — results from the analysis, contains .xlsx and .rds files

# R code file naming conventions

All R code is in the R directory. File naming conventions are:

  - dataPrep.xxx.R — reads in the raw data from the data—raw directory
    and processes it into .rds (and sometimes .xlsx) files and writes
    these to the data or IMPACTData directory
  - dataManagement.xxx.R — reads in .rds data files from the data or
    IMPACTData directory, does more processing and writes writes these
    to the data or IMPACTData directory.
  - xxxFunctions.R — has generic functions used in the xxx R scripts. A
    special functions script is nutrientModFunctions.R. In addition to R
    functions it holds the names of key variables such as file names and
    directory paths. All other scripts read this information in with
    functions from this script.
  - nutrientCalcs.R — all final calculations are done in this script.

# Results files

All file names have a standard format — substantive name, creation date,
suffix. An example is `metaData.2018-04-17.xlsx`.

  - suffix — `.rds` (a compressed file format used in R) in all cases,
    `.xlsx` and `.csv` as options

## substantive name examples

  - General — `metaData, nut.requirements, food.group, staples`
  - requirements — `req.EARxxx, req.RDA.vitsxxx, req.RDA.minrls,
    req.RDA.macroxxx, req.UL.vitsxxx,req.UL.minrlsxxx`
  - type of results — ratio of nutrient consumption by
    commodity/staple/food group to total consumption, ratio of nutrient
    consumption by commodity/staple/food group to requirement

# Variable naming conventions

  - scenario — name of the IMPACT scenario, currently SSP2-GFDL,
    SSP2-MIROC, SSP2-NoCC
  - region — the aggregation from individual countries to larger
    regions. Currently region\_code.IMPACT3
  - year — 4 digit year with an X to start with so it is a character
    value. Example - X2020
  - IMPACT\_code — a 4 or 5 digit code for each of the IMPACT
    commodities. Example - cwhea - the quantity of wheat consumed by a
    representative consumer
  - food.group.code — the code for a food group. Example - cereals.
  - staple.code — the code for a food group. Example - staple
  - foodAvailpDay — the per capita quantity of an IMPACT commodity
    available for consumption by a representative consumer. Units -
    kg/day
  - nutrient.code — a nutrient code combines the common name of the
    nutrient and the units it is in. Example - folate\_µg (folate in micrograms)  
  - nutrient.Q — the quantity of a nutrient available from an IMPACT
    commodity per day
  - nutrient.sum \[.all, .staple, .foodGroup\] — The sum of nutrient.Q
    for all commodities, for each staple group, for each food group.
  - nutrient.ratio \[.all, .staple, .foodGroup\] — The ratio of a
    nutrient to the total consumed
  - nutrient.req.ratio \[.all, .staple, .foodGroup\] — The ratio of a
    nutrient to the daily requirement for a representative consumer (i., population 
    weighted by age and gender requirements)
