# County Health Dashboard

## Overview
This project is an interactive R Shiny dashboard built using the CDC PLACES county-level dataset. The dashboard explores age-adjusted public health measures across U.S. counties, with a focus on obesity, diagnosed diabetes, and their relationship.

## Project Goals
The purpose of this dashboard is to:
- visualize county-level public health burden
- compare counties across states
- explore the relationship between obesity and diagnosed diabetes
- present results in a clean and user-friendly dashboard format

## Dataset
**Source:** CDC PLACES county-level dataset, 2023

The dashboard currently uses:
- Age-adjusted prevalence estimates
- County-level data
- Measures related to obesity and diagnosed diabetes

## Dashboard Features
- **Year filter** (currently fixed to 2023)
- **State filter** for viewing all states or one selected state
- **Obesity tab** with top 10 county chart and ranking table
- **Diabetes tab** with top 10 county chart and ranking table
- **Relationship tab** showing the association between obesity and diagnosed diabetes
- **About tab** explaining the dashboard and how to use it

## Key Measures
- Obesity among adults
- Diagnosed diabetes among adults

## Tools Used
- **R**
- **Shiny**
- **tidyverse**
- **ggplot2**
- **bslib**

## Project Files
- `app.R` — main Shiny dashboard file
- `app_working_v1.R` — backup working version
- `analysis_charts.R` — earlier chart development code
- `data/places_county_2025.csv` — dataset used in the project

## Example Insights
- Several counties in Alabama, Mississippi, and Louisiana show very high age-adjusted obesity prevalence.
- Counties with higher obesity prevalence tend to also have higher diagnosed diabetes prevalence.
- The county-level correlation between age-adjusted obesity and diagnosed diabetes was positive in the 2023 dataset.

## How to Run the Dashboard
1. Open the project in RStudio
2. Make sure required packages are installed
3. Run:

```r
shiny::runApp()