# Great Britain Adult Smoking Habits Visualisation
This Shiny app is designed to explore and visualise annual data on the proportion of adults in Great Britain who smoke cigarettes. 

## Data Source
The dataset used in this app was downloaded from the [Office for National Statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/drugusealcoholandsmoking/datasets/adultsmokinghabitsingreatbritain).

## Prerequisites 
- R 
- RStudio (optional but recommended for an enhanced R development environment) 
- Required R libraries: `readxl`, `stringr`, `tidyr`, `dplyr`, `shiny`, `ggplot2`

## Shiny App Structure
The Shiny app consists of two main tabs:
1. **By Gender:**
    - Allows users to explore the proportion of cigarette smokers over the years by gender for each age group
2. **By Age Group:**
    - Enables users to explore smoking habits by age group over the years for each gender 

## Usage

1. Run the Shiny app using the `shinyApp(ui, server)` command in RStudio.
2. Navigate between tabs to explore smoking habits by different criteria.
3. Use the provided input controls to customise the displayed data.
4. The line plots will dynamically update based on the inputs.

