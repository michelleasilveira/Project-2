# Project-2
# Superstore Shiny Explorer


An interactive Shiny app to explore the **Superstore** sales dataset (Kaggle). Users can subset the data by category and numeric ranges, view/download filtered data, and build a variety of multivariate plots and summaries.


- **Data source:** Kaggle — Superstore dataset (Orders file). See original dataset: https://www.kaggle.com/datasets/juhi1994/superstore/data
- **Tech:** R, Shiny, tidyverse, DT, bslib, shinycssloaders
- **Deployed at:** (add your shinyapps.io URL here after deployment)


## App Features
- Sidebar-driven subsetting with two categorical fields and two numeric range controls (generated dynamically)
- Action button to apply filters (reactive dataset updates only when pressed)
- Tabs:
- **About**: purpose, data link, and app overview
- **Data Download**: DT table + CSV download of the (possibly) filtered data
- **Data Exploration**: numeric stats, one-/two-way tables, and at least six multivariate plots with options for coloring, faceting, grouping


## How to run locally
```r
# install.packages(c("shiny","tidyverse","DT","bslib","shinycssloaders"))
# Optional: shinyWidgets, janitor, patchwork


shiny::runApp()