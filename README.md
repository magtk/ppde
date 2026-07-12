# Palmer Penguins Dashboard Explorer

**PPDE (Palmer Penguins Dashboard Explorer)** is an interactive R Shiny dashboard for exploring the Palmer Penguins dataset through a CRISP-DM-inspired workflow.

The repository combines a compact teaching example with a more complete exploratory dashboard. It can be used during classes to demonstrate how a Shiny application grows from a minimal prototype into a richer analytical tool.

## Repository contents

```text
.
├── README.Rmd
├── pp-quick-start.Rmd
└── ppde.R
```

### `pp-quick-start.Rmd`

A step-by-step introduction to R Shiny designed for classroom use.

The document demonstrates how to build a simple Shiny application incrementally, including:

- application structure,
- user interface and server logic,
- reactive inputs and outputs,
- basic visualisations,
- filtering and interaction.

### `ppde.R`

The complete PPDE dashboard.

The application includes:

- dataset overview and description,
- summary statistics and data preview,
- histograms, density plots and violin plots,
- correlogram,
- missing-data assessment,
- potential outlier detection,
- interactive 2D and 3D scatterplots,
- colour and marker-shape mappings,
- trend lines for 2D plots,
- island map with species filtering,
- decision tree classification,
- confusion matrix,
- accuracy, balanced accuracy and per-class evaluation,
- light and dark modes,
- high-contrast mode,
- a colour-blind-safe palette.

## CRISP-DM-inspired workflow

The dashboard loosely follows selected stages of the CRISP-DM process.

### 1. Data understanding

- dataset description,
- descriptive statistics,
- data preview,
- distribution plots,
- correlation analysis.

### 2. Data quality assessment

- missing-value analysis,
- potential outlier detection.

### 3. Data exploration

- interactive 2D and 3D visualisation,
- colour and marker-shape mappings,
- species and island filtering,
- trend analysis.

### 4. Geographical context

- island-level summaries,
- species distribution across islands,
- interactive map popups.

### 5. Modelling and evaluation

- decision tree classifier,
- test-set predictions,
- accuracy,
- balanced accuracy,
- confusion matrix,
- per-class precision, recall and F1 score.

The application is intended as an educational walkthrough rather than a complete production implementation of CRISP-DM.

## Requirements

Install the required R packages:

```{r install-packages, eval=FALSE}
install.packages(c(
  "shiny",
  "shinythemes",
  "palmerpenguins",
  "ggplot2",
  "dplyr",
  "tidyr",
  "plotly",
  "leaflet",
  "naniar",
  "scales",
  "rpart",
  "rpart.plot"
))
```

## Run the dashboard

Clone or download the repository, set the repository directory as the working directory and run:

```{r run-dashboard, eval=FALSE}
shiny::runApp("ppde.R")
```

Alternatively, open `ppde.R` in RStudio and select **Run App**.

## Dataset

The application uses the `penguins` dataset from the [`palmerpenguins`](https://allisonhorst.github.io/palmerpenguins/) R package.

The dataset contains observations of three penguin species from the Palmer Archipelago, Antarctica:

- Adelie,
- Chinstrap,
- Gentoo.

The observations come from three islands:

- Biscoe,
- Dream,
- Torgersen.

The available variables include:

- species,
- island,
- bill length,
- bill depth,
- flipper length,
- body mass,
- sex,
- year.

## Educational use

The repository can be used to:

- introduce the structure of Shiny applications,
- demonstrate reactive programming,
- explain exploratory data analysis,
- discuss visual encoding with colour and marker shape,
- compare 2D and 3D visualisations,
- introduce data-quality assessment,
- demonstrate a basic machine-learning workflow,
- discuss accessibility in interactive data applications,
- show how an application can follow a CRISP-DM-inspired analytical process.

## Project status

This repository contains material prepared for teaching and demonstration purposes.

The dashboard may be extended in the future with additional data-cleaning operations, modelling methods, deployment options and reusable modules.

### Live demo

A live deployment is available on shinyapps.io:

[Open PPDE](https://magt.shinyapps.io/ppde/)

If the service is temporarily unavailable, run the application locally:

```
shiny::runApp("app.R")
```
## Licence

This project is shared under the:

**Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International  
CC BY-NC-ND 4.0**

The material may be shared with attribution for non-commercial purposes. Modified versions may not be distributed under this licence.

## Project Anatomy
```
Head  (concept & design):           MagT        ,_,
Eyes  (exploration & insight):      ambr0wl    (O,O)
Hands (development):                MagTk      (   )
Feet  (reality check & grounding):  anovi      -"-"-
>      Curious?     --------->      https://magt.ovh
```
