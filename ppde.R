# Palmer Penguins Explorer -------------------------------------------------
# One-file Shiny application

required_packages <- c(
    "shiny", "palmerpenguins", "ggplot2", "dplyr", "tidyr",
    "plotly", "leaflet", "naniar", "scales", "rpart", "rpart.plot", "shinythemes"
)

missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
    stop(
        paste0(
            "Install the following packages before running the app: ",
            paste(missing_packages, collapse = ", "),
            "\n\nRun:\ninstall.packages(c(",
            paste(sprintf('"%s"', missing_packages), collapse = ", "),
            "))"
        ),
        call. = FALSE
    )
}

library(shiny)
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)
library(naniar)
library(scales)
library(rpart)
library(rpart.plot)
library(shinythemes)

# Application configuration -----------------------------------------------
# To adapt the application to another dataset, replace the data object and
# update the column roles, labels, palettes, map data, and model formula here.

APP_CONFIG <- list(
    data = palmerpenguins::penguins,
    title = "Palmer Penguins Explorer",
    subtitle = "Data understanding, quality assessment, exploration, geography, and modelling",
    description = paste(
        "The Palmer Penguins dataset contains measurements for three penguin",
        "species observed on three islands in the Palmer Archipelago, Antarctica.",
        "It is commonly used as an accessible alternative to the Iris dataset."
    ),
    roles = list(
        species = "species",
        island = "island",
        sex = "sex",
        numeric = c(
            bill_length_mm = "Bill length (mm)",
            bill_depth_mm = "Bill depth (mm)",
            flipper_length_mm = "Flipper length (mm)",
            body_mass_g = "Body mass (g)"
        )
    ),
    map = tibble::tribble(
        ~island,      ~lat,     ~lon,
        "Biscoe",    -65.433,  -65.500,
        "Dream",     -64.733,  -64.233,
        "Torgersen", -64.767,  -64.067
    ),
    model_formula = species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g
)

numeric_columns <- names(APP_CONFIG$roles$numeric)
numeric_choices <- setNames(numeric_columns, APP_CONFIG$roles$numeric)
variable_labels <- APP_CONFIG$roles$numeric

penguins_data <- APP_CONFIG$data %>%
    mutate(
        species = factor(.data[[APP_CONFIG$roles$species]]),
        island = factor(.data[[APP_CONFIG$roles$island]], levels = c("Biscoe", "Dream", "Torgersen")),
        sex_display = factor(
            if_else(is.na(.data[[APP_CONFIG$roles$sex]]), "Unknown", as.character(.data[[APP_CONFIG$roles$sex]])),
            levels = c("female", "male", "Unknown")
        )
    )

# Palettes ----------------------------------------------------------------
# No colour is reused between the island and species semantic mappings.

palettes <- list(
    default = list(
        name = "Tableau-inspired",
        island = c(Biscoe = "#2F6BFF", Dream = "#F28E2B", Torgersen = "#2CA02C"),
        species = c(Adelie = "#9467BD", Chinstrap = "#D62728", Gentoo = "#17BECF")
    ),
    colorblind = list(
        name = "Okabe–Ito",
        island = c(Biscoe = "#0072B2", Dream = "#E69F00", Torgersen = "#009E73"),
        species = c(Adelie = "#CC79A7", Chinstrap = "#D55E00", Gentoo = "#56B4E9")
    ),
    high_contrast = list(
        name = "High-contrast categorical",
        island = c(Biscoe = "#0047AB", Dream = "#FF8C00", Torgersen = "#006400"),
        species = c(Adelie = "#6A0DAD", Chinstrap = "#B00020", Gentoo = "#008B8B")
    )
)

variable_palette <- c(
    bill_length_mm = "#0072B2",
    bill_depth_mm = "#D55E00",
    flipper_length_mm = "#009E73",
    body_mass_g = "#CC79A7"
)

# Marker mappings shared by Plotly 2D and Plotly 3D.
# Species use filled symbols that render consistently in Plotly/WebGL:
# Adelie = circle  (base R pch 16),
# Chinstrap = diamond (base R pch 18),
# Gentoo = square (base R pch 15).
shape_maps <- list(
    island = c(Biscoe = "circle-open", Dream = "diamond-open", Torgersen = "square-open"),
    species = c(Adelie = "circle", Chinstrap = "diamond", Gentoo = "square")
)

# Header signature ---------------------------------------------------------

app_signature <- tags$div(
    class = "app-signature",
    tags$div(
        class = "signature-copy",
        tags$div(
            class = "signature-authors",
            "MagT • MagTk • ambr0wl • CC BY-NC-ND 4.0"
        ),
        tags$div(
            class = "signature-link",
            tags$span("> Curious?  ------>  "),
            tags$a(
                href = "https://magt.ovh",
                target = "_blank",
                rel = "noopener noreferrer",
                "https://magt.ovh"
            )
        )
    ),
    tags$pre(
        class = "signature-owl",
        " ,_,\n(0,0)\n(   )\n =\"=\"="
    )
)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
    theme = shinythemes::shinytheme("spacelab"),
    tags$head(
        tags$meta(charset = "utf-8"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        tags$link(
            rel = "preconnect",
            href = "https://fonts.gstatic.com",
            crossorigin = "anonymous"
        ),
        tags$link(
            rel = "stylesheet",
            href = paste0(
                "https://fonts.googleapis.com/css2?",
                "family=JetBrains+Mono:wght@400;500;600;700",
                "&display=swap"
            )
        ),
        tags$style(HTML("\
      body { transition: background-color .2s ease, color .2s ease; }\
      .app-header { display:flex; justify-content:space-between; align-items:flex-start; gap:1.5rem; flex-wrap:wrap; margin-bottom:.75rem; }\
      .app-title h2 { margin-top:0; margin-bottom:.2rem; }\
      .app-subtitle { margin:0; color:#5f6368; }\
      .app-signature { display:flex; align-items:flex-end; gap:1.25rem; width:max-content; margin-top:.9rem; padding-top:.55rem; border-top:1px solid #cfd6de; font-family:'JetBrains Mono','Cascadia Mono',Consolas,'Courier New',monospace; font-size:11.5px; line-height:1.35; color:#4f5b66; }\
      .signature-copy { min-width:390px; }\
      .signature-authors,.signature-link { white-space:nowrap; }\
      .signature-link { margin-top:.35rem; }\
      .signature-owl { margin:0; padding:0; border:0; background:transparent; font:inherit; line-height:1.05; color:inherit; white-space:pre; }\
      .app-signature a { color:inherit; font-weight:600; text-decoration:none; }\
      .app-signature a:hover, .app-signature a:focus { text-decoration:underline; }\
      .accessibility-panel { border:1px solid #d7dce2; border-radius:8px; padding:.65rem .9rem; min-width:280px; background:#f8f9fa; }\
      .accessibility-panel .form-group { margin-bottom:.35rem; }\
      .palette-message { margin-top:.55rem; padding:.6rem .75rem; border-radius:4px; background:#fff3cd; border:2px solid #856404; color:#533f03; font-weight:800; }\
      .metric-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(150px,1fr)); gap:.75rem; margin-bottom:1rem; }\
      .metric-card { border:1px solid #d7dce2; border-radius:8px; padding:.8rem 1rem; background:#fff; min-height:92px; }\
      .metric-label { font-size:.9rem; color:#5f6368; }\
      .metric-value { font-size:1.75rem; font-weight:700; line-height:1.2; }\
      .section-note { padding:.8rem 1rem; border-left:4px solid #2C7FB8; background:#f5f8fb; margin-bottom:1rem; }\
      .table-scroll { overflow-x:auto; }\
      .control-help { font-size:.9rem; color:#5f6368; margin-top:-.35rem; }\
      .leaflet { border:1px solid #d7dce2; border-radius:8px; }\
      .quality-top { margin-bottom:.5rem; }\
      .variable-checklist { border:1px solid #cfd6de; border-radius:6px; padding:.65rem .8rem; margin-bottom:.8rem; background:#fff; }\
      .variable-checklist .form-group { margin-bottom:.25rem; }\
      .scatter-mode-box { border:2px solid #0072B2; border-radius:7px; padding:.6rem .75rem; margin-bottom:.9rem; background:#eef6fb; }\
      .scatter-mode-status { font-weight:700; margin-top:-.25rem; }\
      .trend-controls-box { border:2px solid #555; border-radius:7px; padding:.65rem .75rem; margin:.75rem 0; background:#f7f7f7; }\
      .trend-controls-box .form-group { margin-bottom:.35rem; }\
      body.dark-mode .app-signature { color:#e4e8eb; border-top-color:#68737d; }\
      body.high-contrast .app-signature { border-top:3px solid currentColor; font-weight:700; }\
      @media (max-width:700px) { .app-signature { max-width:calc(100vw - 30px); overflow-x:auto; font-size:10.5px; gap:.8rem; } .signature-copy { min-width:330px; } }\
      /* Nested tab navigation: visually separate sub-tabs from the main tabs. */\
      #understanding-tabs-wrapper > .tabbable > .nav-tabs, #quality-tabs-wrapper > .tabbable > .nav-tabs { background:#d9e0e7; border:1px solid #aeb8c2; border-radius:7px 7px 0 0; padding:6px 6px 0; margin-bottom:0; }\
      #understanding-tabs-wrapper > .tabbable > .nav-tabs > li > a, #quality-tabs-wrapper > .tabbable > .nav-tabs > li > a { background:#c1ccd6; color:#263746; border:1px solid #9eabb7; border-bottom-color:#8795a2; margin-right:5px; font-weight:700; border-radius:5px 5px 0 0; }\
      #understanding-tabs-wrapper > .tabbable > .nav-tabs > li > a:hover, #quality-tabs-wrapper > .tabbable > .nav-tabs > li > a:hover { background:#aebdca; color:#15232f; }\
      #understanding-tabs-wrapper > .tabbable > .nav-tabs > li.active > a, #understanding-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:hover, #understanding-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:focus, #quality-tabs-wrapper > .tabbable > .nav-tabs > li.active > a, #quality-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:hover, #quality-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:focus { background:#fff; color:#1f2d3a; border-color:#8795a2; border-bottom-color:#fff; font-weight:800; }\
      #understanding-tabs-wrapper > .tabbable > .tab-content, #quality-tabs-wrapper > .tabbable > .tab-content { border:1px solid #aeb8c2; border-top:0; padding:12px 14px 4px; background:#fff; }\
    "))
    ),
    
    uiOutput("dynamic_accessibility_css"),
    
    div(
        class = "app-header",
        div(
            class = "app-title",
            h2(APP_CONFIG$title),
            p(class = "app-subtitle", APP_CONFIG$subtitle),
            app_signature
        ),
        div(
            class = "accessibility-panel",
            tags$strong("Accessibility and appearance"),
            checkboxInput("high_contrast", "High contrast", FALSE),
            checkboxInput("colorblind_safe", "Color-blind-safe palette", TRUE),
            checkboxInput("dark_mode", "Dark mode", FALSE),
            uiOutput("palette_message")
        )
    ),
    
    tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
            "1. Data understanding",
            br(),
            div(
                id = "understanding-tabs-wrapper",
                tabsetPanel(
                    id = "understanding_tabs",
                    tabPanel(
                        "Overview & plots",
                        br(),
                        div(class = "section-note", tags$strong("Dataset overview. "), APP_CONFIG$description),
                        uiOutput("dataset_metrics"),
                        fluidRow(
                            column(
                                4,
                                wellPanel(
                                    h4("Distribution plot"),
                                    checkboxGroupInput(
                                        "distribution_variables", "Numeric variables",
                                        choices = numeric_choices,
                                        selected = c("bill_length_mm", "bill_depth_mm")
                                    ),
                                    checkboxInput(
                                        "standardise_distributions",
                                        "Standardise selected variables before overlaying",
                                        TRUE
                                    ),
                                    radioButtons(
                                        "distribution_type", "Plot type",
                                        choices = c("Histogram" = "histogram", "Density plot" = "density", "Violin plot" = "violin"),
                                        selected = "histogram"
                                    ),
                                    conditionalPanel(
                                        "input.distribution_type == 'histogram'",
                                        sliderInput("histogram_bins", "Number of bins", 5, 60, 25, step = 1)
                                    ),
                                    p(class = "control-help", "Selected variables are overlaid in one plot. Standardisation is recommended because the measurements use different units and ranges.")
                                )
                            ),
                            column(8, plotOutput("distribution_plot", height = "520px"))
                        )
                    ),
                    tabPanel(
                        "Summary statistics & data sample",
                        br(),
                        h4("Summary statistics for the complete dataset"),
                        div(class = "table-scroll", tableOutput("summary_table")),
                        hr(),
                        h4("Data sample: first 5 observations"),
                        div(class = "table-scroll", tableOutput("data_sample"))
                    ),
                    tabPanel(
                        "Correlogram",
                        br(),
                        div(
                            class = "section-note",
                            tags$strong("Correlation matrix. "),
                            "Pearson correlations are calculated for numeric variables using pairwise complete observations."
                        ),
                        plotOutput("correlogram", height = "580px")
                    )
                )
            )
        ),
        
        tabPanel(
            "2. Data quality",
            br(),
            div(
                id = "quality-tabs-wrapper",
                tabsetPanel(
                    id = "quality_tabs",
                    tabPanel(
                        "Outlier assessment",
                        br(),
                        fluidRow(
                            column(
                                3,
                                selectInput("outlier_variable", "Numeric variable", choices = numeric_choices, selected = "body_mass_g"),
                                p(class = "control-help", "Potential outliers are identified globally with the 1.5 × IQR rule. They are diagnostic candidates, not rows to delete automatically.")
                            ),
                            column(
                                9,
                                plotOutput("outlier_plot", height = "430px")
                            )
                        ),
                        div(class = "table-scroll", tableOutput("outlier_table"))
                    ),
                    tabPanel(
                        "Missingness matrix",
                        br(),
                        plotOutput("missingness_matrix", height = "520px")
                    )
                )
            )
        ),
        
        tabPanel(
            "3. Interactive scatterplot",
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    tags$div(
                        class = "scatter-mode-box",
                        checkboxInput(
                            "use_3d",
                            "Use 3D scatterplot",
                            FALSE
                        ),
                        uiOutput("scatter_mode_status")
                    ),
                    radioButtons(
                        "scatter_color", "Map colour to",
                        choices = c("None" = "none", "Island" = "island", "Species" = "species"),
                        selected = "island"
                    ),
                    radioButtons(
                        "scatter_shape", "Map marker shape to",
                        choices = c("None" = "none", "Island" = "island", "Species" = "species"),
                        selected = "species"
                    ),
                    selectInput("scatter_x", "X-axis variable", numeric_choices, "bill_length_mm"),
                    selectInput("scatter_y", "Y-axis variable", numeric_choices, "body_mass_g"),
                    uiOutput("scatter_z_control"),
                    checkboxGroupInput(
                        "scatter_sex", "Include sex categories",
                        choices = c("Female" = "female", "Male" = "male", "Unknown" = "Unknown"),
                        selected = c("female", "male")
                    ),
                    uiOutput("trend_controls"),
                    checkboxInput("minimal_ink", "Minimise data-ink ratio", FALSE)
                ),
                mainPanel(width = 9, plotlyOutput("scatter_plot", height = "650px"))
            )
        ),
        
        tabPanel(
            "4. Islands map",
            br(),
            fluidRow(
                column(
                    3,
                    wellPanel(
                        checkboxGroupInput(
                            "map_species", "Include species",
                            choices = setNames(levels(penguins_data$species), levels(penguins_data$species)),
                            selected = levels(penguins_data$species)
                        ),
                        p(class = "control-help", "Only islands containing at least one selected species are displayed.")
                    )
                ),
                column(
                    9,
                    div(class = "section-note", "Click an island marker to display the island name, total number of selected penguins, and species counts. Island colours are identical across all tabs."),
                    leafletOutput("map_plot", height = "650px")
                )
            )
        ),
        
        tabPanel(
            "5. Model & model evaluation",
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    sliderInput("train_fraction", "Training data proportion", .55, .90, .75, step = .05),
                    sliderInput("tree_cp", "Complexity parameter (cp)", .001, .10, .01, step = .001),
                    sliderInput("tree_minsplit", "Minimum split size", 5, 60, 20, step = 1),
                    p(class = "control-help", "The decision tree predicts species from the four numeric measurements. The split is stratified and reproducible.")
                ),
                mainPanel(
                    width = 9,
                    uiOutput("model_metrics"),
                    plotOutput("tree_plot", height = "620px"),
                    fluidRow(
                        column(
                            6,
                            h4("Confusion matrix"),
                            tableOutput("confusion_matrix")
                        ),
                        column(
                            6,
                            h4("Per-class evaluation"),
                            tableOutput("class_metrics")
                        )
                    )
                )
            )
        )
    )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    selected_palette <- reactive({
        if (isTRUE(input$high_contrast)) palettes$high_contrast
        else if (isTRUE(input$colorblind_safe)) palettes$colorblind
        else palettes$default
    })
    
    island_palette <- reactive(selected_palette()$island)
    species_palette <- reactive(selected_palette()$species)
    
    output$palette_message <- renderUI({
        if (isTRUE(input$colorblind_safe) && !isTRUE(input$high_contrast)) {
            div(class = "palette-message", paste0("Palette ‘", palettes$colorblind$name, "’ used."))
        } else if (isTRUE(input$high_contrast)) {
            div(class = "palette-message", paste0("Palette ‘", palettes$high_contrast$name, "’ used."))
        } else {
            div(class = "palette-message", paste0("Palette ‘", palettes$default$name, "’ used."))
        }
    })
    
    output$scatter_mode_status <- renderUI({
        if (isTRUE(input$use_3d)) {
            div(class = "scatter-mode-status", "3D mode active")
        } else {
            div(class = "scatter-mode-status", "2D mode active")
        }
    })
    
    output$scatter_z_control <- renderUI({
        if (isTRUE(input$use_3d)) {
            selectInput("scatter_z", "Z-axis variable", numeric_choices, "flipper_length_mm")
        }
    })
    
    output$trend_controls <- renderUI({
        if (isTRUE(input$use_3d)) {
            return(NULL)
        }
        
        tagList(
            tags$div(
                class = "trend-controls-box",
                tags$strong("Trend lines (2D only)"),
                checkboxInput(
                    "trend_by_color",
                    "Add separate trend lines by mapped colour",
                    FALSE
                ),
                checkboxInput(
                    "trend_by_shape",
                    "Add separate trend lines by mapped marker shape",
                    FALSE
                )
            ),
            p(
                class = "control-help",
                "Colour trends are fitted separately for the variable selected in ‘Map colour to’. Shape trends are fitted separately for the variable selected in ‘Map marker shape to’. No overall trend is added."
            )
        )
    })
    
    output$dynamic_accessibility_css <- renderUI({
        css <- character()
        
        if (isTRUE(input$dark_mode)) {
            css <- c(css, "
        body { background:#121212 !important; color:#f5f5f5 !important; }
        .app-subtitle,.metric-label,.control-help { color:#d7d7d7 !important; }
        .accessibility-panel,.metric-card,.well,.section-note,.palette-message,.variable-checklist,.scatter-mode-box,.trend-controls-box { background:#1f1f1f !important; color:#f5f5f5 !important; border-color:#777 !important; }
        .nav-tabs>li>a { color:#eee !important; }
        .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover { background:#1f1f1f !important; color:#fff !important; border-color:#777 !important; }
        #quality-tabs-wrapper > .tabbable > .nav-tabs { background:#2d3339 !important; border-color:#737b83 !important; }
        #understanding-tabs-wrapper > .tabbable > .nav-tabs > li > a, #quality-tabs-wrapper > .tabbable > .nav-tabs > li > a { background:#3a424a !important; color:#f0f0f0 !important; border-color:#737b83 !important; }
        #understanding-tabs-wrapper > .tabbable > .nav-tabs > li > a:hover, #quality-tabs-wrapper > .tabbable > .nav-tabs > li > a:hover { background:#4a545e !important; color:#fff !important; }
        #quality-tabs-wrapper > .tabbable > .nav-tabs > li.active > a,#quality-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:hover,#quality-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:focus { background:#1f1f1f !important; color:#fff !important; border-bottom-color:#1f1f1f !important; }
        #understanding-tabs-wrapper > .tabbable > .tab-content, #quality-tabs-wrapper > .tabbable > .tab-content { background:#1f1f1f !important; border-color:#737b83 !important; }
        .form-control,.selectize-input,.selectize-dropdown { background:#202124 !important; color:#fff !important; border-color:#888 !important; }
        table { color:#f5f5f5 !important; }
      ")
        }
        
        if (isTRUE(input$high_contrast)) {
            css <- c(css, "
        body { font-size:17px !important; font-weight:600; }
        h1,h2,h3,h4,label,.control-label { font-weight:800 !important; }
        .accessibility-panel,.metric-card,.well,.section-note,.palette-message,.leaflet { border:3px solid currentColor !important; box-shadow:none !important; }
        .form-control,.selectize-input,.btn { border:3px solid currentColor !important; font-weight:700 !important; }
        input[type='checkbox'],input[type='radio'] { transform:scale(1.35); margin-right:8px; }
        a { text-decoration:underline !important; font-weight:800 !important; }
        *:focus { outline:4px solid #FFD800 !important; outline-offset:2px !important; }
        .nav-tabs>li.active>a { border-width:3px !important; }
      ")
        }
        
        if (!length(css)) return(NULL)
        tags$style(HTML(paste(css, collapse = "\n")))
    })
    
    plot_theme <- reactive({
        base <- if (isTRUE(input$dark_mode)) {
            theme_minimal(base_size = if (isTRUE(input$high_contrast)) 15 else 13) +
                theme(
                    plot.background = element_rect(fill = "#121212", colour = NA),
                    panel.background = element_rect(fill = "#121212", colour = NA),
                    legend.background = element_rect(fill = "#121212", colour = NA),
                    legend.key = element_rect(fill = "#121212", colour = NA),
                    text = element_text(colour = "#F5F5F5"),
                    axis.text = element_text(colour = "#F5F5F5"),
                    axis.title = element_text(colour = "#F5F5F5"),
                    panel.grid.major = element_line(colour = "#666"),
                    panel.grid.minor = element_line(colour = "#333")
                )
        } else {
            theme_minimal(base_size = if (isTRUE(input$high_contrast)) 15 else 13)
        }
        
        if (isTRUE(input$high_contrast)) {
            base + theme(
                text = element_text(face = "bold"),
                axis.text = element_text(face = "bold", colour = if (isTRUE(input$dark_mode)) "white" else "black"),
                axis.title = element_text(face = "bold"),
                panel.grid.major = element_line(linewidth = 1.05),
                panel.border = element_rect(colour = if (isTRUE(input$dark_mode)) "white" else "black", fill = NA, linewidth = 1.4)
            )
        } else base
    })
    
    output$dataset_metrics <- renderUI({
        dat <- APP_CONFIG$data
        div(
            class = "metric-grid",
            div(class = "metric-card", div(class = "metric-label", "Rows"), div(class = "metric-value", nrow(dat))),
            div(class = "metric-card", div(class = "metric-label", "Variables"), div(class = "metric-value", ncol(dat))),
            div(class = "metric-card", div(class = "metric-label", "Species"), div(class = "metric-value", n_distinct(dat[[APP_CONFIG$roles$species]], na.rm = TRUE))),
            div(class = "metric-card", div(class = "metric-label", "Islands"), div(class = "metric-value", n_distinct(dat[[APP_CONFIG$roles$island]], na.rm = TRUE))),
            div(class = "metric-card", div(class = "metric-label", "Missing values"), div(class = "metric-value", sum(is.na(dat)))),
            div(class = "metric-card", div(class = "metric-label", "Complete rows"), div(class = "metric-value", sum(complete.cases(dat))))
        )
    })
    
    output$summary_table <- renderTable({
        APP_CONFIG$data %>%
            summarise(across(all_of(numeric_columns), list(
                N = ~sum(!is.na(.x)), Missing = ~sum(is.na(.x)), Mean = ~mean(.x, na.rm = TRUE),
                SD = ~sd(.x, na.rm = TRUE), Median = ~median(.x, na.rm = TRUE),
                Min = ~min(.x, na.rm = TRUE), Max = ~max(.x, na.rm = TRUE)
            ), .names = "{.col}__{.fn}")) %>%
            pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "__") %>%
            mutate(
                Variable = unname(variable_labels[Variable]),
                across(c(Mean, SD, Median, Min, Max), ~round(.x, 2))
            )
    }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")
    
    output$data_sample <- renderTable({
        APP_CONFIG$data %>%
            head(5)
    }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")
    
    output$correlogram <- renderPlot({
        numeric_data <- APP_CONFIG$data %>%
            select(all_of(numeric_columns))
        
        correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
        
        correlation_data <- as.data.frame(as.table(correlation_matrix), stringsAsFactors = FALSE) %>%
            rename(variable_x = Var1, variable_y = Var2, correlation = Freq) %>%
            mutate(
                variable_x = factor(variable_x, levels = numeric_columns, labels = unname(variable_labels[numeric_columns])),
                variable_y = factor(variable_y, levels = rev(numeric_columns), labels = rev(unname(variable_labels[numeric_columns]))),
                label = sprintf("%.2f", correlation)
            )
        
        ggplot(correlation_data, aes(variable_x, variable_y, fill = correlation)) +
            geom_tile(colour = if (isTRUE(input$dark_mode)) "#333333" else "white", linewidth = 1) +
            geom_text(
                aes(label = label),
                colour = ifelse(abs(correlation_data$correlation) >= 0.55, "white", "black"),
                fontface = if (isTRUE(input$high_contrast)) "bold" else "plain",
                size = if (isTRUE(input$high_contrast)) 5 else 4.3
            ) +
            scale_fill_gradient2(
                low = "#0072B2", mid = "#F7F7F7", high = "#D55E00",
                midpoint = 0, limits = c(-1, 1), name = "Pearson r"
            ) +
            coord_equal() +
            labs(x = NULL, y = NULL, title = "Correlogram of numeric variables") +
            plot_theme() +
            theme(
                axis.text.x = element_text(angle = 35, hjust = 1),
                panel.grid = element_blank(),
                legend.position = "right"
            )
    })
    
    selected_distribution_variables <- reactive({
        vars <- input$distribution_variables
        validate(need(length(vars) > 0, "Select at least one numeric variable."))
        vars
    })
    
    distribution_long <- reactive({
        selected <- selected_distribution_variables()
        dat <- penguins_data %>%
            select(all_of(selected)) %>%
            pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
            filter(!is.na(value))
        
        if (isTRUE(input$standardise_distributions)) {
            dat <- dat %>%
                group_by(variable) %>%
                mutate(value = as.numeric(scale(value))) %>%
                ungroup()
        }
        
        dat %>%
            mutate(variable = factor(variable, levels = selected, labels = unname(variable_labels[selected])))
    })
    
    output$distribution_plot <- renderPlot({
        dat <- distribution_long()
        selected <- selected_distribution_variables()
        colours <- variable_palette[selected]
        names(colours) <- unname(variable_labels[selected])
        x_label <- if (isTRUE(input$standardise_distributions)) "Standardised value (z-score)" else "Observed value"
        
        if (input$distribution_type == "histogram") {
            ggplot(dat, aes(x = value, fill = variable, colour = variable)) +
                geom_histogram(
                    bins = input$histogram_bins,
                    alpha = 0.42,
                    position = "identity",
                    linewidth = 0.45
                ) +
                scale_fill_manual(values = colours, drop = FALSE) +
                scale_colour_manual(values = colours, drop = FALSE) +
                labs(
                    title = "Overlaid histograms of selected variables",
                    subtitle = if (isTRUE(input$standardise_distributions)) "Variables are standardised to make their distributions directly comparable." else "Raw values are overlaid; variables use different units and ranges.",
                    x = x_label, y = "Count", fill = "Variable", colour = "Variable"
                ) +
                plot_theme()
        } else if (input$distribution_type == "density") {
            ggplot(dat, aes(value, colour = variable, fill = variable)) +
                geom_density(alpha = .18, linewidth = 1.15) +
                scale_colour_manual(values = colours, drop = FALSE) +
                scale_fill_manual(values = colours, drop = FALSE) +
                labs(title = "Density curves for selected variables", x = x_label, y = "Density", colour = "Variable", fill = "Variable") +
                plot_theme()
        } else {
            ggplot(dat, aes(variable, value, fill = variable)) +
                geom_violin(trim = FALSE, alpha = .78) +
                geom_boxplot(width = .12, fill = "white", colour = "black", outlier.shape = NA) +
                scale_fill_manual(values = colours, drop = FALSE) +
                labs(title = "Selected-variable distributions", x = NULL, y = x_label, fill = "Variable") +
                plot_theme() + theme(legend.position = "none", axis.text.x = element_text(angle = 18, hjust = 1))
        }
    })
    
    potential_outliers <- reactive({
        req(input$outlier_variable)
        v <- input$outlier_variable
        
        # Penguin species have markedly different body-size distributions.
        # A single global IQR can therefore hide meaningful within-species
        # outliers. Limits are calculated independently for each species,
        # without exposing another grouping control in the interface.
        penguins_data %>%
            filter(!is.na(.data[[v]]), !is.na(species)) %>%
            group_by(species) %>%
            mutate(
                q1 = quantile(.data[[v]], .25, na.rm = TRUE),
                q3 = quantile(.data[[v]], .75, na.rm = TRUE),
                iqr_value = q3 - q1,
                lower_limit = q1 - 1.5 * iqr_value,
                upper_limit = q3 + 1.5 * iqr_value,
                is_outlier = .data[[v]] < lower_limit | .data[[v]] > upper_limit
            ) %>%
            ungroup()
    })
    
    output$outlier_plot <- renderPlot({
        v <- input$outlier_variable
        dat <- potential_outliers()
        out <- dat %>% filter(is_outlier %in% TRUE)
        
        ggplot(dat, aes(x = species, y = .data[[v]], fill = species)) +
            geom_boxplot(width = .55, alpha = .72, outlier.shape = NA) +
            geom_point(
                data = out,
                aes(x = species, y = .data[[v]]),
                inherit.aes = FALSE,
                shape = 21, size = 3.8, stroke = 1.2,
                fill = "white", colour = "black",
                position = position_jitter(width = .06, height = 0)
            ) +
            scale_fill_manual(values = species_palette(), drop = FALSE) +
            labs(
                title = paste("Potential outliers:", variable_labels[[v]]),
                subtitle = paste0(nrow(out), " observation(s) outside species-specific 1.5 × IQR limits"),
                x = NULL, y = variable_labels[[v]], fill = "Species"
            ) +
            plot_theme() +
            theme(legend.position = "none")
    })
    
    output$outlier_table <- renderTable({
        v <- input$outlier_variable
        out <- potential_outliers() %>% filter(is_outlier)
        if (!nrow(out)) return(data.frame(Result = "No potential outliers were detected with the selected rule."))
        out %>% transmute(
            Species = species, Island = island, Sex = sex_display,
            Variable = variable_labels[[v]], Value = round(.data[[v]], 2),
            `Lower IQR limit` = round(lower_limit, 2), `Upper IQR limit` = round(upper_limit, 2)
        )
    }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")
    
    output$missingness_matrix <- renderPlot({
        naniar::vis_miss(APP_CONFIG$data, sort_miss = TRUE, warn_large_data = FALSE) +
            labs(title = "Where values are missing", x = "Variables", y = "Observations") +
            plot_theme()
    })
    
    scatter_data <- reactive({
        req(input$scatter_x, input$scatter_y)
        validate(need(length(input$scatter_sex) > 0, "Select at least one sex category."))
        
        needed <- c(input$scatter_x, input$scatter_y)
        if (isTRUE(input$use_3d)) {
            req(input$scatter_z)
            validate(need(input$scatter_z %in% names(penguins_data), "Select a valid Z-axis variable."))
            needed <- c(needed, input$scatter_z)
        }
        
        penguins_data %>%
            filter(sex_display %in% input$scatter_sex) %>%
            filter(if_all(all_of(needed), ~!is.na(.x)))
    })
    
    output$scatter_plot <- renderPlotly({
        x_var <- input$scatter_x
        y_var <- input$scatter_y
        use_3d <- isTRUE(input$use_3d)
        z_var <- if (use_3d) input$scatter_z else NULL
        
        req(x_var, y_var)
        if (use_3d) {
            req(z_var)
            validate(need(z_var %in% names(penguins_data), "Select a valid Z-axis variable."))
        }
        
        dat <- scatter_data()
        color_var <- input$scatter_color
        shape_var <- input$scatter_shape
        colour_values <- switch(
            color_var,
            island = island_palette(),
            species = species_palette(),
            none = c("All observations" = "#000000")
        )
        shape_values <- switch(
            shape_var,
            island = shape_maps$island,
            species = shape_maps$species,
            none = c("All observations" = "circle")
        )
        color_title <- switch(color_var, island = "Island", species = "Species", none = "None")
        shape_title <- switch(shape_var, island = "Island", species = "Species", none = "None")
        
        dat <- dat %>% mutate(
            colour_group = if (color_var == "none") "All observations" else as.character(.data[[color_var]]),
            shape_group = if (shape_var == "none") "All observations" else as.character(.data[[shape_var]]),
            hover_text = paste0(
                "Species: ", species,
                "<br>Island: ", island,
                "<br>Sex: ", sex_display,
                "<br>", variable_labels[[x_var]], ": ", .data[[x_var]],
                "<br>", variable_labels[[y_var]], ": ", .data[[y_var]]
            )
        )
        
        if (use_3d) {
            dat <- dat %>% mutate(
                hover_text = paste0(
                    hover_text,
                    "<br>", variable_labels[[z_var]], ": ", .data[[z_var]]
                )
            )
        }
        
        bg <- if (isTRUE(input$dark_mode)) "#121212" else "#FFFFFF"
        fg <- if (isTRUE(input$dark_mode)) "#F5F5F5" else "#111111"
        grid <- if (isTRUE(input$minimal_ink)) "rgba(0,0,0,0)" else if (isTRUE(input$dark_mode)) "#555555" else "#DDDDDD"
        
        if (use_3d) {
            
            dat$.trace_group <- interaction(dat$colour_group, dat$shape_group, drop = TRUE, lex.order = TRUE)
            p <- plot_ly()
            
            for (trace_name in levels(dat$.trace_group)) {
                trace_data <- dat[dat$.trace_group == trace_name, , drop = FALSE]
                colour_name <- unique(trace_data$colour_group)[1]
                shape_name <- unique(trace_data$shape_group)[1]
                trace_colour <- if (colour_name %in% names(colour_values)) colour_values[[colour_name]] else "#000000"
                trace_symbol <- if (shape_name %in% names(shape_values)) shape_values[[shape_name]] else "circle"
                
                legend_name <- if (color_var == "none" && shape_var == "none") {
                    "All observations"
                } else if (color_var == "none") {
                    shape_name
                } else if (shape_var == "none") {
                    colour_name
                } else if (identical(colour_name, shape_name)) {
                    colour_name
                } else {
                    paste(colour_name, shape_name, sep = " / ")
                }
                
                p <- p %>% add_trace(
                    data = trace_data,
                    x = ~.data[[x_var]],
                    y = ~.data[[y_var]],
                    z = ~.data[[z_var]],
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(
                        symbol = unname(trace_symbol),
                        color = unname(trace_colour),
                        # Plotly/WebGL draws an additional outline around 3D markers when
                        # marker.line is used. For line-based symbols this can create a thick,
                        # two-colour glyph, especially in high-contrast mode.  Open
                        # symbols already carry their own visible stroke, so no extra
                        # marker outline is needed here.
                        size = if (isTRUE(input$high_contrast)) 8 else 6,
                        line = list(width = 0)
                    ),
                    hovertext = ~hover_text,
                    hoverinfo = "text",
                    name = legend_name,
                    legendgroup = legend_name,
                    showlegend = TRUE
                )
            }
            
            p <- p %>% layout(
                title = if (isTRUE(input$minimal_ink)) "" else paste(variable_labels[[y_var]], "vs", variable_labels[[x_var]], "and", variable_labels[[z_var]]),
                paper_bgcolor = bg,
                font = list(color = fg, size = if (isTRUE(input$high_contrast)) 15 else 12),
                scene = list(
                    bgcolor = bg,
                    xaxis = list(title = variable_labels[[x_var]], showgrid = !isTRUE(input$minimal_ink), zeroline = FALSE, gridcolor = grid),
                    yaxis = list(title = variable_labels[[y_var]], showgrid = !isTRUE(input$minimal_ink), zeroline = FALSE, gridcolor = grid),
                    zaxis = list(title = variable_labels[[z_var]], showgrid = !isTRUE(input$minimal_ink), zeroline = FALSE, gridcolor = grid)
                ),
                legend = list(title = list(text = paste(color_title, "/", shape_title)))
            )
        } else {
            p <- plot_ly(
                dat,
                x = ~.data[[x_var]], y = ~.data[[y_var]],
                type = "scatter", mode = "markers",
                color = ~colour_group, colors = unname(colour_values),
                symbol = ~shape_group, symbols = unname(shape_values),
                text = ~hover_text, hoverinfo = "text",
                marker = list(
                    size = if (isTRUE(input$high_contrast)) 11 else 9,
                    opacity = if (isTRUE(input$minimal_ink)) 1 else .80,
                    line = list(width = if (isTRUE(input$high_contrast)) 2 else .7, color = fg)
                )
            )
            
            add_group_trend <- function(plot_object, group_data, group_name, line_colour, line_dash, legend_group, suffix) {
                if (nrow(group_data) < 2 || n_distinct(group_data[[x_var]]) < 2) {
                    return(plot_object)
                }
                
                fit <- lm(reformulate(x_var, response = y_var), data = group_data)
                trend_x <- seq(
                    min(group_data[[x_var]], na.rm = TRUE),
                    max(group_data[[x_var]], na.rm = TRUE),
                    length.out = 100
                )
                pred_data <- setNames(data.frame(trend_x), x_var)
                trend_y <- predict(fit, newdata = pred_data)
                
                plot_object %>% add_lines(
                    x = trend_x,
                    y = trend_y,
                    name = paste(group_name, suffix),
                    legendgroup = legend_group,
                    line = list(
                        color = line_colour,
                        dash = line_dash,
                        width = if (isTRUE(input$high_contrast)) 5 else 3
                    ),
                    hoverinfo = "skip",
                    inherit = FALSE
                )
            }
            
            # Separate trends by the variable mapped to colour.  The split is made
            # directly on the source column, so an overall model is never fitted.
            if (isTRUE(input$trend_by_color) && color_var != "none") {
                trend_groups <- split(dat, as.character(dat[[color_var]]), drop = TRUE)
                for (group_name in names(trend_groups)) {
                    group_data <- trend_groups[[group_name]]
                    line_colour <- unname(colour_values[[group_name]])
                    if (is.null(line_colour) || is.na(line_colour)) line_colour <- fg
                    p <- add_group_trend(
                        p,
                        group_data,
                        group_name,
                        line_colour,
                        "solid",
                        paste0("colour_", group_name),
                        "colour trend"
                    )
                }
            }
            
            # Separate trends by the variable mapped to marker shape.  Each source
            # group receives its own regression and line type for monochrome output.
            if (isTRUE(input$trend_by_shape) && shape_var != "none") {
                trend_groups <- split(dat, as.character(dat[[shape_var]]), drop = TRUE)
                dash_values <- setNames(
                    rep(c("solid", "dash", "dot", "dashdot", "longdash"), length.out = length(trend_groups)),
                    names(trend_groups)
                )
                
                for (group_name in names(trend_groups)) {
                    group_data <- trend_groups[[group_name]]
                    p <- add_group_trend(
                        p,
                        group_data,
                        group_name,
                        fg,
                        unname(dash_values[[group_name]]),
                        paste0("shape_", group_name),
                        "shape trend"
                    )
                }
            }
            
            p <- p %>% layout(
                title = if (isTRUE(input$minimal_ink)) "" else paste(variable_labels[[y_var]], "vs", variable_labels[[x_var]]),
                xaxis = list(title = variable_labels[[x_var]], showgrid = !isTRUE(input$minimal_ink), zeroline = FALSE, gridcolor = grid, linecolor = fg),
                yaxis = list(title = variable_labels[[y_var]], showgrid = !isTRUE(input$minimal_ink), zeroline = FALSE, gridcolor = grid, linecolor = fg),
                paper_bgcolor = bg, plot_bgcolor = bg,
                font = list(color = fg, size = if (isTRUE(input$high_contrast)) 15 else 12),
                legend = list(orientation = "h", x = 0, y = -0.18, title = list(text = paste(color_title, "/", shape_title))),
                margin = list(b = 100)
            )
        }
        
        p %>% config(displaylogo = FALSE, responsive = TRUE)
    })
    
    map_summary <- reactive({
        validate(need(length(input$map_species) > 0, "Select at least one species."))
        
        # The species filter controls which islands are displayed.
        filtered <- penguins_data %>%
            filter(species %in% input$map_species)
        
        selected_totals <- filtered %>%
            count(island, name = "selected_total")
        
        # Popups always describe the complete species composition of each island,
        # independently of the currently selected map filter.
        all_species_counts <- penguins_data %>%
            count(island, species, name = "n") %>%
            complete(
                island = factor(c("Biscoe", "Dream", "Torgersen"), levels = levels(penguins_data$island)),
                species = factor(levels(penguins_data$species), levels = levels(penguins_data$species)),
                fill = list(n = 0)
            )
        
        all_totals <- penguins_data %>%
            count(island, name = "all_total")
        
        popup_rows <- all_species_counts %>%
            arrange(island, species) %>%
            group_by(island) %>%
            summarise(
                species_summary = paste0("<b>", species, ":</b> ", n, collapse = "<br>"),
                .groups = "drop"
            )
        
        APP_CONFIG$map %>%
            left_join(selected_totals, by = "island") %>%
            left_join(all_totals, by = "island") %>%
            left_join(popup_rows, by = "island") %>%
            mutate(selected_total = coalesce(selected_total, 0L)) %>%
            filter(selected_total > 0) %>%
            mutate(
                popup = paste0(
                    "<div style='min-width:190px'><h4 style='margin-top:0'>", island, " Island</h4>",
                    "<b>All penguins:</b> ", all_total,
                    "<hr style='margin:6px 0'>", species_summary, "</div>"
                )
            )
    })
    
    output$map_plot <- renderLeaflet({
        dat <- map_summary()
        pal <- island_palette()
        tile <- if (isTRUE(input$dark_mode)) providers$CartoDB.DarkMatter else providers$CartoDB.Positron
        shown_islands <- as.character(dat$island)
        
        leaflet(dat) %>%
            addProviderTiles(tile) %>%
            fitBounds(-66.1, -66.0, -63.4, -64.1) %>%
            addCircleMarkers(
                ~lon, ~lat,
                radius = if (nrow(dat) == 1) 16 else ~rescale(sqrt(selected_total), to = c(10, 18)),
                color = ~unname(pal[island]),
                fillColor = ~unname(pal[island]),
                fillOpacity = .82,
                weight = if (isTRUE(input$high_contrast)) 6 else 3,
                opacity = 1,
                popup = ~popup,
                label = ~paste0(island, " Island — ", selected_total, " penguins matching the filter")
            ) %>%
            addLegend(
                "bottomright",
                colors = unname(pal[shown_islands]),
                labels = shown_islands,
                title = "Island",
                opacity = 1
            )
    })
    
    model_split <- reactive({
        dat <- penguins_data %>%
            select(species, all_of(numeric_columns)) %>% drop_na()
        set.seed(2026)
        train_idx <- unlist(lapply(split(seq_len(nrow(dat)), dat$species), function(idx) sample(idx, max(1, floor(length(idx) * input$train_fraction)))))
        list(train = dat[train_idx, , drop = FALSE], test = dat[-train_idx, , drop = FALSE])
    })
    
    fitted_tree <- reactive({
        split <- model_split()
        rpart(APP_CONFIG$model_formula, data = split$train, method = "class", control = rpart.control(cp = input$tree_cp, minsplit = input$tree_minsplit))
    })
    
    model_evaluation <- reactive({
        split <- model_split(); model <- fitted_tree()
        pred <- predict(model, newdata = split$test, type = "class")
        cm <- table(Actual = split$test$species, Predicted = pred)
        classes <- union(rownames(cm), colnames(cm))
        cm_full <- matrix(0, length(classes), length(classes), dimnames = list(Actual = classes, Predicted = classes))
        cm_full[rownames(cm), colnames(cm)] <- cm
        accuracy <- sum(diag(cm_full)) / sum(cm_full)
        per_class <- lapply(classes, function(cls) {
            tp <- cm_full[cls, cls]; fp <- sum(cm_full[, cls]) - tp; fn <- sum(cm_full[cls, ]) - tp; tn <- sum(cm_full) - tp - fp - fn
            precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
            recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
            f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
            specificity <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
            data.frame(Class = cls, Precision = precision, Recall = recall, Specificity = specificity, F1 = f1)
        }) %>% bind_rows()
        list(cm = cm_full, accuracy = accuracy, balanced_accuracy = mean(per_class$Recall, na.rm = TRUE), per_class = per_class, test_n = nrow(split$test))
    })
    
    output$tree_plot <- renderPlot({
        model <- fitted_tree()
        split <- model_split()
        
        # For a multiclass rpart model, rpart.plot expects one colour entry
        # for each response level. Read the levels from the training response;
        # model$ylevels is not reliable across rpart versions.
        class_order <- levels(droplevels(split$train$species))
        class_colours <- unname(species_palette()[class_order])
        
        # Defensive fallback: never pass an incomplete palette to rpart.plot.
        if (length(class_colours) != length(class_order) || anyNA(class_colours)) {
            class_colours <- rep("lightgray", length(class_order))
        }
        
        box_palette <- as.list(class_colours)
        
        tryCatch(
            rpart.plot::rpart.plot(
                model,
                type = 4,
                extra = 104,
                fallen.leaves = TRUE,
                branch = 0.45,
                box.palette = box_palette,
                shadow.col = "gray75",
                nn = TRUE,
                faclen = 0,
                varlen = 0,
                tweak = 1.05,
                main = "Decision tree for species classification"
            ),
            error = function(e) {
                rpart.plot::rpart.plot(
                    model,
                    type = 4,
                    extra = 104,
                    fallen.leaves = TRUE,
                    branch = 0.45,
                    box.palette = "auto",
                    shadow.col = "gray75",
                    nn = TRUE,
                    faclen = 0,
                    varlen = 0,
                    tweak = 1.05,
                    main = "Decision tree for species classification"
                )
            }
        )
    }, res = 110)
    
    output$model_metrics <- renderUI({
        ev <- model_evaluation()
        div(class = "metric-grid",
            div(class = "metric-card", div(class = "metric-label", "Test observations"), div(class = "metric-value", ev$test_n)),
            div(class = "metric-card", div(class = "metric-label", "Accuracy"), div(class = "metric-value", percent(ev$accuracy, accuracy = .1))),
            div(class = "metric-card", div(class = "metric-label", "Balanced accuracy"), div(class = "metric-value", percent(ev$balanced_accuracy, accuracy = .1)))
        )
    })
    
    output$confusion_matrix <- renderTable({
        as.data.frame.matrix(model_evaluation()$cm) %>% tibble::rownames_to_column("Actual")
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    output$class_metrics <- renderTable({
        model_evaluation()$per_class %>% mutate(across(where(is.numeric), ~round(.x, 3)))
    }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")
}

shinyApp(ui, server)