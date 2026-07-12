library(shiny) # for dashboard
library(palmerpenguins) # data
library(ggplot2) #chart (plotly if interactivity with charts are welcome)
library(dplyr) # from tidyverse, cleaning data
library(leaflet) # for map

# make and UI
ui <- fluidPage(
    titlePanel("Dashboard for Palmer Archipelago Penguins"),
    
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("speciesInput", "Choose species:",
                               choices = unique(penguins$species), 
                               selected = unique(penguins$species)),
            checkboxGroupInput("sexInput", "Choose sex:",
                               choices = unique(penguins$sex), 
                               selected = unique(penguins$sex)),
            selectInput("islandInput", "Choose island:",
                        choices = unique(penguins$island),
                        selected = unique(penguins$island)[1]),
            sliderInput("billLengthInput", "Range of bill length [mm]:",
                        min = floor(min(penguins$bill_length_mm, na.rm = TRUE)),
                        max = ceiling(max(penguins$bill_length_mm, na.rm = TRUE)),
                        value = c(floor(min(penguins$bill_length_mm, na.rm = TRUE)), 
                                  ceiling(max(penguins$bill_length_mm, na.rm = TRUE))))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Chart", plotOutput("scatterPlot")),
                tabPanel("Map", leafletOutput("mapPlot"))
            )
        )
    )
)

server <- function(input, output) {
    filteredData <- reactive({
        penguins %>% # see dplyr for info
            filter(
                species %in% input$speciesInput,
                sex %in% input$sexInput,
                island %in% input$islandInput,
                !is.na(bill_length_mm),
                bill_length_mm >= input$billLengthInput[1],
                bill_length_mm <= input$billLengthInput[2]
            ) %>%
            na.omit()
    })
    
    output$scatterPlot <- renderPlot({
        ggplot(filteredData(),
               aes(x = bill_length_mm,
                   y = body_mass_g,
                   color = species)) +
            geom_point(size = 3, alpha = 0.7) +
                        labs(title = "Bill length vs Boby mass",
                             x = "Bill length (mm)", y = "Body mass (g)") +
            theme_minimal()
    })
    
    output$mapPlot <- renderLeaflet({
        # Islands coordinates
        island_coords <- data.frame(
            island = c("Torgersen", "Biscoe", "Dream"),
            lat = c(-64.767, -65.533, -64.733),
            lon = c(-64.067, -65.067, -64.233)
        )
        
        # Island: selected and others
        selected_coords <- island_coords %>% filter(island == input$islandInput)
        other_coords <- island_coords %>% filter(island != input$islandInput)
        
        # Creating map
        leaflet() %>%
            addTiles() %>%
            # Markers for selected island (emphasis-pre-attentive attribute)
            addCircleMarkers(data = selected_coords, 
                             lat = ~lat, 
                             lng = ~lon, 
                             color = "red", 
                             radius = 10, 
                             popup = ~paste("Island:", island)) %>%
            # Marker for others
            addCircleMarkers(data = other_coords, 
                             lat = ~lat, 
                             lng = ~lon, 
                             color = "blue", 
                             radius = 6, 
                             popup = ~paste("Island:", island))
    })
}

shinyApp(ui, server)
