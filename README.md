# California Crash Data Explorer

## Overview
An interactive Shiny app for exploring car crash data in California (2014–2023). The app offers visualizations and spatial analysis to identify patterns in crash frequency, severity, causes, and geographic distribution. NOTE: csv too large for upload, only code is available. 
## Features

### **Crash Analysis Tab**
Explore crash trends and statistics with dynamic plots:

- **Plot 1: Crashes by Time**
  - Choose to group crashes by **Month**, **Day of Week**, or **Time of Day**
  - Faceted by **Collision Severity**

- **Plot 2: Top 10 Categories**
  - Visualize crash counts by top 10 values in:
    - **Type of Collision**
    - **Violation Type**
    - **City**
    - **Day of Week**

- **Plot 3: Average Casualties**
  - View average casualties per crash by **City** or **Type of Collision**

Interactive filters include:
- City selection (or all cities)
- Year range (2021–2023)
- Collision severity (multi-select)

### **Crash Map Tab**
Visualize crash locations on a **Leaflet Map** with:
- City-level filtering
- Year range selection
- Color-coded markers by:
  - **Collision Severity**
  - **Violation Type**
- Popups with crash details including date, city, and selected metric

## Dataset
The app uses `crashes_california_2014_2023.csv`, which includes detailed crash records from California between 2014 and 2023. Data fields include:
- Date & Time
- Weather & Road Conditions
- Collision Severity
- Vehicle Involvement (motorcycle, truck, pedestrian, etc.)
- Geographic Coordinates

## How to Run

1. Ensure you have R and the following packages installed:
```r
install.packages(c("shiny", "tidyverse", "lubridate", "leaflet", "plotly"))
```
  - Place all files in the same directory
  - Run the app
