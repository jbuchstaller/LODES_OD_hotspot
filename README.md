# LODES_OD_hotspot

Goal: Visualize the number of workers in each censustract in South East Michigan and their origins

Based on 2015 LODES data (https://lehd.ces.census.gov/data/) I created a clickable shiny application that shows on a map how many people work in each censustract. By clicking on the census tract, the user can see hotspots the workers commute from.

Download the folder with all the files and run the file "app.R" in R-Studio, setting the working directory to the folder. It might take a while to load the data and display the maps.


Deployed at https://geospatially.shinyapps.io/lodes_od_hotspot/

Hotspot analysis methodology:
I calculated the Getis-Ord Gi* statistic (z-score) for each block with 1-hour driving distance of the origin (censustract clicked by user). This statistic determines whether the local average number of commuters is significantly higher or lower than the global average. To choose the number of neighbors used for the local average (not shown), I computed the global Moran's I statistic for a number of scenarios (1 to 100 neighbors) and choose the number of neigbors that resulted in maximal global Moran's I (value where spatial clustering is strongest) and then constructed the spatial relation matrix accordingly.






