source("Worker_Census_Tracts_Blocks_DB2.R")
source(".Rprofile")

library(shiny)
library(leaflet)
library(RCurl)
library(httr)
library(tidyr)
library(jsonlite)
library(spdep)
library(rgeos)

ui <- fluidPage(uiOutput('title'),
                leafletOutput("map"),
                htmlOutput('text'))

server <- function(input, output, session) {
  colorpal <- reactive({
    colorBin(
      "RdYlBu",
      bins = c(-3,-2,-1, 0, 1, 5, 10, 15, 20, 40),
      domain = c(-3,-2,-1, 0, 1, 5, 10, 15, 20, 40),
      reverse = TRUE
    )
  })
  
  colorpal2 <- reactive({
    colorBin("RdYlBu", c(1, 5, 100, 500, 1000), reverse = TRUE)
  })
  
  filteredData <- reactive({
    withProgress(message = 'Calculating hotspots', detail = 'Please be patient, it may take a while to display', value = 0, {
      incProgress(1 / 9) #1
      LODES_JT00_2015_S100_tri_business_district <-
        filter(
          LODES_JT00_2015_S100_tri,
          w_geocode_trct == as.numeric(input$map_shape_click)[[1]]
        )
      incProgress(1 / 9) #2
      LODES_JT00_2015_S100_tri_business_district <-
        group_by(LODES_JT00_2015_S100_tri_business_district, h_geocode)
      #sum over h_geocodes
      LODES_JT00_2015_S100_tri_business_district <-
        mutate(LODES_JT00_2015_S100_tri_business_district,
               sumS000byh_geocode = sum(S000))
      incProgress(1 / 9)#3
      LODES_JT00_2015_S100_tri_h_selected <-
        unique(
          select(
            LODES_JT00_2015_S100_tri_business_district,
            h_geocode,
            h_lon,
            h_lat,
            sumS000byh_geocode
          )
        )
      incProgress(1 / 9)#4
      mi_censustracts_LODES_OD_w_trct_top <-
        subset(
          mi_censustracts_LODES_OD_w_trct,
          GEOID == as.numeric(input$map_shape_click)[[1]]
        )
      here_maps_url = "https://isoline.route.api.here.com/routing/7.2/calculateisoline.json"
      longitude = as.character(mi_censustracts_LODES_OD_w_trct_top@data$INTPTLON)
      latitude = as.character(mi_censustracts_LODES_OD_w_trct_top@data$INTPTLAT)
      res <- GET(
        here_maps_url,
        query = list(
          start = paste('geo!', latitude, ',', longitude, sep = ''),
          app_id = HERE_APP_ID,
          app_code = HERE_APP_CODE,
          mode = 'shortest;car;traffic:disabled',
          range = '3600',
          # 1hour
          rangetype = 'time'
        )
      )
      incProgress(1 / 9)#5
      recs <- fromJSON(content(res, as = "text"))
      isoline <- recs$response$isoline[[2]]
      isolineShape <- as.data.frame(c(isoline[[1]]$shape))
      colnames(isolineShape) <- c('geom')
      isolineShape_tidy <-
        separate(isolineShape,
                 geom,
                 into = c('latitude', 'longitude'),
                 sep = ',')
      isolineShape_tidy$latitude <-
        as.numeric(isolineShape_tidy$latitude)
      isolineShape_tidy$longitude <-
        as.numeric(isolineShape_tidy$longitude)
      isolineShape_tidy <-
        select(isolineShape_tidy, longitude, latitude) #lon first then lat
      isoline_spatial_line <-
        SpatialLines(list(Lines(Line(
          isolineShape_tidy
        ), ID = "a")))
      my_polygon <- gPolygonize(isoline_spatial_line)
      LODES_JT00_2015_S100_tri_h_selected_lon_lat <-
        select(LODES_JT00_2015_S100_tri_h_selected, h_lon, h_lat)
      coordinates(LODES_JT00_2015_S100_tri_h_selected_lon_lat) <-
        c("h_lon", "h_lat")
      insidePolygon <-
        !is.na(over(
          LODES_JT00_2015_S100_tri_h_selected_lon_lat,
          as(my_polygon, "SpatialPolygons")
        ))
      LODES_JT00_2015_S100_tri_h_selected$insidePolygon <-
        insidePolygon
      incProgress(1 / 9)#6
      LODES_JT00_2015_S100_tri_h_selected_insidePolygon <-
        filter(LODES_JT00_2015_S100_tri_h_selected,
               insidePolygon == TRUE)
      p <- LODES_JT00_2015_S100_tri_h_selected_insidePolygon
      xy_p <-
        coordinates(LODES_JT00_2015_S100_tri_h_selected_insidePolygon[, 1:2])
      y <- p$sumS000byh_geocode
      test <- knn2nb(knearneigh(xy_p, 40))
      localGvalues <-
        localG(
          x = as.numeric(y),
          listw = nb2listw(test, style = "B"),
          zero.policy = TRUE
        )
      incProgress(1 / 9)#7
      localGvalues <- round(localGvalues, 3)
      incProgress(1 / 9)#8
      LODES_JT00_2015_S100_tri_h_selected_insidePolygon$localGvalues <-
        localGvalues
      #})
      incProgress(1 / 9) #9
      p <- LODES_JT00_2015_S100_tri_h_selected_insidePolygon
    })
  })
  
  filteredData3 <- reactive({
      mi_censustracts_LODES_OD_w_trct_top <-
        subset(
          mi_censustracts_LODES_OD_w_trct,
          GEOID == as.numeric(input$map_shape_click)[[1]]
        )
    })

  
  output$text <-
    renderText(
      paste(
        "Number of Workers in Census Tracts in SE Michigan. Red tracts have a high number
        of workers in that tract and are sites of large employers. ",
        "Please zoom in and click on a tract to see the origins of these workers",
        sep = "\n"
      )
    )
  
  output$title <- renderUI({
    titlePanel("Workers in Census Tracts")
  })
  
  output$map <- renderLeaflet({
    bins2 <-
      c(1, 5, 100, 500, 1000, 3000, 10000, 20000, 30000, 50000, 60000)
    pal2 <-
      colorBin(
        palette = "RdYlBu",
        domain = mi_censustracts_LODES_OD_w_trct$sumS000byw_geocode_trct,
        bins = bins2,
        reverse = TRUE
      )
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(
        data = mi_censustracts_LODES_OD_w_trct,
        fillColor = ~ pal2(
          mi_censustracts_LODES_OD_w_trct$sumS000byw_geocode_trct
        ),
        fillOpacity = 0.8,
        color = "#000000",
        weight = 1,
        layerId = mi_censustracts_LODES_OD_w_trct$GEOID
      ) %>%
      addLegend(
        "bottomright",
        pal = pal2,
        values = mi_censustracts_LODES_OD_w_trct$sumS000byw_geocode_trct,
        title = "Number of workers",
        opacity = 1
      )
  })
  
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (click$id == 0 || is.null(click$id)) {
      return()
    }
    
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        data = MI_SP_trct,
        color = "#000000",
        weight = 1,
        fillOpacity = 0,
        options = pathOptions(clickable = FALSE)
      ) %>%
      addCircleMarkers(
        lng = ~ h_lon,
        lat = ~ h_lat,
        color = ~ pal(localGvalues),
        #radius = ~sumS000byh_geocode/50000,
        popup = ~ paste0(sumS000byh_geocode),
        fillOpacity = 1,
        stroke = FALSE,
        radius = 3
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~ localGvalues,
        title = "Hotspot",
        opacity = 1
      ) %>%
      addPolygons(
        data = filteredData3(),
        color = "red",
        fill = FALSE,
        opacity = 0.5
      ) %>%
      setView(click$lng, click$lat, zoom = 9)
    
    output$title <- renderUI({
      fluidPage(titlePanel("Origins of Workers"))
    })
    
    output$text <-
      renderText(
        paste0(
          "Origins of people commuting into the chosen censustract. Red dots designate blocks with
          a significantly higher number of commuters going into the chosen tract when compared to
          the surrounding areas. Blue dots represent blocks from which a significantly lower number
          of commuters goes into the chosen tract. ",
          "To repeat the analysis with another tract, please reload the page.",
          sep = "\n"
        )
      )
  })
  
}

shinyApp(ui = ui, server = server)
