library(shiny)
library(tidyverse)
library(tmap)
library(sf)
library(countrycode)
library(plotly)
library(CGPfunctions)
library(gganimate)
library(gifski)
library(png)
library(magick)
library(corrplot)
library(ggstatsplot)
library(heatmaply)
library(rstantools)

data <- read_csv("data/gii_dataset_2015-2020_wide_score.csv")

data_2 <- read_csv("data/gii_dataset_2015-2020_wide_score_with_region.csv")

years <- data %>%
    select(Year) %>%
    unique() %>%
    as.matrix()

cty <- data %>%
    select(Country) %>%
    unique() %>%
    as.matrix()

cty_ranked <- data %>%
    arrange(desc(`GLOBAL INNOVATION INDEX`)) %>%
    select(Country) %>%
    unique() %>%
    as.matrix()

indicators <- data %>%
    pivot_longer(cols = !Country:Category,
                 names_to = "Indicator",
                 values_to = "Score") %>%
    select(Indicator) %>%
    arrange(Indicator) %>%
    unique() %>%
    as.matrix()

shp <- st_read(dsn = "data/shape_files",
               layer = "TM_WORLD_BORDERS-0.3")

# map_data <- data %>%
#     mutate(iso3 = countrycode(Country, origin = 'country.name.en', destination = 'iso3c')) %>%
#     mutate(region = countrycode(Country, origin = 'country.name.en', destination = 'un.region.name'))

map_data <- read_csv("data/map_data.csv")

map_data <- left_join(shp, map_data,
                      by = c("ISO3" = "iso3"))



shinyServer(function(input, output) {
    
    output$map <- renderTmap({
        
        yr <- input$year
        ind <- input$indicator
        
        map <- map_data %>%
            select(Country, Year, ind, ISO3) %>%
            filter(Year == yr)
        
        tmap_mode("view")
        
        tm_basemap("CartoDB.DarkMatter") +
            tm_tiles("CartoDB.DarkMatter")+
            tm_shape(map)+
            tm_polygons(ind, palette = "Blues", alpha = 0.5) +
            tm_view(set.view = 2)
        
    })
    
    output$radar <- renderPlotly({
        
        radar_data <- data %>%
            filter(Year == input$year_2) %>%
            select(Country, input$indicator_2) %>%
            replace(is.na(.), 0) %>%
            pivot_longer(cols = input$indicator_2, names_to = "Indicator", values_to = "Score" )
        
        fig <- plot_ly(
            type = 'scatterpolar',
            fill = 'toself',
            mode = 'markers'
        ) 
        
        for(c in input$cty){
            
            r_data <- radar_data %>% filter(Country == c)
            
            fig <- fig %>%
                add_trace(
                    data = r_data,
                    r = r_data$Score,
                    theta = r_data$Indicator,
                    name = r_data$Country
                )
        }
        
        fig <- fig %>%
            layout(
                polar = list(
                    bgcolor = "darkgrey",
                    radialaxis = list(
                        visible = T,
                        range = c(0,100),
                        gridcolor = "white",
                        linecolor = "red"
                    ),
                    
                    angularaxis = list(tickfont = list(size = 14))
                ),
                showlegend = T
            )
        
        fig
        
    })
    
    
    output$slope <- renderPlot({
        
        slope_data <- data_2 %>%
            filter(Country %in% input$cty_2) %>%
            select(Country, Year, input$indicator_3) %>%
            mutate(Year = as.numeric(Year)) %>%
            filter(Year %in% input$year_3) %>%
            mutate(Year = as.factor(Year)) %>%
            mutate(indicate = !!rlang::sym(input$indicator_3))
        
        newggslopegraph(dataframe = slope_data,
                        Times = Year,
                        Measurement = indicate,
                        Grouping = Country,
                        Title = paste("Trend of", input$indicator_3, "through Selected Years"),
                        SubTitle = "Based on: Global Innovation Index",
                        Caption = NULL,
                        LineThickness = .5,
                        YTextSize = 5,
                        DataTextSize = 4)
        
    })
    
    output$bubble <- renderImage({
        
        
        input$submit_bubble
        
        isolate({
           
            temp_df <- data_2 %>%
                select(Country, region, Year, input$indicator_4_x, input$indicator_4_y, input$indicator_4_sz) %>%
                mutate(Year = as.integer(Year)) %>%
                filter(Country %in% input$cty_2)
            
            p <- ggplot(temp_df, aes(x = !! sym(input$indicator_4_x), 
                                     y = !! sym(input$indicator_4_y), 
                                     size = !! sym(input$indicator_4_sz),
                                     color = region)) +
                geom_point(alpha = 0.5, show.legend = T) +
                scale_size(range = c(.1, 18)) +
                
                geom_text(aes(label=Country),hjust=0, vjust=0, size = 5) +
                
                labs(title = 'Year: {frame_time}', x = input$indicator_4_x, y = input$indicator_4_y) +
                transition_time(Year) +
                ease_aes('linear')
            
            anim_save("outfile.gif", animate(p, height = 800, width =800))
            
            list(src = "outfile.gif",
                 contentType = 'image/gif'
                 # width = 400,
                 # height = 300,
                 # alt = "This is alternate text"
            ) 
            
        })
        

        
    }, deleteFile = TRUE)
    
    
    output$correlation <- renderPlot({
        
        temp_df <- data_2 %>%
            filter(Year %in% input$year_5) %>%
            filter(Country %in% input$cty_5) %>%
            select(input$indicator_5) %>%
            replace(is.na(.), 0)
        
        if(input$plotOrder == "hclust"){
            
            corrplot(cor(temp_df, method = input$corMethod, use = input$corUse), 
                     diag = FALSE, order = input$plotOrder, method = input$plotMethod, type = input$plotType,
                     hclust.method = input$plotHclustMethod, addrect = input$plotHclustAddrect)
            
        }
        else{
            
            corrplot(cor(temp_df, method = input$corMethod, use = input$corUse), 
                     diag = FALSE, order = input$plotOrder, method = input$plotMethod, type = input$plotType)
            
        }
        
    })
    
    
    output$stats <- renderPlot({
        
        temp_df <- data_2 %>%
            filter(Year %in% input$year_6) %>%
            mutate(temp_ind = !! sym(input$indicator_6)) %>%
            select(temp_ind, region) %>%
            replace(is.na(.), 0)
        
        ggbetweenstats(
            data = temp_df,
            x = region, 
            y = temp_ind, 
            title = input$indicator_6,
            type = 'parametric',
            conf.level = 0.95,
            pairwise.comparisons = TRUE,
            pairwise.display = 'significant', 
            p.adjust.method = 'holm', 
            ggtheme = ggplot2::theme_classic()
            
        )
        
    })
    
    output$hcluster <- renderPlotly({
        
        temp_df <- data_2 %>%
            filter(Year == input$year_7) %>%
            select(Country, input$indicator_7) %>%
            replace(is.na(.), 0) %>%
            column_to_rownames(var = "Country") %>%
            percentize()
        
        heatmaply(temp_df, k_row = input$krow, k_col = input$kcol,
                  dist_method = input$distMethod, hclust_method = input$hclusMethod)
        
    })
    
    
    

})


