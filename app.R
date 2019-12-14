library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(scales)
library(sf)
library(geojsonio)

df <- read_csv('data/crimedata_csv_all_years_modified.csv')

list_of_locations <- df['NEIGHBOURHOOD'] %>% drop_na() %>% distinct() %>% add_row('NEIGHBOURHOOD' = 'ALL')
list_of_crimes = df['TYPE'] %>% distinct() %>% add_row('TYPE' = 'ALL')
list_of_years = list('YEAR', 'MONTH', 'DAY OF WEEK', 'HOUR')
min_year = df['YEAR'] %>% min()
max_year = df['YEAR'] %>% max()
yearMarks <- lapply(unique(df$YEAR), as.character)
names(yearMarks) <- unique(unique(df$YEAR))

vancouver <- sf::st_read('data/our_geojson_modified.geojson')
crime <- read_csv("data/crimedata_csv_all_years_modified.csv")
crime$HUNDRED_BLOCK <- NULL
crime$X <- NULL
crime$Y <- NULL

crime <- crime %>% 
    group_by(NEIGHBOURHOOD, TYPE, YEAR) %>%
    summarise(COUNT = n())

plot_choropleth <- function(year_init = 2010, year_end = 2018, crime_type = 'ALL', crime_threshold = 1) {
    crime_cnt <- crime %>% 
        filter(YEAR >= year_init & YEAR <= year_end)
    
    if(crime_type != 'ALL') {
        crime_cnt <- crime_cnt %>%
            filter(TYPE == crime_type)
    }
    crime_cnt <- crime_cnt %>% 
        group_by(NEIGHBOURHOOD) %>%
        summarise(sum = sum(COUNT)) %>%
        mutate(min_max = (sum - min(sum)) / (max(sum) - min(sum)))
    
    full_dt <- full_join(vancouver, crime_cnt, by = c('NEIGHBOURHOOD', 'NEIGHBOURHOOD'))
    ggplotly(full_dt %>%
        ggplot(mapping = aes(fill = .data[['min_max']])) +
        geom_sf(color = 'white', size = 0.2) +
        scale_fill_viridis_c(option = 'viridis', 
                             name = "Crime Index", 
                             labels = comma, 
                             limits = c(0, crime_threshold)) +
        labs(title = paste("Crime Type =", crime_type)))
}
graph_choropleth <- dccGraph(
  id = 'choropleth',
  figure=plot_choropleth() # gets initial data using argument defaults
)

plot_func <- function(df_line=df, start=2010, end=2018, neighbourhood_1='ALL', neighbourhood_2='ALL', crime='ALL', time_scale='YEAR') {
    # !!sym(time_scale)

    df <- df_line %>% filter(YEAR >= start & YEAR <= end)
    crime_title = crime
    neighbourhood_1_title = neighbourhood_1
    neighbourhood_2_title = neighbourhood_2

    if (time_scale == 'DAY OF WEEK') {

        time_scale <- 'DAY_OF_WEEK'
    }
    
    if (crime == 'ALL') {
            crime_title = 'All Crimes'
            if (neighbourhood_1 == 'ALL') {
                neighbourhood_1_title = 'All Neighbourhoods'
                    df1 <- df %>% 
                        group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))

            } else {
                df1 <- df %>% 
                    filter(NEIGHBOURHOOD == neighbourhood_1) %>%
                group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
        }   
    } else {
    
    if (neighbourhood_1 == 'ALL') {
                neighbourhood_1_title = 'All Neighbourhoods'
                df1 <- df %>% 
                filter(TYPE == crime) %>%
                group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
            
        } else {
            df1 <- df %>% 
                filter((NEIGHBOURHOOD == neighbourhood_1 & TYPE == crime)) %>%
                group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
        
    }
}

    if (crime == 'ALL') {
        crime_title = 'All Crimes'
        if (neighbourhood_2 == 'ALL') {
                neighbourhood_2_title = 'All Neighbourhoods'
                df2 <- df %>% 
                    group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
                
        } else {
            df2 <- df %>% 
                filter(NEIGHBOURHOOD == neighbourhood_2) %>%
                group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
        }   
    } else {
        
        if (neighbourhood_2 == 'ALL') {
                neighbourhood_2_title = 'All Neighbourhoods'
                df2 <- df %>% 
                filter(TYPE == crime) %>%
                group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
            
        } else {
            df2 <- df %>% 
                filter((NEIGHBOURHOOD == neighbourhood_2 & TYPE == crime)) %>%
                group_by(!!sym(time_scale)) %>%
                tally() %>% 
    mutate(!!sym(time_scale) := as.factor(!!sym(time_scale)))
    }
                    }
    ggplotly(ggplot() +
        geom_line(df1, mapping = aes(x=!!sym(time_scale), y=n, group=1), color='blue') +
        geom_line(df2, mapping = aes(x=!!sym(time_scale), y=n, group=1), color='red') +
        ylab('Number of Crimes') +
        ggtitle(paste(neighbourhood_1_title, "vs", neighbourhood_2_title, ":", crime_title)) +
        theme_bw())
}

graph_line <- dccGraph(
  id = 'line_chart',
  figure=plot_func() # gets initial data using argument defaults
)

types <- unique(crime$TYPE)
plot_choropleth(year_init = 2016, crime_type = types[3], crime_threshold = 0.3)
plot_func()
app <- Dash$new()

app$layout(
  htmlDiv(
      list(
        
        # Header
        htmlDiv(
          list(
              htmlImg(src='https://img.icons8.com/wired/64/000000/policeman-male.png', style = list(float = 'left', marginTop = '10px', marginLeft = '8px')),
              htmlH1('Vancouver Crime Stats', style = list(float = 'left', marginLeft = '15px')),
              htmlDiv(children="Open source crime data from the City of Vancouver shown for neighbourhood comparison", style = list(position = 'absolute', float = 'left', marginTop = '60px', marginLeft = '90px'))
              ),
           style = list(position = 'absolute', width = '96%', height = '90px', backgroundColor = '#9ee6f6', border = '3px solid black')
        ),

        # Crime Map
        htmlDiv(
            list(
                htmlDiv(
                    list(
                        htmlH3('Crime Type'),
                        dccDropdown(
                            id = 'crime-chart',
                            options = lapply(list_of_crimes$TYPE, function(x){list(label=x, value=x)}),
                            value = 'ALL',
                            placeholder = 'ALL',
                            style = list(width='80%', verticalAlign="middle")
                            ),
                        htmlH3('Years to Include'),
                        htmlDiv(
                            list(
                                dccRangeSlider(
                                    id = 'year-slider',
                                    min = min_year,
                                    max = max_year,
                                    step = 1,
                                    marks = yearMarks,
                                    value = list(min_year, max_year)
                                    )
                                ),
                            style = list(width = '60%', marginLeft = '20px')
                        )
                    ),
                    style = list(height = '200px', marginLeft = '20px')
                ),
                htmlDiv(
                    list(
                        htmlDiv(
                            list(
                                graph_choropleth
                            ),
                            style = list(width = '80%', height = '500px', float = 'left')
                        ),
                        htmlDiv(
                            list(
                                htmlH4('Crime Index Max'),
                                dccSlider(
                                    id = 'slider-updatemode',
                                    marks = c("0.1", "1"),
                                    min = 0.1,
                                    max = 1,
                                    step = 0.01,
                                    updatemode = 'drag',
                                    vertical = TRUE,
                                    value = 1
                                )
                            ),
                            style = list(height = '200px', float = 'left', marginLeft = '30px', marginTop = '10px')
                        )
                    )
                ),
                htmlDiv(
                    list(
                        htmlP('Crime Index shows how the crime count of a neighbourhood compares to other neighbourhoods.'),
                        htmlP('- If it equals 1, then this neighbourhood has the highest crime count among all neighbourhoods.'),
                        htmlP('- If it equals 0, then this neighbourhood has the lowest crime count among all neighbourhoods'),
                        htmlP('Adjusting the \"Crime Index Max\" bar can saturate neighbourhoods with higher crime counts and reveal differences in lower crime count neighbourhoods'),
                        htmlP('Note that this function is slow and may take a few seconds to load')
                    ),
                    style = list(width = '900px')
                )
            ),
            style = list(float = 'left', width = '60%', height = '900px', marginTop = '83px', backgroundColor = '#e0e0eb', border = '3px solid black')
        ),

        # Crime Trends
        htmlDiv(
            list(
                htmlDiv(
                    list(
                        htmlH3('Neighbourhood 1', style = list(color = 'blue')),
                        dccDropdown(
                            id = 'dd-chart',
                            options = lapply(list_of_locations$NEIGHBOURHOOD, function(x){list(label=x, value=x)}),
                            value = 'ALL',
                            placeholder = 'ALL',
                            style = list(width = '90%', verticalAlign = "middle")
                        ),
                        htmlH3('Neighbourhood 2', style = list(color = 'red')),
                        dccDropdown(
                            id = 'dd-chart-2',
                            options = lapply(list_of_locations$NEIGHBOURHOOD, function(x){list(label=x, value=x)}),
                            value = 'ALL',
                            placeholder = 'ALL',
                            style = list(width = '90%', verticalAlign = "middle")
                        ),
                        htmlH3('Time Scale'),
                        dccDropdown(
                            id = 'year-chart',
                            options = lapply(list_of_years, function(x){list(label=x, value=x)}),
                            value = 'YEAR',
                            style = list(width = '90%', verticalAlign = "middle")
                        )
                    ),
                    style = list(height = '300px', marginLeft = '20px')
                ),
                htmlDiv(
                    list(
                        graph_line
                    ),
                    style = list(width = '95%', height = '500px')
                )
            ),
            style = list(float  = 'left', width = '36.5%', height = '900px', marginTop = '83px', backgroundColor = '#e0e0eb', border = '3px solid black')
        )
    )
  )
)

app$callback(
  output=list(id = 'line_chart', property='figure'),
  params=list(input(id = 'year-slider', property='value'),
              input(id = 'dd-chart', property='value'),
              input(id = 'dd-chart-2', property='value'),
              input(id = 'crime-chart', property='value'),
              input(id = 'year-chart', property='value')),
  function(year_range, location, location2, types, year) {
    plot_func(start=year_range[1], end=year_range[2], neighbourhood_1=location, neighbourhood_2=location2, crime=types, time_scale=year)
  })

app$callback(
  output=list(id = 'choropleth', property='figure'),
  params=list(input(id = 'year-slider', property='value'),
              input(id = 'crime-chart', property='value'),
              input(id = 'slider-updatemode', property='value')),
  function(year_range, crime_type, crime_thresh) {
    plot_choropleth(year_init = year_range[1], year_end = year_range[2], crime_type = crime_type, crime_threshold = crime_thresh)
  })

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))
