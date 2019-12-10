library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)

df <- read_csv('data/crimedata_csv_all_years_modified.csv')

list_of_locations <- df['NEIGHBOURHOOD'] %>% drop_na() %>% distinct() %>% add_row('NEIGHBOURHOOD' = 'ALL')
list_of_crimes = df['TYPE'] %>% distinct() %>% add_row('TYPE' = 'ALL')
list_of_years = list('YEAR', 'MONTH', 'DAY OF WEEK', 'HOUR')
min_year = df['YEAR'] %>% min()
max_year = df['YEAR'] %>% max()
yearMarks <- lapply(unique(df$YEAR), as.character)
names(yearMarks) <- unique(unique(df$YEAR))

app <- Dash$new()

app$layout(
  htmlDiv(
      list(
        
        # Header
        htmlDiv(
          list(
              htmlImg(src='https://img.icons8.com/wired/64/000000/policeman-male.png'),
              htmlH1('Vancouver Crime Stats'),
              htmlDiv(children="Open source crime data from the City of Vancouver shown for neighbourhood comparison")
              )
            
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
                                )
                        )
                    )
                ),
                htmlDiv(
                    list(
                        htmlIframe(

                            # Choroplepth HERE, might be able to change to graph object instead of Iframe

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
                                    vertical = FALSE,
                                    value = 1
                                )
                            )
                        )
                    )
                ),
                htmlDiv(
                    list(
                        htmlP('Crime Index shows how the crime count of a neighborhood compares to other neighborhoods.'),
                        htmlP('- If it equals 1, then this neighborhood has the highest crime count among all neighborhoods.'),
                        htmlP('- If it equals 0, then this neighborhood has the lowest crime count among all neighborhoods'),
                        htmlP('Adjusting the \"Crime Index Max\" bar can saturate neighborhoods with higher crime counts and reveal differences in lower crime count neighbourhoods'),
                        htmlP('Note that this function is slow and may take a few seconds to load')
                    )
                )
            )
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

                        # Crime Trend Graph Object HERE

                    )
                )
            )
        )
    )
  )
)

app$run_server()