library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)

df <- read_csv('data/crimedata_csv_all_years_modified.csv')

list_of_locations <- df['NEIGHBOURHOOD'] %>% drop_na() %>% distinct() %>% add_row('NEIGHBOURHOOD' = 'ALL')
list_of_locations_dropdown <- list()
for (i in list_of_locations[['NEIGHBOURHOOD']]) {
    list_of_locations_dropdown <- list_of_locations_dropdown %>% append(list(label = i, value = i))
    }
list_of_crimes = df['TYPE'] %>% distinct() %>% add_row('TYPE' = 'ALL')
list_of_years = list('YEAR', 'MONTH', 'DAY OF WEEK', 'HOUR')

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
            list()
        ),

        # Crime Trends
        htmlDiv(
            list(
                htmlDiv(
                    list(
                        htmlH3('Neighbourhood 1', style = list(color = 'blue')),
                        dccDropdown(
                            id = 'dd-chart',
                            options = list_of_locations_dropdown,
                            value = 'ALL',
                            placeholder = 'ALL',
                            style = list(width = '90%', verticalAlign = "middle")
                        )

                        

                    )
                )
            )
        )
    )
  )
)

app$run_server()