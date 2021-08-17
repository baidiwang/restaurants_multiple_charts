library(readr)
library(ggplot2)
library(geofacet)
library(dplyr)
library(rts)
library(lubridate)
library(tidyr)
library(rvest)

data <- read_csv("restaurants_multiple_charts/data.csv")
# View(data)
data <- data[-c(52,53,54,55),]
data <- data[,-c(2,3)]
        

url = 'https://excelnotes.com/states-abbreviations-in-us/'

state_abb <-
  url %>%
  read_html() %>%
  html_nodes(css = 'table') %>%
  html_table()

state_abbr = state_abb[[1]][,3]

data <- cbind(state_abbr,data)

# total_across_month <- 
#   data.frame(colSums(data[1:50,-c(1,2)])) %>%
#   rename('total_number' = 'colSums.data.1.50...c.1..2...')
# 
# total_across_month$month = lubridate::my(row.names(total_across_month))
# 
# total_across_month %>%
#   ggplot(aes(x = month, y = total_number, group = 1)) +
#   geom_line(size = 1) +
#   labs(title = "Refugee Acceptance on the Decline",
#        subtitle = "Number of refugees accepted annually, 2002 - 2019",
#        x = element_blank(),
#        y = element_blank()) 

final_data <-
  data %>%
  pivot_longer(cols = colnames(data[,-c(1,2)]), names_to = 'month', values_to = 'number')

final_data %>%
  ggplot(aes(x = lubridate::my(month), y = number, group=1)) +
  geom_line(color = "black") +
  geom_rect(mapping=aes(xmin=lubridate::my('Feb-20'), xmax=lubridate::my('Mar-20'), ymin=0, ymax=2000000), fill = "#97BA58", alpha = .05) +
  geom_rect(mapping=aes(xmin=lubridate::my('Mar-20'), xmax=lubridate::my('Jun-21'), ymin=0, ymax=2000000), fill = "#FF9999", alpha = .05) +
  scale_x_continuous(breaks = c(lubridate::my('Feb-20'),lubridate::my('Jun-21'))) +
  scale_y_continuous(breaks = c(0,1459100)) +
  facet_geo(~ Abbreviation, grid = "us_state_grid2") +
  labs(title = "Eating and drinking place employment trends by state from Feb 2020 t0 June 2021",
       subtitle = "Job numbers of eating and drinking places decresed when the pandemic started and haven't bounced back yet",
       x = 'month',
       y = 'numbers') +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    strip.background = element_rect(color = "white")
  )

