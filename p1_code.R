# Load Libraries
library(tidyverse)
library(ggplot2)
library(shiny)

# Create Color Palettes
ao <- c('#1592d1', 'aliceblue')
rg <- c('#005335', '#c76131')
wimbledon <- c('#583688', '#047444')
us_open <- c('#042c8c', '#dea739')

# Read in rankings using player IDs
# rank_70 <- read.csv('atp_rankings_70s.csv')
# rank_80 <- read.csv('atp_rankings_80s.csv')
# rank_90 <- read.csv('atp_rankings_90s.csv')
# rank_00 <- read.csv('atp_rankings_00s.csv')
# rank_10 <- read.csv('atp_rankings_10s.csv')
# rank_20 <- read.csv('atp_rankings_20s.csv')
# rank_all <- rbind(rank_70, rank_80, rank_90, rank_00, rank_10, rank_20)

# Find end-of-year top 10 using player IDs
# eoy_dates <- rank_all %>%
#   mutate(year=as.numeric(substr(ranking_date, 1, 4)),
#          date=as.numeric(substr(ranking_date, 5, 8))) %>%
#   group_by(year) %>%
#   mutate(eoy_date = (year*10000) + max(date))
# 
# eoy_top10_id <- eoy_dates %>%
#   filter(ranking_date==eoy_date & rank<11) %>%
#   select(rank, player, year)

# Create end-of-year top 10 using players' full names
# eoy_top10 <- eoy_top10_id %>%
#   left_join(players, by=c('player'='player_id')) %>%
#   select(rank, name, year)

# Write data set
# write.csv(eoy_top10, 'eoy_top10_2.csv', row.names=FALSE)

# Add a column with players' type of backhand based on personal
# knowledge and references above

# Read in the new data set
eoy_top10_2 <- read.csv('eoy_top10_2.csv')

# Tidying data for graphing
eoy_top10_3 <- eoy_top10_2 %>% 
  group_by(year, backhand) %>%
  count() %>% 
  mutate(pos = n/2 + lead(n, 1),
         pos = if_else(is.na(pos), n/2, pos))

# Create plot
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 5,
                 fluidPage(
                   fluidRow(
                     column(12,
                            selectInput("col_pal", 
                                        label = "Color Palette", 
                                        choices = c('Australian Open'='ao', 'Roland Garros'='rg', 'Wimbledon'='wimbledon', 'US Open'='us_open'),
                                        selected = 'ao')
                     ),
                     column(12,
                            sliderInput("year", 
                                        label = "Year", 
                                        min = 1973,
                                        max = 2023,
                                        value = 1973,
                                        step = 1,
                                        sep = "")
                     )
                   )
                 )
    ),
    mainPanel(width = 7,
              plotOutput("p1")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    subset(eoy_top10_3, year==input$year)
  })
  output$p1 <- renderPlot({
    ggplot(filtered_data(), aes(x='', y=n, fill=backhand)) +
      geom_bar(stat='identity') +
      coord_polar('y', start=0) +
      geom_text(aes(label=paste(backhand, n, sep='\n')),
                position=position_stack(vjust=0.5),
                color=ifelse(input$col_pal=='ao', 'black', 'white')) +
      scale_fill_manual(values=get(input$col_pal)) +
      labs(x='',
           y='',
           title=paste('Split of Backhand Types in the Top 10 at the End of', input$year)) +
      theme_void() +
      theme(legend.position='none',
            plot.title=element_text(hjust=0.5))
  })
}
shinyApp(ui = ui, server = server)