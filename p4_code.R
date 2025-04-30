# Load Libraries
library(tidyverse)
library(ggplot2)
library(treemapify)
library(shiny)

# Create Color Palettes
ao <- c('#1592d1', 'aliceblue')
rg <- c('#005335', '#c76131')
wimbledon <- c('#583688', '#047444')
us_open <- c('#042c8c', '#dea739')

# Download top 50 GOATs (As of 12/18/2023), add a column for backhand type,
# split win-loss column, then read in the file
goat_list <- read.csv('GOATList.csv')

# Tidy data set for graphing
df3 <- goat_list %>%
  pivot_longer(cols=c('won', 'lost'),
               names_to='result',
               values_to='count')

df3$result <- ifelse(df3$result=='won', 'Wins', 'Losses')

# Create plot
ui2 <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 5,
                 fluidPage(
                   fluidRow(
                     column(12,
                            selectInput("col_pal", 
                                        label = "Color Palette", 
                                        choices = c('Australian Open'='ao', 'Roland Garros'='rg', 'Wimbledon'='wimbledon', 'US Open'='us_open'),
                                        selected = 'rg')
                     ),
                     column(12,
                            selectInput("player", 
                                        label = "Player Name", 
                                        choices = goat_list$name,
                                        selected = 'Novak Djokovic')
                     )
                   )
                 )
    ),
    mainPanel(width = 7,
              plotOutput("p4"),
              textOutput('caption')
    )
  )
)

server2 <- function(input, output) {
  output$p4 <- renderPlot({
    player_row <- filter(df3, name==input$player)
    
    ggplot(player_row, aes(area=count, fill=result)) +
      geom_treemap() + 
      geom_treemap_text(aes(label=paste(result, count, sep='\n')),
                        place='centre',
                        color=ifelse(input$col_pal=='ao', 'black', 'white')) +
      scale_fill_manual(values=get(input$col_pal)) +
      labs(x='',
           y='',
           title=paste('Career Win-Loss Record of', input$player, '(As of 12/18/23)')) +
      theme_void() + 
      theme(legend.position='none',
            plot.title=element_text(hjust=0.5))
  })
  output$caption <- renderText({
    player_row <- filter(df3, name==input$player)
    backhand_type <- player_row$backhand[1]
    paste(input$player, 'uses a', tolower(backhand_type), 'backhand.')
  })
}
shinyApp(ui = ui2, server = server2)