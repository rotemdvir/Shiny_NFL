### NFL Shiny project
### March 2021
### Main App file

library(shiny)
library(shinythemes)
library(tidyverse)
library(kableExtra)

### App code
mydata <- read.csv("dataFull.csv")

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("NFL Advanced & Classic Metrics: My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select the season you wish to view."),
      selectInput("year", "Select Season:", c("", "2016", "2017", "2018", "2019", "2020")),
      
      helpText("Now, select your preferred metric from the list below."),
      selectInput("data", "Select Metric:",
                  list('Advanced' = list("", "QBR", "ANY_A", "EPA"),
                        'Classic' = list("", "TD", "Comp%", "QB Rating", "Yards/game"))),
      
      
      tags$br(),
      h4("Advanced Metrics glossary (external sites):"),
      uiOutput("qbr"),
      uiOutput("anya"),
      uiOutput("epa"),
      
      tags$br(),
      h4("Data Sources (external sites):"),
      uiOutput("epa_espn"),
      uiOutput("other_pfr")
      
  ),
  mainPanel(
    tableOutput("tab"),
    plotOutput("plot")  
  )
)
)

### Full function for results   ###
server <- function(input, output) {

  output$tab <- function(){

## Season 2016: Tables
    if(input$year == 2016){
      data2016 <- mydata %>%
        filter(season == 2016)
      
    if (input$data == "QBR") {
      dat_tab <- data2016 %>%
        filter(QBR_rank < 16) %>%
        select(First, Last, QBR)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("QBR: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
    } else
     if (input$data == "EPA") {
        dat_tab <- data2016 %>%
          filter(EPA_rank < 16) %>%
          select(First, Last, EPA_play) %>%
          arrange(-EPA_play)
        
        dat_tab %>%
          knitr::kable("html") %>%
          kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
          add_header_above(c("EPA per Play: Top 15" = 3)) %>%
          scroll_box(height = "250px", width = "450px")
     } else
       if (input$data == "ANY_A") {
         dat_tab <- data2016 %>%
           filter(ANYA_rank < 16) %>%
           select(First, Last, ANY_A) %>%
           arrange(-ANY_A)
         
         dat_tab %>%
           knitr::kable("html") %>%
           kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
           add_header_above(c("Adj. Net Yards per Attempt: Top 15" = 3)) %>%
           scroll_box(height = "250px", width = "450px")
       } else
         if (input$data == "TD") {
           dat_tab <- data2016 %>%
             filter(TD_rank < 16) %>%
             select(First, Last, TD) %>%
             arrange(-TD)
           
           dat_tab %>%
             knitr::kable("html") %>%
             kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
             add_header_above(c("Total TDs: Top 15" = 3)) %>%
             scroll_box(height = "250px", width = "450px")
      } else
         if (input$data == "Comp%") {
             dat_tab <- data2016 %>%
               filter(Comp_rank < 16) %>%
               select(First, Last, CompPercent) %>%
               arrange(-CompPercent)
             
             dat_tab %>%
               knitr::kable("html") %>%
               kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
               add_header_above(c("Completion Percents: Top 15" = 3)) %>%
               scroll_box(height = "250px", width = "450px")
     } else
       if (input$data == "Yards/game") {
               dat_tab <- data2016 %>%
                 filter(Yards_rank < 16) %>%
                 select(First, Last, YardsGame) %>%
                 arrange(-YardsGame)
               
               dat_tab %>%
                 knitr::kable("html") %>%
                 kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
                 add_header_above(c("Avg. Yards per Game: Top 15" = 3)) %>%
                 scroll_box(height = "250px", width = "450px")
      } else
       if (input$data == "QB Rating") {
          dat_tab <- data2016 %>%
            filter(Rate_rank < 16) %>%
            select(First, Last, Rate) %>%
            arrange(-Rate)
          
          dat_tab %>%
            knitr::kable("html") %>%
            kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
            add_header_above(c("QB Rating: Top 15" = 3)) %>%
            scroll_box(height = "250px", width = "450px")
        }
      
##  Season 2017: Tables        
    } else
    if (input$year == 2017){
      data2017 <- mydata %>%
        filter(season == 2017)
      
      if (input$data == "QBR") {
        dat_tab <- data2017 %>%
          filter(QBR_rank < 16) %>%
          select(First, Last, QBR)
        
        dat_tab %>%
          knitr::kable("html") %>%
          kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
          add_header_above(c("QBR: Top 15" = 3)) %>%
          scroll_box(height = "250px", width = "450px")
      } else
        if (input$data == "EPA") {
          dat_tab <- data2017 %>%
            filter(EPA_rank < 16) %>%
            select(First, Last, EPA_play) %>%
            arrange(-EPA_play)
          
          dat_tab %>%
            knitr::kable("html") %>%
            kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
            add_header_above(c("EPA per Play: Top 15" = 3)) %>%
            scroll_box(height = "250px", width = "450px")
        } else
          if (input$data == "ANY_A") {
            dat_tab <- data2017 %>%
              filter(ANYA_rank < 16) %>%
              select(First, Last, ANY_A) %>%
              arrange(-ANY_A)
            
            dat_tab %>%
              knitr::kable("html") %>%
              kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
              add_header_above(c("Adj. Net Yards per Attempt: Top 15" = 3)) %>%
              scroll_box(height = "250px", width = "450px")
        } else
          if (input$data == "TD") {
            dat_tab <- data2017 %>%
              filter(TD_rank < 16) %>%
              select(First, Last, TD) %>%
              arrange(-TD)
            
            dat_tab %>%
              knitr::kable("html") %>%
              kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
              add_header_above(c("total TDs.: Top 15" = 3)) %>%
              scroll_box(height = "250px", width = "450px")
        } else
          if (input$data == "Comp%") {
            dat_tab <- data2017 %>%
              filter(Comp_rank < 16) %>%
              select(First, Last, CompPercent) %>%
              arrange(-CompPercent)
            
            dat_tab %>%
              knitr::kable("html") %>%
              kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
              add_header_above(c("Completion %: Top 15" = 3)) %>%
              scroll_box(height = "250px", width = "450px")
        } else
          if (input$data == "QB Rating") {
            dat_tab <- data2017 %>%
              filter(Rate_rank < 16) %>%
              select(First, Last, Rate) %>%
              arrange(-Rate)
            
            dat_tab %>%
              knitr::kable("html") %>%
              kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
              add_header_above(c("QB Rating: Top 15" = 3)) %>%
              scroll_box(height = "250px", width = "450px")
        } else
          if (input$data == "Yards/game") {
            dat_tab <- data2017 %>%
              filter(Yards_rank < 16) %>%
              select(First, Last, YardsGame) %>%
              arrange(-YardsGame)
            
            dat_tab %>%
              knitr::kable("html") %>%
              kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
              add_header_above(c("Avg. Yards per Game: Top 15" = 3)) %>%
              scroll_box(height = "250px", width = "450px")
          }
      
## Season 2018: Tables
    } else
    if (input$year == 2018){
      data2018 <- mydata %>%
        filter(season == 2018)
      
      if (input$data == "QBR") {
        dat_tab <- data2018 %>%
          filter(QBR_rank < 16) %>%
          select(First, Last, QBR)
        
        dat_tab %>%
          knitr::kable("html") %>%
          kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
          add_header_above(c("QBR: Top 15" = 3)) %>%
          scroll_box(height = "250px", width = "450px")
      } else
        if (input$data == "EPA") {
          dat_tab <- data2018 %>%
            filter(EPA_rank < 16) %>%
            select(First, Last, EPA_play) %>%
            arrange(-EPA_play)
          
          dat_tab %>%
            knitr::kable("html") %>%
            kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
            add_header_above(c("EPA per Play: Top 15" = 3)) %>%
            scroll_box(height = "250px", width = "450px")
      } else
        if (input$data == "ANY_A") {
          dat_tab <- data2018 %>%
            filter(ANYA_rank < 16) %>%
            select(First, Last, ANY_A) %>%
            arrange(-ANY_A)
          
          dat_tab %>%
            knitr::kable("html") %>%
            kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
            add_header_above(c("Adj. Net Yards per Attempt: Top 15" = 3)) %>%
            scroll_box(height = "250px", width = "450px")
      } else
        if (input$data == "TD") {
          dat_tab <- data2018 %>%
            filter(TD_rank < 16) %>%
            select(First, Last, TD) %>%
            arrange(-TD)
          
          dat_tab %>%
            knitr::kable("html") %>%
            kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
            add_header_above(c("Total TDs.: Top 15" = 3)) %>%
            scroll_box(height = "250px", width = "450px")
      } else
        if (input$data == "Comp%") {
          dat_tab <- data2018 %>%
            filter(Comp_rank < 16) %>%
            select(First, Last, CompPercent) %>%
            arrange(-CompPercent)
          
          dat_tab %>%
            knitr::kable("html") %>%
            kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
            add_header_above(c("Completion %: Top 15" = 3)) %>%
            scroll_box(height = "250px", width = "450px")
     } else
       if (input$data == "QB Rating") {
         dat_tab <- data2018 %>%
           filter(Rate_rank < 16) %>%
           select(First, Last, Rate) %>%
           arrange(-Rate)
         
         dat_tab %>%
           knitr::kable("html") %>%
           kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
           add_header_above(c("QB Rating: Top 15" = 3)) %>%
           scroll_box(height = "250px", width = "450px")
    } else
      if (input$data == "Yards/game") {
        dat_tab <- data2018 %>%
          filter(Yards_rank < 16) %>%
          select(First, Last, YardsGame) %>%
          arrange(-YardsGame)
        
        dat_tab %>%
          knitr::kable("html") %>%
          kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
          add_header_above(c("Avg. Yards per Game: Top 15" = 3)) %>%
          scroll_box(height = "250px", width = "450px")
      }
        
## Season 2019: Tables
  } else
  if (input$year == 2019){
    data2019 <- mydata %>%
      filter(season == 2019)
    
    if (input$data == "QBR") {
      dat_tab <- data2019 %>%
        filter(QBR_rank < 16) %>%
        select(First, Last, QBR)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("QBR: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
    } else
      if (input$data == "EPA") {
        dat_tab <- data2019 %>%
          filter(EPA_rank < 16) %>%
          select(First, Last, EPA_play) %>%
          arrange(-EPA_play)
        
        dat_tab %>%
          knitr::kable("html") %>%
          kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
          add_header_above(c("EPA per Play: Top 15" = 3)) %>%
          scroll_box(height = "250px", width = "450px")
   } else
     if (input$data == "ANY_A") {
       dat_tab <- data2019 %>%
         filter(ANYA_rank < 16) %>%
         select(First, Last, ANY_A) %>%
         arrange(-ANY_A)
       
       dat_tab %>%
         knitr::kable("html") %>%
         kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
         add_header_above(c("Adj. Net Yards per Attempt: Top 15" = 3)) %>%
         scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "TD") {
      dat_tab <- data2019 %>%
        filter(TD_rank < 16) %>%
        select(First, Last, TD) %>%
        arrange(-TD)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Total TDs.: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "Comp%") {
      dat_tab <- data2019 %>%
        filter(Comp_rank < 16) %>%
        select(First, Last, CompPercent) %>%
        arrange(-CompPercent)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Completion %: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "QB Rating") {
      dat_tab <- data2019 %>%
        filter(Rate_rank < 16) %>%
        select(First, Last, Rate) %>%
        arrange(-Rate)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("QB Rating: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "Yards/game") {
      dat_tab <- data2019 %>%
        filter(Yards_rank < 16) %>%
        select(First, Last, YardsGame) %>%
        arrange(-YardsGame)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Avg. Yards per Game: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
    }
    
## Season 2020: Tables
  } else
  if (input$year == 2020){
    data2020 <- mydata %>%
      filter(season == 2020)
    
    if (input$data == "QBR") {
      dat_tab <- data2020 %>%
        filter(QBR_rank < 16) %>%
        select(First, Last, QBR)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("QBR: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "EPA") {
      dat_tab <- data2020 %>%
        filter(EPA_rank < 16) %>%
        select(First, Last, EPA_play) %>%
        arrange(-EPA_play)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("EPA per Play: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "ANY_A") {
      dat_tab <- data2020 %>%
        filter(ANYA_rank < 16) %>%
        select(First, Last, ANY_A) %>%
        arrange(-ANY_A)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Adj. Net Yards per Attempt: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "TD") {
      dat_tab <- data2020 %>%
        filter(TD_rank < 16) %>%
        select(First, Last, TD) %>%
        arrange(-TD)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Total TDs.: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "Comp%") {
      dat_tab <- data2020 %>%
        filter(Comp_rank < 16) %>%
        select(First, Last, CompPercent) %>%
        arrange(-CompPercent)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Completion %: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "QB Rating") {
      dat_tab <- data2020 %>%
        filter(Rate_rank < 16) %>%
        select(First, Last, Rate) %>%
        arrange(-Rate)
      
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("QB Rating: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
  } else
    if (input$data == "Yards/game") {
      dat_tab <- data2020 %>%
        filter(Yards_rank < 16) %>%
        select(First, Last, YardsGame) %>%
        arrange(-YardsGame)
        
      dat_tab %>%
        knitr::kable("html") %>%
        kable_styling(font_size = 15, "striped", full_width = F, position = "center") %>%
        add_header_above(c("Avg. Yards per Game: Top 15" = 3)) %>%
        scroll_box(height = "250px", width = "450px")
    }
  }
}
  
#### Create plots ####
######################
  output$plot <- renderPlot({
    if(input$year == 2016){
      data2016 <- mydata %>%
        filter(season == 2016)
      
    if (input$data == "QBR") {
      dat <- data2016 %>%
        filter(QBR_rank < 11) %>%
        select(First, Last, QBR, QBR_rank)
      
      plot_qbr16 <- ggplot(dat, aes(x = QBR_rank, y = QBR)) +
        geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
        geom_text(aes(label = Last, vjust = -1)) +
        geom_text(aes(label = QBR), vjust = 1.5, color = "white") +
        xlab("") + ylab("Season QBR") + ggtitle("QBR: Top-10") + ylim(0,85) +
        theme_bw()  
      plot_qbr16 <- plot_qbr16 + theme(axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank())
      plot_qbr16
      
    } else
    if (input$data == "EPA") {
      dat <- data2016 %>%
        filter(EPA_rank < 11) %>%
        select(First, Last, EPA_play, EPA_rank)
      
      plot_epa16 <- ggplot(dat, aes(x = EPA_rank, y = EPA_play)) +
        geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
        geom_text(aes(label = Last, vjust = -1)) +
        geom_text(aes(label = EPA_play), vjust = 1.5, color = "white") +
        xlab("") + ylab("Season EPA per Play") + ggtitle("EPA/play: Top-10") + ylim(0,0.2) + 
        theme_bw() 
      plot_epa16 <- plot_epa16 + theme(axis.text.x = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks = element_blank())
      plot_epa16
      
    } else 
    if (input$data == "ANY_A") {
      dat <- data2016 %>%
        filter(ANYA_rank < 11) %>%
        select(First, Last, ANY_A, ANYA_rank)
      
      plot_any16 <- ggplot(dat, aes(x = ANYA_rank, y = ANY_A)) +
        geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
        geom_text(aes(label = Last, vjust = -1)) +
        geom_text(aes(label = ANY_A), vjust = 1.5, color = "white") +
        xlab("") + ylab("Season ANY_A") + ggtitle("QBs Adj. Net Yards per Attempt: Top-10") + ylim(0,9.5) +
        theme_bw() 
      plot_any16 <- plot_any16 + theme(axis.text.x = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks = element_blank())
      plot_any16
      
    } else
      if (input$data == "TD") {
        dat <- data2016 %>%
          filter(TD_rank < 11) %>%
          select(First, Last, TD, TD_rank) 
        
        plot_td16 <- ggplot(dat, aes(x = TD_rank, y = TD)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = TD), vjust = 1.5, color = "white") +
          xlab("") + ylab("Total TDs") + ggtitle("Total TDs: Top-10") + ylim(0,45) +
          theme_bw() 
        plot_td16 <- plot_td16 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
        plot_td16
        
      } else
      if (input$data == "Comp%") {
        dat <- data2016 %>%
          filter(Comp_rank < 11) %>%
          select(First, Last, CompPercent, Comp_rank) 
        
          plot_comp16 <- ggplot(dat, aes(x = Comp_rank, y = CompPercent)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = CompPercent), vjust = 1.5, color = "white") +
          xlab("") + ylab("Completion Percentage") + ggtitle("Completion %: Top-10") + ylim(0,80) +
          theme_bw() 
        plot_comp16 <- plot_comp16 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_comp16
        
      } else
       if (input$data == "QB Rating"){
          dat <- data2016 %>%
            filter(Rate_rank < 11) %>%
            select(First, Last, Rate, Rate_rank) 
          
          plot_rating16 <- ggplot(dat, aes(x = Rate_rank, y = Rate)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = Rate), vjust = 1.5, color = "white") +
            xlab("") + ylab("QB Rating") + ggtitle("QB Rating: Top-10") + ylim(0,120) +
            theme_bw() 
          plot_rating16 <- plot_rating16 + theme(axis.text.x = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks = element_blank())
          plot_rating16
       
      } else
        if (input$data == "Yards/game"){
          dat <- data2016 %>%
            filter(Yards_rank < 11) %>%
            select(First, Last, YardsGame, Yards_rank) 
          
          plot_yards16 <- ggplot(dat, aes(x = Yards_rank, y = YardsGame)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = YardsGame), vjust = 1.5, color = "white") +
            xlab("") + ylab("Yards/Game") + ggtitle("Avg. Yards per Game: Top-10") + ylim(0,330) +
            theme_bw() 
          plot_yards16 <- plot_yards16 + theme(axis.text.x = element_blank(),
                                                 axis.text.y = element_blank(),
                                                 axis.ticks = element_blank())
          plot_yards16
        }      
        
### Plots 2017 season
      
    } else
    if (input$year == 2017){
      data2017 <- mydata %>%
        filter(season == 2017)
      
      if (input$data == "QBR") {
        dat <- data2017 %>%
          filter(QBR_rank < 11) %>%
          select(First, Last, QBR, QBR_rank)
        
        plot_qbr17 <- ggplot(dat, aes(x = QBR_rank, y = QBR)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = QBR), vjust = 1.5, color = "white") +
          xlab("") + ylab("Season QBR") + ggtitle("QBR: Top-10") + ylim(0,85) +
          theme_bw()  
        plot_qbr17 <- plot_qbr17 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_qbr17
      } else
        if (input$data == "EPA") {
          dat <- data2017 %>%
            filter(EPA_rank < 11) %>%
            select(First, Last, EPA_play, EPA_rank)
          
          plot_epa17 <- ggplot(dat, aes(x = EPA_rank, y = EPA_play)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = EPA_play), vjust = 1.5, color = "white") +
            xlab("") + ylab("Season EPA per Play") + ggtitle("EPA/play: Top-10") + ylim(0,0.2) + 
            theme_bw() 
          plot_epa17 <- plot_epa17 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
          plot_epa17
        } else 
          if (input$data == "ANY_A") {
            dat <- data2017 %>%
              filter(ANYA_rank < 11) %>%
              select(First, Last, ANY_A, ANYA_rank)
            
            plot_any17 <- ggplot(dat, aes(x = ANYA_rank, y = ANY_A)) +
              geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
              geom_text(aes(label = Last, vjust = -1)) +
              geom_text(aes(label = ANY_A), vjust = 1.5, color = "white") +
              xlab("") + ylab("Season ANY_A") + ggtitle("QBs Adj. Net Yards per Attempt: Top-10") + ylim(0,9.5) +
              theme_bw() 
            plot_any17 <- plot_any17 + theme(axis.text.x = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks = element_blank())
            plot_any17
          } else
            if (input$data == "TD") {
              dat <- data2017 %>%
                filter(TD_rank < 11) %>%
                select(First, Last, TD, TD_rank)
              
              plot_td17 <- ggplot(dat, aes(x = TD_rank, y = TD)) +
                geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
                geom_text(aes(label = Last, vjust = -1)) +
                geom_text(aes(label = TD), vjust = 1.5, color = "white") +
                xlab("") + ylab("Total TDs") + ggtitle("Total TDs: Top-10") + ylim(0,40) +
                theme_bw() 
              plot_td17 <- plot_td17 + theme(axis.text.x = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks = element_blank())
              plot_td17
          } else
            if (input$data == "Comp%") {
              dat <- data2017 %>%
                filter(Comp_rank < 11) %>%
                select(First, Last, CompPercent, Comp_rank)
              
              plot_comp17 <- ggplot(dat, aes(x = Comp_rank, y = CompPercent)) +
                geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
                geom_text(aes(label = Last, vjust = -1)) +
                geom_text(aes(label = CompPercent), vjust = 1.5, color = "white") +
                xlab("") + ylab("Completion Percentage") + ggtitle("Completion %: Top-10") + ylim(0,80) +
                theme_bw() 
              plot_comp17 <- plot_comp17 + theme(axis.text.x = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks = element_blank())
              plot_comp17
          } else
            if (input$data == "QB Rating") {
              dat <- data2017 %>%
                filter(Rate_rank < 11) %>%
                select(First, Last, Rate, Rate_rank)
              
              plot_rating17 <- ggplot(dat, aes(x = Rate_rank, y = Rate)) +
                geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
                geom_text(aes(label = Last, vjust = -1)) +
                geom_text(aes(label = Rate), vjust = 1.5, color = "white") +
                xlab("") + ylab("QB Rating") + ggtitle("QB Rating: Top-10") + ylim(0,120) +
                theme_bw() 
              plot_rating17 <- plot_rating17 + theme(axis.text.x = element_blank(),
                                                 axis.text.y = element_blank(),
                                                 axis.ticks = element_blank())
              plot_rating17
          } else
            if (input$data == "Yards/game") {
              dat <- data2017 %>%
                filter(Yards_rank < 11) %>%
                select(First, Last, YardsGame, Yards_rank)
              
              plot_yards17 <- ggplot(dat, aes(x = Yards_rank, y = YardsGame)) +
                geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
                geom_text(aes(label = Last, vjust = -1)) +
                geom_text(aes(label = YardsGame), vjust = 1.5, color = "white") +
                xlab("") + ylab("Yards/Game") + ggtitle("Avg. Yards per Game") + ylim(0,330) +
                theme_bw() 
              plot_yards17 <- plot_yards17 + theme(axis.text.x = element_blank(),
                                                     axis.text.y = element_blank(),
                                                     axis.ticks = element_blank())
              plot_yards17
          } 

### Plots 2018 season
    } else
    if (input$year == 2018){
      data2018 <- mydata %>%
        filter(season == 2018)
      
      if (input$data == "QBR") {
        dat <- data2018 %>%
          filter(QBR_rank < 11) %>%
          select(First, Last, QBR, QBR_rank)
        
        plot_qbr18 <- ggplot(dat, aes(x = QBR_rank, y = QBR)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = QBR), vjust = 1.5, color = "white") +
          xlab("") + ylab("Season QBR") + ggtitle("QBR: Top-10") + ylim(0,85) +
          theme_bw()  
        plot_qbr18 <- plot_qbr18 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_qbr18
      } else
        if (input$data == "EPA") {
          dat <- data2018 %>%
            filter(EPA_rank < 11) %>%
            select(First, Last, EPA_play, EPA_rank)
          
          plot_epa18 <- ggplot(dat, aes(x = EPA_rank, y = EPA_play)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = EPA_play), vjust = 1.5, color = "white") +
            xlab("") + ylab("Season EPA per Play") + ggtitle("EPA/play: Top-10") + ylim(0,0.2) + 
            theme_bw() 
          plot_epa18 <- plot_epa18 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
          plot_epa18
      } else
        if (input$data == "ANY_A") {
          dat <- data2018 %>%
            filter(ANYA_rank < 11) %>%
            select(First, Last, ANY_A, ANYA_rank)
          
          plot_any18 <- ggplot(dat, aes(x = ANYA_rank, y = ANY_A)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = ANY_A), vjust = 1.5, color = "white") +
            xlab("") + ylab("Season ANY_A") + ggtitle("QBs Adj. Net Yards per Attempt: Top-10") + ylim(0,9.5) +
            theme_bw() 
          plot_any18 <- plot_any18 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
          plot_any18
      } else
        if (input$data == "TD") {
          dat <- data2018 %>%
            filter(TD_rank < 11) %>%
            select(First, Last, TD, TD_rank)
          
          plot_td18 <- ggplot(dat, aes(x = TD_rank, y = TD)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = TD), vjust = 1.5, color = "white") +
            xlab("") + ylab("Total TDs") + ggtitle("Total TDs: Top-10") + ylim(0,52) +
            theme_bw() 
          plot_td18 <- plot_td18 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
          plot_td18
      } else
        if (input$data == "Comp%") {
          dat <- data2018 %>%
            filter(Comp_rank < 11) %>%
            select(First, Last, CompPercent, Comp_rank)
          
          plot_comp18 <- ggplot(dat, aes(x = Comp_rank, y = CompPercent)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = CompPercent), vjust = 1.5, color = "white") +
            xlab("") + ylab("Completion Percentage") + ggtitle("Completion %: Top-10") + ylim(0,80) +
            theme_bw() 
          plot_comp18 <- plot_comp18 + theme(axis.text.x = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks = element_blank())
          plot_comp18
      } else
        if (input$data == "QB Rating") {
          dat <- data2018 %>%
            filter(Rate_rank < 11) %>%
            select(First, Last, Rate, Rate_rank)
          
          plot_rating18 <- ggplot(dat, aes(x = Rate_rank, y = Rate)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = Rate), vjust = 1.5, color = "white") +
            xlab("") + ylab("QB Rating") + ggtitle("QB Rating: Top-10") + ylim(0,120) +
            theme_bw() 
          plot_rating18 <- plot_rating18 + theme(axis.text.x = element_blank(),
                                                 axis.text.y = element_blank(),
                                                 axis.ticks = element_blank())
          plot_rating18
      } else
        if (input$data == "Yards/game") {
          dat <- data2018 %>%
            filter(Yards_rank < 11) %>%
            select(First, Last, YardsGame, Yards_rank)
          
          plot_yards18 <- ggplot(dat, aes(x = Yards_rank, y = YardsGame)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = YardsGame), vjust = 1.5, color = "white") +
            xlab("") + ylab("Yards/Game") + ggtitle("Avg. Yards per Game") + ylim(0,330) +
            theme_bw() 
          plot_yards18 <- plot_yards18 + theme(axis.text.x = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks = element_blank())
          plot_yards18
        }
      
### Plots 2019 season
  } else
    if (input$year == 2019){
      data2019 <- mydata %>%
        filter(season == 2019)
      
      if (input$data == "QBR") {
        dat <- data2019 %>%
          filter(QBR_rank < 11) %>%
          select(First, Last, QBR, QBR_rank)
        
        plot_qbr19 <- ggplot(dat, aes(x = QBR_rank, y = QBR)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = QBR), vjust = 1.5, color = "white") +
          xlab("") + ylab("Season QBR") + ggtitle("QBR: Top-10") + ylim(0,85) +
          theme_bw()  
        plot_qbr19 <- plot_qbr19 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_qbr19
      } else
        if (input$data == "EPA") {
          dat <- data2019 %>%
            filter(EPA_rank < 11) %>%
            select(First, Last, EPA_play, EPA_rank)
          
          plot_epa19 <- ggplot(dat, aes(x = EPA_rank, y = EPA_play)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = EPA_play), vjust = 1.5, color = "white") +
            xlab("") + ylab("Season EPA per Play") + ggtitle("EPA/play: Top-10") + ylim(0,0.2) + 
            theme_bw() 
          plot_epa19 <- plot_epa19 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
          plot_epa19
      } else
        if (input$data == "ANY_A") {
          dat <- data2019 %>%
            filter(ANYA_rank < 11) %>%
            select(First, Last, ANY_A, ANYA_rank)
          
          plot_any19 <- ggplot(dat, aes(x = ANYA_rank, y = ANY_A)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = ANY_A), vjust = 1.5, color = "white") +
            xlab("") + ylab("Season ANY_A") + ggtitle("QBs Adj. Net Yards per Attempt: Top-10") + ylim(0,9.5) +
            theme_bw() 
          plot_any19 <- plot_any19 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
          plot_any19
      } else
        if (input$data == "TD") {
          dat <- data2019 %>%
            filter(TD_rank < 11) %>%
            select(First, Last, TD, TD_rank)
          
          plot_td19 <- ggplot(dat, aes(x = TD_rank, y = TD)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = TD), vjust = 1.5, color = "white") +
            xlab("") + ylab("Total TDs") + ggtitle("Total TDs: Top-10") + ylim(0,40) +
            theme_bw() 
          plot_td19 <- plot_td19 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
          plot_td19
      } else
        if (input$data == "Comp%") {
          dat <- data2019 %>%
            filter(Comp_rank < 11) %>%
            select(First, Last, CompPercent, Comp_rank)
          
          plot_comp19 <- ggplot(dat, aes(x = Comp_rank, y = CompPercent)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = CompPercent), vjust = 1.5, color = "white") +
            xlab("") + ylab("Completion Percentage") + ggtitle("Completion %: Top-10") + ylim(0,80) +
            theme_bw() 
          plot_comp19 <- plot_comp19 + theme(axis.text.x = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks = element_blank())
          plot_comp19
      } else
        if (input$data == "QB Rating") {
          dat <- data2019 %>%
            filter(Rate_rank < 11) %>%
            select(First, Last, Rate, Rate_rank)
          
          plot_rating19 <- ggplot(dat, aes(x = Rate_rank, y = Rate)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = Rate), vjust = 1.5, color = "white") +
            xlab("") + ylab("QB Rating") + ggtitle("QB Rating: Top-10") + ylim(0,120) +
            theme_bw() 
          plot_rating19 <- plot_rating19 + theme(axis.text.x = element_blank(),
                                                 axis.text.y = element_blank(),
                                                 axis.ticks = element_blank())
          plot_rating19
      } else
        if (input$data == "Yards/game") {
          dat <- data2019 %>%
            filter(Yards_rank < 11) %>%
            select(First, Last, YardsGame, Yards_rank)
          
          plot_yards19 <- ggplot(dat, aes(x = Yards_rank, y = YardsGame)) +
            geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
            geom_text(aes(label = Last, vjust = -1)) +
            geom_text(aes(label = YardsGame), vjust = 1.5, color = "white") +
            xlab("") + ylab("Yards/Game") + ggtitle("Avg. Yards per Game") + ylim(0,330) +
            theme_bw() 
          plot_yards19 <- plot_yards19 + theme(axis.text.x = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks = element_blank())
          plot_yards19
        }
  
### Plots 2020 season  
  } else
    if (input$year == 2020){
      data2020 <- mydata %>%
        filter(season == 2020)
      
      if (input$data == "QBR") {
        dat <- data2020 %>%
          filter(QBR_rank < 11) %>%
          select(First, Last, QBR, QBR_rank)
        
        plot_qbr20 <- ggplot(dat, aes(x = QBR_rank, y = QBR)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = QBR), vjust = 1.5, color = "white") +
          xlab("") + ylab("Season QBR") + ggtitle("QBR: Top-10") + ylim(0,85) +
          theme_bw()  
        plot_qbr20 <- plot_qbr20 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_qbr20
    } else
      if (input$data == "EPA") {
        dat <- data2020 %>%
          filter(EPA_rank < 11) %>%
          select(First, Last, EPA_play, EPA_rank)
        
        plot_epa20 <- ggplot(dat, aes(x = EPA_rank, y = EPA_play)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = EPA_play), vjust = 1.5, color = "white") +
          xlab("") + ylab("Season EPA per Play") + ggtitle("EPA/play: Top-10") + ylim(0,0.2) + 
          theme_bw() 
        plot_epa20 <- plot_epa20 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_epa20
    } else
      if (input$data == "ANY_A") {
        dat <- data2020 %>%
          filter(ANYA_rank < 11) %>%
          select(First, Last, ANY_A, ANYA_rank)
        
        plot_any20 <- ggplot(dat, aes(x = ANYA_rank, y = ANY_A)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = ANY_A), vjust = 1.5, color = "white") +
          xlab("") + ylab("Season ANY_A") + ggtitle("QBs Adj. Net Yards per Attempt: Top-10") + ylim(0,9.5) +
          theme_bw() 
        plot_any20 <- plot_any20 + theme(axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks = element_blank())
        plot_any20
    } else
      if (input$data == "TD") {
        dat <- data2020 %>%
          filter(TD_rank < 11) %>%
          select(First, Last, TD, TD_rank)
        
        plot_td20 <- ggplot(dat, aes(x = TD_rank, y = TD)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = TD), vjust = 1.5, color = "white") +
          xlab("") + ylab("Total TDs") + ggtitle("Total TDs: Top-10") + ylim(0,50) +
          theme_bw() 
        plot_td20 <- plot_td20 + theme(axis.text.x = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks = element_blank())
        plot_td20
    } else
      if (input$data == "Comp%") {
        dat <- data2020 %>%
          filter(Comp_rank < 11) %>%
          select(First, Last, CompPercent, Comp_rank)
        
        plot_comp20 <- ggplot(dat, aes(x = Comp_rank, y = CompPercent)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = CompPercent), vjust = 1.5, color = "white") +
          xlab("") + ylab("Completion Percentage") + ggtitle("Completion %: Top-10") + ylim(0,80) +
          theme_bw() 
        plot_comp20 <- plot_comp20 + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks = element_blank())
        plot_comp20
    } else
      if (input$data == "QB Rating") {
        dat <- data2020 %>%
          filter(Rate_rank < 11) %>%
          select(First, Last, Rate, Rate_rank)
        
        plot_rating20 <- ggplot(dat, aes(x = Rate_rank, y = Rate)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = Rate), vjust = 1.5, color = "white") +
          xlab("") + ylab("QB Rating") + ggtitle("QB Rating: Top-10") + ylim(0,125) +
          theme_bw() 
        plot_rating20 <- plot_rating20 + theme(axis.text.x = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks = element_blank())
        plot_rating20
    } else
      if (input$data == "Yards/game") {
        dat <- data2020 %>%
          filter(Yards_rank < 11) %>%
          select(First, Last, YardsGame, Yards_rank)
        
        plot_yards20 <- ggplot(dat, aes(x = Yards_rank, y = YardsGame)) +
          geom_bar(stat = "identity", color = "navyblue", fill = "darkblue") +
          geom_text(aes(label = Last, vjust = -1)) +
          geom_text(aes(label = YardsGame), vjust = 1.5, color = "white") +
          xlab("") + ylab("Yards/Game") + ggtitle("Avg. Yards per Game") + ylim(0,330) +
          theme_bw() 
        plot_yards20 <- plot_yards20 + theme(axis.text.x = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks = element_blank())
        plot_yards20
      }
      
    }
    
  })

### Links to external sites
  url1 <- a("Link", href="https://www.espn.com/nfl/story/_/id/6833215/explaining-statistics-total-quarterback-rating",
            target = "_blank")
  output$qbr <- renderUI({
    tagList("What is QBR? (ESPN.com):", url1)
  })
  url2 <- a("Link", href="https://www.pro-football-reference.com/leaders/pass_adj_net_yds_per_att_career.htm",
            target = "_blank")
  output$anya <- renderUI({
    tagList("What is ANY_A? (pro-football-reference.com):", url2)
  })
  url3 <- a("Link", href="https://sportsinfosolutionsblog.com/2020/12/01/a-primer-on-total-points/",
            target = "_blank")
  output$epa <- renderUI({
    tagList("What is EPA & EPA/play? (sportsinfosolutionsblog.com):", url3)
  })
  
### Links to data sources
  url4 <- a("Link", href="https://www.espn.com/nfl/qbr", target = "_blank")
  output$epa_espn <- renderUI({
    tagList("EPA (ESPN.com/nfl/stats)", url4)
  })
  url5 <- a("Link", href="https://www.pro-football-reference.com/years/2020//passing.htm", target = "_blank")
  output$other_pfr <- renderUI({
    tagList("Other data (pro-football-reference.com)", url5)
  })
  

}
shinyApp(ui = ui, server = server)




