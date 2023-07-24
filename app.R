library(reactable)
library(tidyverse)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

full <- bind_rows(train, test)

men <- full[full$Sex == "male", ] %>% 
  mutate(Age = ifelse(Age <1, 0, Age))

women <- full[full$Sex == "female", ]

#brainstormed ideas:
# I want two tabs, male and female. Main filter being whether they survived or not
# Male tab: 4 charts. class, age, cabin, embarked
# Female tab: 4 charts. class, age, cabin, embarked
# tab 3: ML tab covering what had the greatest impact on survival/class/fare/age

ui <- dashboardPage(
  dashboardHeader(title = "Tinternic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Men Analysis", tabName = "mens", icon = icon("person")),
      menuItem("Women Analysis", tabName = "womens", icon = icon("person-dress"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mens",
              fluidRow(column(width = 6, highchartOutput('class_man')),
                       column(width = 6, highchartOutput('age_man'))),
              fluidRow(column(width = 6, highchartOutput('')),
                       column(width = 6, highchartOutput('')))),
      tabItem(tabName = "womens",
              fluidRow(column(width = 6, highchartOutput('class_woman')),
                       column(width = 6, highchartOutput('age_woman'))),
              fluidRow(column(width = 6, highchartOutput('')),
                       column(width = 6, highchartOutput('')))))
    )
  )

server <- function(input, output) {
  output$class_man <- renderHighchart({
    men %>% 
      mutate(Pclass = case_when(
        Pclass == 1 ~ "First class",
        Pclass == 2 ~ "Second class",
        Pclass == 3 ~ "Third class")) %>% 
      count(Pclass) %>% 
      arrange(n) %>% 
      hchart(type = "treemap", hcaes(x = Pclass, value = n, color = n)) %>% 
      hc_title(text = "Class Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.value}") %>% 
      hc_add_theme(hc_theme_smpl())
  })
  
  output$age_man <- renderHighchart({
    men_age_buckets <- cut(men$Age, breaks = seq(0, max(men$Age, na.rm = TRUE) + 1, by = 5))
    men_age_counts <- table(men_age_buckets)
    men_age_levels <- levels(men_age_buckets)
    men_age <- data.frame(Age = men_age_levels, People = men_age_counts)
    men_age %>% 
      hchart(type = "column", hcaes(x = Age, y = People.Freq)) %>% 
      hc_title(text = "Age Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.y}") %>% 
      hc_add_theme(hc_theme_smpl())
  })
  
  output$class_woman <- renderHighchart({
    women %>% 
      mutate(Pclass = case_when(
        Pclass == 1 ~ "First class",
        Pclass == 2 ~ "Second class",
        Pclass == 3 ~ "Third class")) %>% 
      count(Pclass) %>% 
      arrange(n) %>% 
      hchart(type = "treemap", hcaes(x = Pclass, value = n, color = n)) %>% 
      hc_title(text = "Class Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.value}") %>% 
      hc_add_theme(hc_theme_smpl())
  })
  
  output$age_woman <- renderHighchart({
    women_age_buckets <- cut(women$Age, breaks = seq(0, max(women$Age, na.rm = TRUE) + 1, by = 5))
    women_age_counts <- table(women_age_buckets)
    women_age_levels <- levels(women_age_buckets)
    women_age <- data.frame(Age = women_age_levels, People = women_age_counts)
    women_age %>% 
      hchart(type = "column", hcaes(x = Age, y = People.Freq)) %>% 
      hc_title(text = "Age Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.y}") %>% 
      hc_add_theme(hc_theme_smpl())
    
  })
}

shinyApp(ui = ui, server = server)
