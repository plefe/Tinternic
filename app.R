library(reactable)
library(tidyverse)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

full <- bind_rows(train, test)

men <- full[full$Sex == "male", ]
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
                       column(width = 6, highchartOutput(''))),
              fluidRow(column(width = 6, highchartOutput('')),
                       column(width = 6, highchartOutput('')))),
      tabItem(tabName = "womens",
              fluidRow(column(width = 6, highchartOutput('class_woman')),
                       column(width = 6, highchartOutput(''))),
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
}

shinyApp(ui = ui, server = server)



