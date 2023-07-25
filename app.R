library(reactable)
library(tidyverse)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(randomForest)
library(ISLR)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

full <- bind_rows(train, test)

men <- full[full$Sex == "male", ] %>% 
  mutate(Age = ifelse(Age <1, 0, Age))

women <- full[full$Sex == "female", ]%>% 
  mutate(Age = ifelse(Age <1, 0, Age))
# 
# set.seed(69420)
# lost_woods <- randomForest(Survived ~ Sex, Embarked, Age, data = train, ntree = 50)
# plot(lost_woods)

model <- lm(data= train, Survived ~ Sex+Age)
summary(model)


logistic_model <- lm(Survived ~ Sex + Pclass + Age + Embarked + Fare + Cabin + SibSp + Parch, data = train)#, family = binomial)
summary(logistic_model)

male_prob <- predict(logistic_model, newdata = data.frame(Sex = "male"), type = "response")

specific_person <- data.frame(Sex = "male", Pclass = 3, Age = 30, Name = "Behr, Mr. Karl Howell")


#employees
Rylans_Mom <- data.frame(PassengerId = 665, Sex = "female", Pclass = 2, Age = 50, SibSp = 1,
                         Parch = 1, Fare = 12.98, Cabin = "B4", Embarked = "S")
Jon_Dunham <- data.frame(PassengerId = 666, Sex = "male", Pclass = 2, Age = 27, SibSp = 1,
                         Parch = 0, Fare = 12.98, Cabin = "B4", Embarked = "S")
Parker_LeFebvre <- data.frame(PassengerId = 664, Sex = "male", Pclass = 1, Age = 22, SibSp = 2,
                              Parch = 2, Fare = 248.4, Cabin = "A10", Embarked = "S")
Rylan_Murry <- data.frame(PassengerId = 663, Sex = "male", Pclass = 1, Age = 28, SibSp = -1,
                              Parch = -1, Fare = 1000000000, Cabin = "B4", Embarked = "S")
Luke_Wagoner <- data.frame(PassengerId = 662, Sex = "male", Pclass = 3, Age = 33, SibSp = 3,
                          Parch = 1, Fare = 10.77, Cabin = "D19", Embarked = "Q")
Parkers_Mom <- data.frame(PassengerId = 661, Sex = "female", Pclass = 1, Age = 55, SibSp = 0,
                          Parch = 3, Fare = 250, Cabin = "A16", Embarked = "S")

Rylans_mom_prob <- predict(logistic_model, newdata = Rylans_Mom, type = "response")
print(Rylans_mom_prob)
Jons_prob <- predict(logistic_model, newdata = Jon_Dunham, type = "response")
print(Jons_prob)
Parkers_prob <- predict(logistic_model, newdata = Parker_LeFebvre, type = "response")
print(Parkers_prob)
Rylans_prob <- predict(logistic_model, newdata = Rylan_Murry, type = "response")
print(Rylans_prob)
Lukes_prob <- predict(logistic_model, newdata = Luke_Wagoner, type = "response")
print(Lukes_prob)
Parkers_mom_prob <- predict(logistic_model, newdata = Parkers_Mom, type = "response")
print(Parkers_mom_prob)




filterable <- reactive ({
  full %>% 
    filter(Survived == input$Survived)
}) 
  


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
      menuItem("Women Analysis", tabName = "womens", icon = icon("person-dress")),
      radioButtons("Survived", "Survived:", choices = unique(full$Survived), selected = unique(full$Survived)[1])
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mens",
              fluidRow(column(width = 6, highchartOutput('class_man')),
                       column(width = 6, highchartOutput('age_man'))),
              fluidRow(column(width = 6, highchartOutput('cabin_man')),
                       column(width = 6, highchartOutput('')))),
      tabItem(tabName = "womens",
              fluidRow(column(width = 6, highchartOutput('class_woman')),
                       column(width = 6, highchartOutput('age_woman'))),
              fluidRow(column(width = 6, highchartOutput('cabin_woman')),
                       column(width = 6, highchartOutput('')))))
    )
  )

server <- function(input, output) {
  
  filterable <- reactive ({
    full %>% 
      filter(Survived == input$Survived)
  }) 
  
#men ####
  output$class_man <- renderHighchart({
    filterable() %>% 
      filter(Sex == 'male')%>%
      mutate(Age = ifelse(Age <1, 0, Age)) %>% 
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
    temp <-  filterable() %>% 
       filter(Sex == 'male')
    men_age_buckets <- cut(temp$Age, breaks = seq(0, max(temp$Age, na.rm = TRUE) + 1, by = 5))
    men_age_counts <- table(men_age_buckets)
    men_age_levels <- levels(men_age_buckets)
    men_age <- data.frame(Age = men_age_levels, People = men_age_counts)
    men_age %>% 
      hchart(type = "column", hcaes(x = Age, y = People.Freq)) %>% 
      hc_title(text = "Age Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.y}") %>% 
      hc_add_theme(hc_theme_smpl())
  })
  
  output$cabin_man <- renderHighchart({
    filterable() %>% 
      filter(Sex == 'male')%>%
      group_by(Cabin = substr(Cabin, 1, 1)) %>%
      summarise(count = n()) %>% 
      hchart(type="pie", hcaes(x = Cabin, y = count)) %>% 
      hc_title(text = "Cabin Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.y}") %>% 
      hc_add_theme(hc_theme_smpl())
  })

#women####
  output$class_woman <- renderHighchart({
    filterable() %>% 
      filter(Sex == 'female')%>% 
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
    temp <-  filterable() %>% 
      filter(Sex == 'female')
    women_age_buckets <- cut(temp$Age, breaks = seq(0, max(temp$Age, na.rm = TRUE) + 1, by = 5))
    women_age_counts <- table(women_age_buckets)
    women_age_levels <- levels(women_age_buckets)
    women_age <- data.frame(Age = women_age_levels, People = women_age_counts)
    women_age %>% 
      hchart(type = "column", hcaes(x = Age, y = People.Freq)) %>% 
      hc_title(text = "Age Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.y}") %>% 
      hc_add_theme(hc_theme_smpl())
  })
  
  output$cabin_woman <- renderHighchart({
    filterable() %>% 
      filter(Sex == 'female')%>% 
      group_by(Cabin = substr(Cabin, 1, 1)) %>%
      summarise(count = n()) %>% 
      hchart(type="pie", hcaes(x = Cabin, y = count)) %>% 
      hc_title(text = "Cabin Distribution") %>%
      hc_tooltip(pointFormat = "Passengers: {point.y}") %>% 
      hc_add_theme(hc_theme_smpl())
  })
}

shinyApp(ui = ui, server = server)
