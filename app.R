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

full <- bind_rows(train, test) %>% 
  na.omit(Survived)

men <- full[full$Sex == "male", ] %>% 
  mutate(Age = ifelse(Age <1, 0, Age))

women <- full[full$Sex == "female", ] %>% 
  mutate(Age = ifelse(Age <1, 0, Age))

 logistic_model <- glm(Survived ~ Sex + Pclass + Age + Embarked + Fare + SibSp + Parch, data = train)#, family = binomial)

# employees ####
Rylans_Mom <- data.frame(PassengerId = 665, Sex = "female", Pclass = 2, Age = 50, SibSp = 1,
                         Parch = 1, Fare = 12.98, Cabin = "", Embarked = "S")
Jon_Dunham <- data.frame(PassengerId = 666, Sex = "male", Pclass = 2, Age = 27, SibSp = 1,
                         Parch = 0, Fare = 12.98, Cabin = "", Embarked = "S")
Parker_LeFebvre <- data.frame(PassengerId = 664, Sex = "male", Pclass = 1, Age = 22, SibSp = 2,
                              Parch = 2, Fare = 248.4, Cabin = "", Embarked = "S")
Rylan_Murry <- data.frame(PassengerId = 663, Sex = "male", Pclass = 1, Age = 28, SibSp = -1,
                              Parch = -1, Fare = 1000000000, Cabin = "", Embarked = "S")
Luke_Wagoner <- data.frame(PassengerId = 662, Sex = "male", Pclass = 3, Age = 33, SibSp = 3,
                          Parch = 1, Fare = 10.77, Cabin = "", Embarked = "Q")
Parkers_Mom <- data.frame(PassengerId = 661, Sex = "female", Pclass = 1, Age = 55, SibSp = 0,
                          Parch = 3, Fare = 250, Cabin = "", Embarked = "S")
Ophelia_Banks <- data.frame(PassengerId = 660, Sex = "female", Pclass = 1, Age = 73, SibSp = 0,
                            Parch = 0, Fare = 42.07, Cabin = "", Embarked = "C")
Rose_Bukater <- data.frame(PassengerId = 659, Sex = "female", Pclass = 1, Age = 31, SibSp = 0,
                           Parch = 2, Fare = 98.9, Cabin = "", Embarked = "C")
Jack_Dawson <- data.frame(PassengerId = 658, Sex = "male", Pclass = 3, Age = 27, SibSp = 0,
                          Parch = 2, Fare = 0, Cabin = "", Embarked = "S")

#probs####
Rylans_mom_prob <- predict(logistic_model, newdata = Rylans_Mom, type = "response")
print(Rylans_mom_prob)
Jons_prob <- predict(logistic_model, newdata = Jon_Dunham, type = "response")
print(Jons_prob)
Parkers_prob <- predict(logistic_model, newdata = Parker_LeFebvre, type = "response")
print(Parkers_prob)
Rylans_prob <- predict(logistic_model, newdata = Rylan_Murry, type = "response")
print(Rylans_prob)
Lukes_prob <- abs(predict(logistic_model, newdata = Luke_Wagoner, type = "response"))
print(Lukes_prob)
Parkers_mom_prob <- predict(logistic_model, newdata = Parkers_Mom, type = "response")
print(Parkers_mom_prob)
Ophelias_prob <- predict(logistic_model, newdata = Ophelia_Banks, type = "response")
print(Ophelias_prob)
Roses_prob <- predict(logistic_model, newdata = Rose_Bukater, type = "response")
print(Roses_prob)
Jacks_prob <- predict(logistic_model, newdata = Jack_Dawson, type = "response")
print(Jacks_prob)



generate_outcomes <- function(prob_survival, num_outcomes) {
  outcomes <- sample(c("SURVIVED", "DIED"), size = num_outcomes, prob = c(prob_survival, 1 - prob_survival), replace = TRUE)
  return(outcomes)
}

# filterable <- reactive ({
#   full %>% 
#     filter(Survived == input$Survived)
# }) 
  

ui <- dashboardPage(
  dashboardHeader(title = "Tinternic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Men Analysis", tabName = "mens", icon = icon("person")),
      menuItem("Women Analysis", tabName = "womens", icon = icon("person-dress")),
      radioButtons("Survived", "Drowned Filter:", choiceNames = c('All Passengers', 'Died', 'Survived'), choiceValues = c(2,0,1)),
      menuItem("Did you Survive?", tabName = "probs", icon = icon("person-drowning")
    )
  )),
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
                       column(width = 6, highchartOutput('')))),
      tabItem(tabName = "probs",
              fluidRow(title = "Parker",
                       column(width = 4, box(title = "Parker", paste0(round(Parkers_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_parker", "Click for Parker's Chance of Survival")),
                       column(width = 4, textOutput("result_parker"))),
              fluidRow(column(width = 4, box(title = "Rylan's Mom", paste0(round(Rylans_mom_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Rylans_Mom", "Click for Rylan's Mom's Chance of Survival")),
                       column(width = 4, textOutput("result_Rylans_Mom"))),
              fluidRow(column(width = 4, box(title = "Rylan", paste0(round(Rylans_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Rylan", "Click for Rylan's Chance of Survival")),
                       column(width = 4, textOutput("result_Rylan"))),
              fluidRow(column(width = 4, box(title = "Jon", paste0(round(Jons_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Jon", "Click for Jon's Chance of Survival")),
                       column(width = 4, textOutput("result_Jon"))),
              fluidRow(column(width = 4, box(title = "Luke", paste0(round(Lukes_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Luke", "Click for Luke's Chance of Survival")),
                       column(width = 4, textOutput("result_Luke"))),
              fluidRow(column(width = 4, box(title = "Parker's Mom", paste0(round(Parkers_mom_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Parkers_mom", "Click for Parker's Mom's Chance of Survival")),
                       column(width = 4, textOutput("result_Parkers_mom"))),
              fluidRow(column(width = 4, box(title = "Ophelia", paste0(round(Ophelias_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Ophelia", "Click for Ophelia's Chance of Survival")),
                       column(width = 4, textOutput("result_Ophelia"))),
              fluidRow(column(width = 4, box(title = "Rose", paste0(round(Roses_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Rose", "Click for Rose's Chance of Survival")),
                       column(width = 4, textOutput("result_Rose"))),
              fluidRow(column(width = 4, box(title = "Jack", paste0(round(Jacks_prob*100, 2), '%'))),
                       column(width = 4, actionButton("btn_Jack", "Click for Jack's Chance of Survival")),
                       column(width = 4, textOutput("result_Jack")))))))


server <- function(input, output) {
  
  filterable <- reactive ({
    if(input$Survived == 2){
      
      full %>%
        filter(!is.na(Survived))
      
    } else if(input$Survived != 2){
      
      full %>% 
        filter(Survived == input$Survived) 
      
    }
      
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
  
  #Who survived tab####
  
  num_outcomes <- 1000
  outcomes_parker <- generate_outcomes(prob_survival = 0.039, num_outcomes)
  outcomes_Rylans_Mom <- generate_outcomes(prob_survival = 0.6543, num_outcomes)
  outcomes_Jon <- generate_outcomes(prob_survival = 0.335, num_outcomes)
  outcomes_Rylan <- generate_outcomes(prob_survival = 1, num_outcomes)
  outcomes_Luke <- generate_outcomes(prob_survival = 0.116, num_outcomes)
  outcomes_Parkers_mom <- generate_outcomes(prob_survival = 1, num_outcomes)
  outcomes_Ophelia <- generate_outcomes(prob_survival = 0.226, num_outcomes)
  outcomes_Rose <- generate_outcomes(prob_survival = 1, num_outcomes)
  outcomes_Jack <- generate_outcomes(prob_survival = 0.094, num_outcomes)
  

  clicked_parker <- reactiveVal(NULL)
  clicked_Rylans_Mom <- reactiveVal(NULL)
  clicked_Jon <- reactiveVal(NULL)
  clicked_Rylan <- reactiveVal(NULL)
  clicked_Luke <- reactiveVal(NULL)
  clicked_Parkers_mom <- reactiveVal(NULL)
  clicked_Ophelia <- reactiveVal(NULL)
  clicked_Rose <- reactiveVal(NULL)
  clicked_Jack <- reactiveVal(NULL)
  
  observeEvent(input$btn_parker, {
    clicked_parker("btn_parker")
  })
  
  observeEvent(input$btn_Rylans_Mom, {
    clicked_Rylans_Mom("btn_Rylans_Mom")
  })
  
  observeEvent(input$btn_Jon, {
    clicked_Jon("btn_Jon")
  })
  
  observeEvent(input$btn_Rylan, {
    clicked_Rylan("btn_Rylan")
  })
  
  observeEvent(input$btn_Luke, {
    clicked_Luke("btn_Luke")
  })
  
  observeEvent(input$btn_Parkers_mom, {
    clicked_Parkers_mom("btn_Parkers_mom")
  })
  
  observeEvent(input$btn_Ophelia, {
    clicked_Ophelia("btn_Ophelia")
  })
  
  observeEvent(input$btn_Rose, {
    clicked_Rose("btn_Rose")
  })
  
  observeEvent(input$btn_Jack, {
    clicked_Jack("btn_Jack")
  })
  
  output$result_parker <- renderText({
    req(clicked_parker())
    outcome <- switch(
      clicked_parker(),
      "btn_parker" = outcomes_parker[1]
    )
    outcomes_parker <<- outcomes_parker[-1]
    paste("Parker", outcome)
  })
  
  output$result_Rylans_Mom <- renderText({
    req(clicked_Rylans_Mom())
    outcome <- switch(
      clicked_Rylans_Mom(),
      "btn_Rylans_Mom" = outcomes_Rylans_Mom[1]
    )
    outcomes_Rylans_Mom <<- outcomes_Rylans_Mom[-1]
    paste("Rylan's Mom", outcome)
  })
  
  output$result_Jon <- renderText({
    req(clicked_Jon())
    outcome <- switch(
      clicked_Jon(),
      "btn_Jon" = outcomes_Jon[1]
    )
    outcomes_Jon <<- outcomes_Jon[-1]
    paste("Jon", outcome)
  })
  
  output$result_Rylan <- renderText({
    req(clicked_Rylan())
    outcome <- switch(
      clicked_Rylan(),
      "btn_Rylan" = outcomes_Rylan[1]
    )
    outcomes_Rylan <<- outcomes_Rylan[-1]
    paste("Rylan", outcome)
  })
  
  output$result_Luke <- renderText({
    req(clicked_Luke())
    outcome <- switch(
      clicked_Luke(),
      "btn_Luke" = outcomes_Luke[1]
    )
    outcomes_Luke <<- outcomes_Luke[-1]
    paste("Luke", outcome)
  })
  
  output$result_Parkers_mom <- renderText({
    req(clicked_Parkers_mom())
    outcome <- switch(
      clicked_Parkers_mom(),
      "btn_Parkers_mom" = outcomes_Parkers_mom[1]
    )
    outcomes_Parkers_mom <<- outcomes_Parkers_mom[-1]
    paste("Parker's Mom", outcome)
  })
  
  output$result_Ophelia <- renderText({
    req(clicked_Ophelia())
    outcome <- switch(
      clicked_Ophelia(),
      "btn_Ophelia" = outcomes_Ophelia[1]
    )
    outcomes_Ophelia <<- outcomes_Ophelia[-1]
    paste("Ophelia", outcome)
  })
  
  output$result_Rose <- renderText({
    req(clicked_Rose())
    outcome <- switch(
      clicked_Rose(),
      "btn_Rose" = outcomes_Rose[1]
    )
    outcomes_Rose <<- outcomes_Rose[-1]
    paste("Rose", outcome)
  })
  
  output$result_Jack <- renderText({
    req(clicked_Jack())
    outcome <- switch(
      clicked_Jack(),
      "btn_Jack" = outcomes_Jack[1]
    )
    outcomes_Jack <<- outcomes_Jack[-1]
    paste("Jack", outcome)
  })
  
  
}

shinyApp(ui = ui, server = server)
