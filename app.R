library(reactable)
library(tidyverse)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

full <- bind_rows(train, test)
str(full)



#brainstormed ideas:
# I want two tabs, male and female. Main filter being whether they survived or not
# Male tab: 4 charts. class, age, cabin, embarked
# Female tab: 4 charts. class, age, cabin, embarked
# tab 3: ML tab covering what had the greatest impact on survival/class/fare/age

output$class_man <- renderHighchart({
  men <- full[full$Sex == "male", ] %>% 
  count(Pclass) %>% 
    arrange(n) %>% 
    hchart(men, type = "treemap", hcaes(x = Pclass, value = n, color = n))
})
hchart(heatmap)

hchart(heatmap)

