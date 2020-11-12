setwd('C:/Users/edz9905/Box/erin-work/bf_costing_optimization')
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
theme_set(theme_bw())

####  load and view data  ####
budget.df <- read_csv('C:/Users/edz9905/Box/erin-work/BF_costing_data/analysis_budget.csv')
head(budget.df)
str(budget.df)
view(budget.df)

budget.df %>% 
  filter(Level == "Objectif") 
budget.df <- budget.df %>% 
  mutate(Level = replace(Level, Level == 'Objectif', "Goal"))
budget.df %>% 
  filter(Level == 'Goal')

unit_costs.df <- read_csv('C:/Users/edz9905/Box/erin-work/BF_costing_data/analysis_unit_costs.csv')
head(unit_costs.df)
str(unit_costs.df)
view(unit_costs.df)

event_count.df <- read_csv('BF NSP projection 0/monthly_Event_Count.csv')
head(event_count.df)
str(event_count.df)
view(event_count.df)

#### view treatments received ####
ggplot(event_count.df, aes(date, Received_Treatment)) +
  geom_bar(stat = 'sum')
ggplot(event_count.df, aes(date, Received_Severe_Treatment )) +
  geom_bar(stat = 'sum')
ggplot(event_count.df, aes(date, Received_NMF_Treatment )) +
  geom_bar(stat = 'sum')
ggplot(event_count.df, aes(date, Bednet_Got_New_One )) +
  geom_bar(stat = 'sum')
ggplot(event_count.df, aes(date, Received_Campaign_Drugs  )) +
  geom_bar(stat = 'sum')
ggplot(event_count.df, aes(date, Received_IRS)) +
  geom_bar(stat = 'sum')

#### look at smc budget items ####
smc_budget <- budget.df %>% 
  filter(grepl("SMC | chemoprevention | Chemoprevention | smc | CPS | cps",`Row title`))
head(smc_budget)
view(smc_budget)

smc_budget$`Row title`

write.csv(smc_budget,'C:/Users/edz9905/Box/erin-work/BF_costing_data/cleaned_data/smc_budget.csv')


