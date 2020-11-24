setwd('C:/Users/edz9905/Box/erin-work/bf_costing_optimization')
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
library(viridis)
theme_set(theme_bw())

####  load data ####
event_count.df <- read_csv('BF_NSP projection_0/monthly_Event_Count.csv')
head(event_count.df)
str(event_count.df)
view(event_count.df)

smc_budget <- read_csv('C:/Users/edz9905/Box/erin-work/BF_costing_data/cleaned_data/smc_budget.csv')
head(smc_budget)
str(smc_budget)
view(smc_budget)


#### clean up smc_budget DS names (only run first time) ####
ggplot(event_count.df, aes(date, Received_Campaign_Drugs  )) +
  geom_bar(stat = 'sum')
ggplot(event_count.df, aes(DS_Name, Received_Campaign_Drugs)) +
  geom_bar(stat = 'sum')
unique(event_count.df$DS_Name)

smc_budget$DS_Name <- NA
smc_budget$DS_Name <- as.character(smc_budget$DS_Name)

for (i in 1:nrow(smc_budget))
{
  smc_budget[i,]$DS_Name <- tail(strsplit(smc_budget[i,]$`Row title`,split=" ")[[1]],1) 
}

# spot clean
sort(unique(smc_budget$DS_Name))
which(smc_budget$DS_Name == "G")
smc_budget[110,]$DS_Name <- "Gorom-Gorom"

which(smc_budget$DS_Name == "visit")
smc_budget[which(smc_budget$DS_Name == "Buffer"),]$DS_Name <- NA

# align spelling
model_DS <- sort(unique(event_count.df$DS_Name))
costed_DS <- sort(unique(smc_budget$DS_Name))
setdiff(costed_DS, model_DS)

smc_budget[which(smc_budget$DS_Name == "Nongr-Massom"),]$DS_Name <- "Nongr-Massoum"
write.csv(smc_budget,'C:/Users/edz9905/Box/erin-work/BF_costing_data/cleaned_data/smc_budget.csv')

#### calculate delivery costs per district ####
event_count.df$SMC_cost <- NA
event_count.df$SMC_type <- NA
event_count.df$SMC_cost <- as.numeric(event_count.df$SMC_cost)
event_count.df$SMC_type <- as.character(event_count.df$SMC_type)

# average over all runs
event_count.df$Avg_SMC <- NA
event_count.df$Avg_SMC <- as.double(event_count.df$Avg_SMC)
for(i in 1:nrow(event_count.df))
{
  temp_date <- event_count.df[i,]$date
  temp_DS <- event_count.df[i,]$DS_Name
  temp <- subset(event_count.df, event_count.df$date == temp_date & event_count.df$DS_Name == temp_DS)
  event_count.df[i,]$Avg_SMC <- mean(temp$Received_Campaign_Drugs) 
}

# Per budgeting spreadsheet costs in CFA 2021 per child per visit for SMC:
# supervised: 625
# unsupervised: 375
# unsupervised MC: 285.5

for (i in 1:nrow(event_count.df))
{
  district <- event_count.df[i,]$DS_Name
  matched_smc <- which(smc_budget$DS_Name == district &
                         grepl("3 to 59", smc_budget$`Row title`, fixed = TRUE))
  unit_cost <- as.numeric(smc_budget[matched_smc + 1,]$`Unit cost in local currency (CFA)`)
  event_count.df[i,]$SMC_cost <- unit_cost * event_count.df[i,]$Avg_SMC
  if (unit_cost == 285.5)
    event_count.df[i,]$SMC_type <- "Unsupervised_MC"
  if (unit_cost == 375)
    event_count.df[i,]$SMC_type <- "Unsupervised"
  if (unit_cost == 625)
    event_count.df[i,]$SMC_type <- "Supervised"
}

delivery_costs_smc <- ggplot(event_count.df %>% filter(date > "2020-12-31" & date < "2022-01-01"),
       aes(DS_Name, SMC_cost, fill = SMC_type)) +
  geom_bar(stat = "sum", show.legend=c(size=FALSE)) +
  labs(title = "Aggregate costs for delivering SMC in 2021 (NSP projection 0)", y = "Cost (2021 CFA)", 
       fill = "SMC delivery")+
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(breaks = seq(0,3000000,500000), labels = seq(0,300000,50000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(0.85,0.8))
delivery_costs_smc

pdf("C:/Users/edz9905/Box/erin-work/bf_costing_optimization/output/figures/delivery_costs_smc.pdf",
    height = 5, width = 8)
delivery_costs_smc
dev.off()

png("C:/Users/edz9905/Box/erin-work/bf_costing_optimization/output/figures/delivery_costs_smc.png",
    height = 5, width = 8, units = "in", res = 300)
delivery_costs_smc
dev.off()

#### calculate drug costs ####
drug_costs <- smc_budget %>% 
  filter((grepl("blisters",`Row title`) | grepl("Blister", `Row title`))
         & grepl("3 months to 59", Activity)) %>% 
  filter(!is.na(`Unit type`))
view(drug_costs)

# costs differ for ages bc different dosing (3-11mos and 12-59mos); situations (emergency
# vs normal) cost same per unit
# separate acquisition costs and supply/maintenance costs
# acquisition --> purchase costs
# supply costs --> mgmt fees, approach costs, ppm buffer, procurement agent fee, 
# freight and insurance costs

drug_costs <- drug_costs %>%
  mutate(Age = case_when(
    grepl("76.5",`Row title`) ~ "3-11mo",
    grepl("153",`Row title`) ~ "12-59mo"
  ))
view(drug_costs)
drug_costs$Age

drug_costs$Cost_type <- NA
drug_costs$Cost_type <- as.character(drug_costs$Cost_type)
for(i in 1:nrow(drug_costs))
{
  drug_costs[i,]$Cost_type <- paste(gsub(".*\\- ", "", drug_costs[i,]$`Row title`))
}
drug_costs$Cost_type
drug_costs$`Unit cost in local currency (CFA)` <- as.numeric(drug_costs$`Unit cost in local currency (CFA)`)

ggplot(drug_costs, aes(Cost_type, `Unit cost in local currency (CFA)`, fill = Age)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_viridis(discrete = T) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = c(0.9,0.8)) +
  labs(title = "Unit costs for SP+AQ blister packs in 2021", y = "Cost per pack (2021 CFA)", 
       fill = "Age\n(differing dose)", x = "Cost category")

drug_unit_costs <- ggplot(drug_costs, aes(Age, `Unit cost in local currency (CFA)`, fill = Cost_type)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = T) +
  labs(title = "Unit costs for SP+AQ blister packs in 2021", y = "Cost per pack (2021 CFA)", 
       fill = "Cost category", x = "Age category for dosage")

pdf("C:/Users/edz9905/Box/erin-work/bf_costing_optimization/output/figures/drug_unit_costs_smc.pdf",
    height = 5, width = 8)
drug_unit_costs
dev.off()

png("C:/Users/edz9905/Box/erin-work/bf_costing_optimization/output/figures/drug_unit_costs_smc.png",
    height = 5, width = 8, units = "in", res = 300)
drug_unit_costs
dev.off()

drug_costs$`Row title`

