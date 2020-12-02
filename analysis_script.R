setwd('C:/Users/edz9905/Box/NU-malaria-team/projects/hbhi_burkina/bf_costing_optimization/scripts')
working_dir <- 'C:/Users/edz9905/Box/NU-malaria-team/projects/hbhi_burkina/bf_costing_optimization'
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
library(viridis)
library(cowplot)
library(stringr)
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


#### look at BF costing categories  ####
full_budget.df <- read_csv(sprintf('%s/BF_costing_data/cleaned_data/full_detailed_budget.csv', working_dir))
full_budget.df$`Global Fund Modules` <- as.factor(full_budget.df$`Global Fund Modules`)
full_budget.df$`Global Fund interventions` <- as.factor(full_budget.df$`Global Fund interventions`)
temp <- which(full_budget.df$`Global Fund Cost Categories` == '2.0 Travel Costs (Travel)')
full_budget.df[temp,]$`Global Fund Cost Categories` <- '2.0 Travel costs (Travel)'
full_budget.df$`Cost 2021 (CFA)` <- as.numeric(full_budget.df$`Cost 2021 (CFA)`)

module_costs <- ggplot(full_budget.df %>% filter(Level == "Sub-activity"), 
       aes(`Global Fund Modules`, `Cost 2021 (CFA)`, fill = `Global Fund Cost Categories`)) +
  geom_bar(stat = 'identity', position = "stack") +
  scale_fill_viridis(discrete = T, option = 'plasma', na.value = "grey") +
  labs(title = "2021 Global Fund module costs by cost category", y = "Cost (2021 CFA)", 
       fill = "Cost Category", x = "") +
  theme(axis.text.x = element_text(angle = 40, hjust=1), legend.position = 'bottom') +
  guides(fill=guide_legend(nrow=4,byrow=TRUE, title.position="top", title.hjust = 0.5))

pdf("output/figures/GF_module_costs.pdf", height = 11, width = 8.5)
module_costs
dev.off()

module_interventions <- ggplot(full_budget.df %>% filter(Level == "Sub-activity"), 
                       aes(`Global Fund Modules`, `Cost 2021 (CFA)`, fill = `Global Fund interventions`)) +
  geom_bar(stat = 'identity', position = "stack") +
  scale_fill_viridis(discrete = T, option = 'plasma', na.value = "grey") +
  labs(title = "2021 Global Fund module costs by intervention category", y = "Cost (2021 CFA)", 
       fill = "Intervention Category", x = "") +
  theme(axis.text.x = element_text(angle = 40, hjust=1), legend.position = 'bottom',
        legend.key.size = unit(0.2, "cm"), legend.box="vertical", legend.margin=margin(), 
        legend.text = element_text(size = 5)) +
  guides(fill=guide_legend(ncol=2,byrow=F, title.position="top", title.hjust = 0.5))
module_interventions

pdf("output/figures/GF_module_interventions.pdf", height = 11, width = 8.5)
module_interventions
dev.off()

pc_module_interventions <- ggplot(full_budget.df %>% filter(Level == "Sub-activity"), 
                               aes(`Global Fund Modules`, `Cost 2021 (CFA)`, fill = `Global Fund interventions`)) +
  geom_bar(stat = 'identity', position = "fill") +
  scale_fill_viridis(discrete = T, option = 'plasma', na.value = "grey") +
  labs(title = "2021 Global Fund module costs by intervention category", y = "Percent of total module cost", 
       fill = "Intervention Category", x = "") +
  theme(axis.text.x = element_text(angle = 40, hjust=1), legend.position = 'bottom',
        legend.key.size = unit(0.2, "cm"), legend.box="vertical", legend.margin=margin(), 
        legend.text = element_text(size = 5)) +
  guides(fill=guide_legend(ncol=2,byrow=F, title.position="top", title.hjust = 0.5))
pc_module_interventions

pdf("output/figures/GF_module_interventions_percent.pdf", height = 11, width = 8.5)
pc_module_interventions
dev.off()


intervention_costs <- ggplot(full_budget.df %>% filter(Level == "Sub-activity"), 
                       aes(`Global Fund interventions`, `Cost 2021 (CFA)`, fill = `Global Fund Cost Categories`)) +
  geom_bar(stat = 'identity', position = "stack") +
  scale_fill_viridis(discrete = T, option = 'plasma') +
  labs(title = "2021 Global Fund intervention costs by cost category", y = "Cost (2021 CFA)", 
       fill = "Cost Category", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust=1), legend.position = 'bottom',
        legend.key.size = unit(0.2, "cm"), legend.box="vertical", legend.margin=margin(), 
        legend.text = element_text(size = 7)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE, title.position="top", title.hjust = 0.5))
intervention_costs

pdf("output/figures/GF_intervention_costs.pdf", height = 11, width = 8.5)
intervention_costs
dev.off()

view(full_budget.df %>% 
       filter(`Global Fund Modules` == "Specific prevention interventions"))

unique(full_budget.df %>% 
         filter(`Global Fund Modules` == "Specific prevention interventions")$`Global Fund interventions`)


cm_interventions <- ggplot(full_budget.df %>% filter(Level == "Sub-activity") %>% 
                                 filter(`Global Fund Modules` == "Case management"), 
                               aes(`Global Fund Modules`, `Cost 2021 (CFA)`, fill = `Global Fund interventions`)) +
  geom_bar(stat = 'identity', position = "fill") +
  scale_fill_viridis(discrete = T, option = 'plasma', na.value = "grey") +
  labs(title = "2021 Case management costs by intervention category", y = "% of cost", 
       fill = "Intervention Category", x = "") +
  theme(axis.text.x = element_text(angle = 40, hjust=1), legend.position = 'bottom',
        legend.key.size = unit(0.2, "cm"), legend.box="vertical", legend.margin=margin(), 
        legend.text = element_text(size = 5)) +
  guides(fill=guide_legend(ncol=2,byrow=F, title.position="top", title.hjust = 0.5))
cm_interventions

pdf("output/figures/GF_module_interventions.pdf", height = 11, width = 8.5)
cm_interventions
dev.off()

# separate modules for easier category viewing
percentage_bar <- function(df, filter_var, x_var, y_var, fill_var, x_label, y_label, fill_label) 
{
  ggplot(df, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_bar(stat = 'identity', position = "fill") +
    scale_fill_viridis(discrete = T, option = 'plasma', na.value = "grey") +
    labs(title = str_wrap(filter_var, 40),
         y = y_label, fill = fill_label, x = x_label) +
    theme(legend.position = 'bottom',legend.key.size = unit(0.2, "cm"), legend.box="vertical", 
          legend.margin=margin(), legend.text = element_text(size = 5),
          legend.title=element_text(size=10), plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    guides(fill=guide_legend(ncol=1,byrow=F, title.position="top", title.hjust = 0.5))
}

modules <- unique(subset(full_budget.df, full_budget.df$Level == "Sub-activity")$`Global Fund Modules`)
modules <- as.character(modules)

plot_list <- list()

for(i in 1:length(modules))
{
  temp_plot <- percentage_bar(full_budget.df %>% filter(Level == "Sub-activity") %>% 
                                filter(`Global Fund Modules` == modules[i]),
                              modules[i], 
                              "`Global Fund Modules`", "`Cost 2021 (CFA)`", "`Global Fund interventions`",
                              "", "% of cost", "Intervention category")
  plot_list[[i]] <- temp_plot
}

separate_modules <- plot_grid(plotlist = plot_list[c(9,10,11)])
separate_modules


pdf("output/figures/GF_module_interventions_separate3.pdf", height = 11, width = 8.5)
separate_modules
dev.off()


