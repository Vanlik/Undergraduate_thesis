#Libs----------------------------------------------------------
setwd("/Users/vanliktan/Desktop/V_LIK\ desktop\ /Undergrad_thesis/ES\ data/data")
library(tidyverse)
library(dplyr) #for data manipulation
library(rddtools) #for rdd est
library(plyr)
library(data.table) # data manipu.
library(knitr) #for making table
library(ggplot2) # for graph
library(foreign)
library(stargazer)
library(table1) # for descriptive table : https://thatdatatho.com/2018/08/20/easily-create-descriptive-summary-statistic-tables-r-studio/
library(xtable) 
#load package plm for panel data 
library(plm)
library(lmtest)
library(effects) # visulize the effect in logit reg
#*Start work here----------------------------------------------------------

#variables in interests
# l5 = num_female
# l10 = for_train
#   _2016_b7a = female manager
# e6 = foreign_tech
#b5 = year establishment
#f1 = capacity utilization exact_output/max_output (some Na)

#select subset of R dataframe
es_1316_5 <- as_tibble(es_1316[!is.na(es_1316$idstd2016),]) # Extract all the Na row from the panel; use View()
es_1316_5 <- as_tibble(es_1316_5[as.character(es_1316_5$panel) == "2013 and 2016",]) #Eligible for comparison
# col_list <- c()
# for (i in colnames(es_1316_5)){
#   if (str_detect(i, c('2016', '2013'))){
#     col_list <- append(col_list, c(i))
#     }
#   } #Take out columns with 2016 and 2013 : Changing variables 


names(es_1316_5)[names(es_1316_5) == "idstd2016"] <- "firm_id"  #same firm_id use idstd2016
names(es_1316_5)[names(es_1316_5) == "l1"] <- "num_labor" 
names(es_1316_5)[names(es_1316_5) == "l5"] <- "num_female_labor" 
names(es_1316_5)[names(es_1316_5) == "l10"] <- "formal_train" 
#names(es_1316_5)[names(es_1316_5) == "h7"] <- "r.d_i" #cost of r.d (in-house and contracted: 28 are not large over 31 valid value)
names(es_1316_5)[names(es_1316_5) == "a7"] <- "of_largefirm" 
names(es_1316_5)[names(es_1316_5) == "e6"] <- "foreign_tech" 

#only 2016 crossection
names(es_1316_5)[names(es_1316_5) == "_2016_eah15"] <- "formal_train_last3y" 
names(es_1316_5)[names(es_1316_5) == "_2016_eah16"] <- "foreign_tech_last3y" 
names(es_1316_5)[names(es_1316_5) == "_2016_d2"] <- "total_sales_2016" #total annual sales 
#################################3 have both 2013 and 2016
names(es_1316_5)[names(es_1316_5) == "n2a"] <- "labor_cost"  #labor cost 


names(es_1316_5)[names(es_1316_5) == "_2016_b7a"] <- "female_exec" #in that particular year

names(es_1316_5)[names(es_1316_5) == "_2016_a3a"] <- "samp_region"
names(es_1316_5)[names(es_1316_5) == "_2016_a0"] <- "samp_industry"

#Create a new dataframe with wanted columns
#Mutate dummy of inno. variables
firm_id <- unique(es_1316_5$firm_id)

es_1316.inno <- es_1316_5 %>% mutate(num_year_prodt =ifelse(h1 == 'Yes', 3,0)) #if yes -> num_year_inno = 3
es_1316.inno <- es_1316.inno %>% mutate(num_year_process =ifelse(h3 == 'Yes', 3,0)) 
es_1316.inno <- es_1316.inno %>% mutate(num_year_market =ifelse(h6 == 'Yes', 3,0)) 
es_1316.inno <- es_1316.inno %>% mutate(num_year_method =ifelse(h5 == 'Yes', 3,0))
es_1316.inno <- es_1316.inno %>% mutate(num_year_rd =ifelse(h7 == 'Yes', 3,0))

num_year_inno_2013 <- es_1316.inno$num_year_prodt[es_1316.inno$year == '2013'] + es_1316.inno$num_year_process[es_1316.inno$year == '2013'] + es_1316.inno$num_year_market[es_1316.inno$year == '2013']+es_1316.inno$num_year_method[es_1316.inno$year == '2013'] + es_1316.inno$num_year_rd[es_1316.inno$year == '2013']
num_year_inno_2016 <- es_1316.inno$num_year_prodt[es_1316.inno$year == '2016'] + es_1316.inno$num_year_process[es_1316.inno$year == '2016'] + es_1316.inno$num_year_market[es_1316.inno$year == '2016']+es_1316.inno$num_year_method[es_1316.inno$year == '2016'] + es_1316.inno$num_year_rd[es_1316.inno$year == '2016']


#num_labor
num_labor_2016 <- es_1316.inno$num_labor[es_1316.inno$year == '2016']
num_labor_2013 <- es_1316.inno$num_labor[es_1316.inno$year == '2013']

es_1316.inno <- es_1316_5 %>% mutate(inno.prox =ifelse(h1 == 'Yes'| h3 == 'Yes'| h6 == 'Yes'| h5 == 'Yes'|h7 == 'Yes', 1,0)) #either one is yes proxy
# inno.prox_2013 <- es_1316.inno$inno.prox[es_1316.inno$year== '2013']
inno.prox_2016 <- es_1316.inno$inno.prox[es_1316.inno$year== '2016']
#formal training
es_1316.inno <- es_1316_5 %>% mutate(formal_train =ifelse(formal_train == 'Yes', 1, 0)) # if either one of these is Yes
formal_train_2013 <- es_1316.inno$formal_train[es_1316.inno$year =='2013']
formal_train_2016 <- es_1316.inno$formal_train[es_1316.inno$year =='2016']

# #r.d
# es_1316.inno <- es_1316_5 %>% mutate(r.d_i =ifelse(r.d_i == 'Yes', 1, 0)) # if either one of these is Yes
# r.d_2013 <- es_1316.inno$r.d_i[es_1316.inno$year =='2013']
# r.d_2016 <- es_1316.inno$r.d_i[es_1316.inno$year =='2016']

#be part of large firm
es_1316.inno <- es_1316_5 %>% mutate(of_largefirm =ifelse(of_largefirm == 'Yes', 1, 0)) # if either one of these is Yes
of_largefirm_2013 <- es_1316.inno$of_largefirm[es_1316.inno$year =='2013']
of_largefirm_2016 <- es_1316.inno$of_largefirm[es_1316.inno$year =='2016']
#Foreign tech
es_1316.inno <- es_1316_5 %>% mutate(foreign_tech =ifelse(foreign_tech == 'Yes', 1, 0)) # if either one of these is Yes
foreign_tech_2013 <- es_1316.inno$foreign_tech[es_1316.inno$year =='2013']
foreign_tech_2016 <- es_1316.inno$foreign_tech[es_1316.inno$year =='2016']
#labor cost
labor_cost_2013 <- es_1316.inno$labor_cost[es_1316.inno$year =='2013']
labor_cost_2016 <- es_1316.inno$labor_cost[es_1316.inno$year =='2016']

 
### Crossection 2016-----------------------------------------------------------------------------------
es_1316.inno <- es_1316.inno %>% mutate(formal_train_last3y =ifelse(formal_train_last3y == 'Yes', 1,0))
formal_train_last3y <- es_1316.inno$formal_train_last3y[es_1316.inno$year =='2016']
es_1316.inno <- es_1316.inno %>% mutate(foreign_tech_last3y =ifelse(foreign_tech_last3y == 'Yes', 1,0))
foreign_tech_last3y <- es_1316.inno$foreign_tech_last3y[es_1316.inno$year =='2016']
#sales
total_sales_2016 <- es_1316.inno$total_sales_2016[es_1316.inno$year =='2016']
#female
es_1316.inno <- es_1316_5 %>% mutate(female_exec =ifelse(female_exec == 'Yes', 1, 0)) # if either one of these is Yes
#foreign_tech_2013 <- es_1316.inno$foreign_tech[es_1316.inno$year =='2013']
female_exec_2016 <- es_1316.inno$female_exec[es_1316.inno$year =='2016']

samp_region <- as.factor(na.omit(es_1316_5$samp_region))
samp_industry <- as.factor(na.omit(es_1316_5$samp_industry))

#colname <- c('firm_id', 'num_labor_2013','num_labor_2016', 'inno.prox_2013', 'inno.prox_2016', 'samp_region', 'samp_industry')

es_1316.new <- data.frame(firm_id, num_labor_2013, num_labor_2016,
                          num_year_inno_2013, num_year_inno_2016,
                          formal_train_2013, formal_train_2016,
                          formal_train_last3y, foreign_tech_last3y,
                          inno.prox_2016,
                          of_largefirm_2013, of_largefirm_2016,
                          foreign_tech_2013,foreign_tech_2016,
                          female_exec_2016, total_sales_2016,
                          labor_cost_2013,labor_cost_2016,
                          samp_industry, samp_region) #New Dataframe with specified column

es_1316.new[is.na(es_1316.new)] <- 0 #Kill all the NAs


#es_1316.newdiff_for_train <- ifelse(formal_train_2016 - formal_train_2013 ==1, 1,0)
##The Ys and the Xs
#Differeces
es_1316.new$diff_num_labor <- es_1316.new$num_labor_2016 - es_1316.new$num_labor_2013 #num_labor diff
es_1316.new$diff_num_year <- es_1316.new$num_year_inno_2016 - es_1316.new$num_year_inno_2013 #num_year diff
es_1316.new$labor_pro_2016 <- es_1316.new$total_sales_2016/es_1316.new$labor_cost_2016 #labor productivity 2016



#Model--------------------------------------
#for panel
summary(lm(diff_num_year ~ diff_num_labor+ samp_industry + samp_region - 1, data = es_1316.new))
logit_log_num <- glm(inno.prox_2016 ~ log(num_labor_2013) + formal_train_last3y + foreign_tech_last3y + samp_industry+ samp_region, 
    data = es_1316.new,family = 'binomial')

#So both years I see formal trainining has impact on number

lm_diff <- lm(diff_num_year ~ diff_num_labor + samp_industry + samp_region, data = es_1316.new) #see no effects

#for cross-section2016

#with control foreign tech and interation term
summary(glm(inno.prox_2016 ~ formal_train_last3y 
           + foreign_tech_last3y+ foreign_tech_last3y*formal_train_last3y+ samp_region -1 
           , data = es_1316.new, family = 'binomial'))
summary(lm(num_year_inno_2016 ~ formal_train_last3y 
           + foreign_tech_last3y + foreign_tech_last3y*formal_train_last3y + samp_industry + samp_region -1, data = es_1316.new))


##Labor productivity
cor(es_1316.new$labor_pro_2016, es_1316.new$num_labor_2016)
lm_labor_pro <- lm(labor_pro_2016 ~ num_labor_2016, data = es_1316.new) #diminishing marginal labor pro


#Results
stargazer(GPA.Model.1, GPA.Model.2,type="pdf", 
          column.labels = c("Main Effects", "Interaction"), #with and without interaction
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE)










#es_1316.inno <- es_1316.inno %>% group_by(firm_id) %>% mutate(diff_num = filter(es_1316.inno, year == "2016")$num_labor - filter(es_1316.inno, year == "2013")$num_labor)

## INNOVATION
#product inno.
#names(es_1316_5)[names(es_1316_5) == "h1"] <- "pro_new_last3y" #pro= product
# names(es_1316_5)[names(es_2016) == "eah2a"] <- "pro_pct_sale"
# names(es_1316_5)[names(es_2016) == "eah10"] <- "pro_who_developed"
# names(es_1316_5)[names(es_2016) == "eah11a"] <- "pro_attemp_last3y"
#process inno.
#names(es_1316_5)[names(es_1316_5) == "h3"] <- "impro_method" # impro= improvement on process 
# names(es_1316_5)[names(es_2016) == "h4a"] <- "impro_logistic"
# names(es_1316_5)[names(es_2016) == "h4b"] <- "impro_operation" # operation, accounting system

# #impact
# names(es_1316_5)[names(es_2016) == "eah12a"] <- "impro_automation"
# names(es_2016_5)[names(es_2016) == "eah12b"] <- "impro_new_tech" # whether or not the change helped introduce new method/technology
# names(es_1316_5)[names(es_2016) == "eah13"] <- "impro_who_developed"
#marketing inno.
#names(es_1316_5)[names(es_2016) == "h6"] <- "mar_method"
# names(es_1316_5)[names(es_2016) == "h1"] <- "new_product_last3y"

#organizational inno.
#names(es_1316_5)[names(es_1316_5) == "h5"] <- "impro_method"
#inno. activities: r.d
# names(es_1316_5)[names(es_1316_5) == "h7"] <- "r.d_i" #cost of r.d (in-house and contracted: 28 are not large over 31 valid value)


#func----------------------------------------------------------

# logit_2016 <- glm(formula = inno.prox_2016~num_labor_2016 + formal_train_2016 + 
#                foreign_tech_2016 + of_largefirm_2016 + r.d_2016+
#                samp_industry + samp_region, 
#              data = es_1316.new, family = 'binomial')
# 
# logit_2013 <- glm(formula = inno.prox_2013~num_labor_2013 + formal_train_2013 + 
#                     foreign_tech_2013 + of_largefirm_2013 + r.d_2013+
#                     samp_industry + samp_region, 
#                   data = es_1316.new, family = 'binomial')