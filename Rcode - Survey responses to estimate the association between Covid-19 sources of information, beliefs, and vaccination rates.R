
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("forcats")
library("reshape2")



#### Data Handling ####
# EU 494 2021 
eu494 <- read.spss("C:/Users/anton/Desktop/ZA7771_v1-0-0.sav", to.data.frame = T, use.missings = F)
eu494 <- eu494[c(5,7,9,41,77:86,101,102,103,106,107:109,110,111,112,113:116,99,100,92,93,168)]

names(eu494)[4] <- c("date_vacination")
names(eu494)[5:14] <- c("Info_EU", "Info_government","Info_health_authorities","Info_local_public_authorities","Info_health_professionals",
                        "Info_Media","Info_websites","Info_online_social_networks","Info_people","Info_DK")
names(eu494)[15:16] <- c("vaccine_safe", "vaccine_effective")
names(eu494)[17:18] <- c("Age", "Gender")
names(eu494)[19:21] <- c("Education", "Educ_11cat", "Educ_5cat")
names(eu494)[22:23] <- c("Employment", "Occupation")
names(eu494)[24] <- c("Type_community")
names(eu494)[25:28] <- c("Household_size_15_more", "Household_size_15_more_recod",
                         "Household_size_15_less", "Household_size_15_less_recod")
names(eu494)[29:30] <- c("vaccine_as_child", "vaccine_as_adult")
names(eu494)[31:32] <- c("handle_vaccination_government", "handle_vaccination_EU")



## Re-coding some variables and NA values 
#Individual-level data
eu494$date_vacination[eu494$date_vacination == "Don't know"] <- NA
eu494$date_vacination[eu494$date_vacination == "Prefer not to answer"] <- NA

eu494$date_vacination_3cat[eu494$date_vacination == "As soon as possible" | 
                               eu494$date_vacination == "I have already been vaccinated"] <- "Willing to be vaccinated"
eu494$date_vacination_3cat[eu494$date_vacination == "Some time in 2021" | 
                               eu494$date_vacination == "Later"] <- "Some time later"
eu494$date_vacination_3cat[eu494$date_vacination == "Never"] <- "Not willing to be vaccinated"
eu494$date_vacination_3cat[eu494$date_vacination == "Don't know"] <- NA
eu494$date_vacination_3cat[eu494$date_vacination == "Prefer not to answer"] <- NA
eu494$date_vacination_3cat <- as.factor(eu494$date_vacination_3cat)



eu494$vaccine_safe[eu494$vaccine_safe == "Don't know"] <- NA
eu494$vaccine_effective[eu494$vaccine_effective == "Don't know"] <- NA

eu494$vaccine_safe_2cat [eu494$vaccine_safe == "Totally agree" | eu494$vaccine_safe == "Tend to agree"] <- "Yes"
eu494$vaccine_safe_2cat [eu494$vaccine_safe == "Totally disagree" | eu494$vaccine_safe == "Tend to disagree"] <- "No"
eu494$vaccine_safe_2cat [eu494$vaccine_safe == "Don't know"] <- NA
eu494$vaccine_safe_2cat <- as.factor(eu494$vaccine_safe_2cat)

eu494$vaccine_effective_2cat [eu494$vaccine_effective == "Totally agree" | eu494$vaccine_effective == "Tend to agree"] <- "Yes"
eu494$vaccine_effective_2cat [eu494$vaccine_effective == "Totally disagree" | eu494$vaccine_effective == "Tend to disagree"] <- "No"
eu494$vaccine_effective_2cat [eu494$vaccine_effective == "Don't know"] <- NA
eu494$vaccine_effective_2cat <- as.factor(eu494$vaccine_effective_2cat)

eu494$vaccine_safe_cont[eu494$vaccine_safe == "Totally disagree"] <- 0
eu494$vaccine_safe_cont[eu494$vaccine_safe == "Tend to disagree"] <- 1
eu494$vaccine_safe_cont[eu494$vaccine_safe == "Tend to agree"] <- 2
eu494$vaccine_safe_cont[eu494$vaccine_safe == "Totally agree"] <- 3
eu494$vaccine_safe_cont[eu494$vaccine_safe == "Don't know"] <-NA

eu494$vaccine_effective_cont[eu494$vaccine_effective == "Totally disagree"] <- 0
eu494$vaccine_effective_cont[eu494$vaccine_effective == "Tend to disagree"] <- 1
eu494$vaccine_effective_cont[eu494$vaccine_effective == "Tend to agree"] <- 2
eu494$vaccine_effective_cont[eu494$vaccine_effective == "Totally agree"] <- 3
eu494$vaccine_effective_cont[eu494$vaccine_effective == "Don't know"] <-NA



eu494$Age_3clusters [ eu494$Age >= 15 & eu494$Age < 45 ] <- "15-44"  
eu494$Age_3clusters [ eu494$Age >= 45 & eu494$Age < 70 ] <- "45-69"  
eu494$Age_3clusters [ eu494$Age >= 70] <- "70+"  
eu494$Age_3clusters <- as.factor(eu494$Age_3clusters)

eu494$Age_3clusters2 [ eu494$Age >= 15 & eu494$Age < 35 ] <- "15-34"  
eu494$Age_3clusters2 [ eu494$Age >= 35 & eu494$Age < 65 ] <- "35-64"  
eu494$Age_3clusters2 [ eu494$Age >= 65] <- "65+"  
eu494$Age_3clusters2 <- as.factor(eu494$Age_3clusters2)

eu494$Age_6clusters [ eu494$Age >= 15 & eu494$Age < 25 ] <- "15-24"  
eu494$Age_6clusters [ eu494$Age >= 25 & eu494$Age < 35 ] <- "25-34"  
eu494$Age_6clusters [ eu494$Age >= 35 & eu494$Age < 45 ] <- "35-44"  
eu494$Age_6clusters [ eu494$Age >= 45 & eu494$Age < 55 ] <- "45-54"  
eu494$Age_6clusters [ eu494$Age >= 55 & eu494$Age < 65 ] <- "55-64"  
eu494$Age_6clusters [ eu494$Age >= 65 ] <- "65+"  
eu494$Age_6clusters <- as.factor(eu494$Age_6clusters)


eu494$Gender[eu494$Gender == "Prefer not to say"] <- NA
eu494$Educ_5cat[eu494$Educ_5cat == "Don't know"] <- NA
eu494$Educ_5cat[eu494$Educ_5cat == "Refusal"] <- NA
eu494$Type_community[eu494$Type_community == "DK"] <- NA
eu494$Employment[eu494$Employment == "Refusal"] <- NA
eu494$Employment<- fct_drop(eu494$Employment, only = "Refusal")


eu494$vaccine_as_child[eu494$vaccine_as_child == "Prefer not to answer"] <- NA
eu494$vaccine_as_child[eu494$vaccine_as_child == "Don't know"] <- NA
eu494$vaccine_as_adult[eu494$vaccine_as_adult == "Prefer not to answer"] <- NA
eu494$vaccine_as_adult[eu494$vaccine_as_adult == "Don't know"] <- NA


eu494$handle_vaccination_government[eu494$handle_vaccination_government == "Don't know"] <- NA
eu494$handle_vaccination_EU[eu494$handle_vaccination_EU == "Don't know"] <- NA


eu494$handle_vaccination_government_2cat [eu494$handle_vaccination_government == "Very satisfied" | eu494$handle_vaccination_government == "Fairly satisfied"] <- "Good"
eu494$handle_vaccination_government_2cat [eu494$handle_vaccination_government == "Not satisfied at all" | eu494$handle_vaccination_government == "Fairly dissatisfied"] <- "Bad"
eu494$handle_vaccination_government_2cat [eu494$handle_vaccination_government == "Don't know"] <- NA
eu494$handle_vaccination_government_2cat <- as.factor(eu494$handle_vaccination_government_2cat)

eu494$handle_vaccination_EU_2cat [eu494$handle_vaccination_EU == "Very satisfied" | eu494$handle_vaccination_EU == "Fairly satisfied"] <- "Good"
eu494$handle_vaccination_EU_2cat [eu494$handle_vaccination_EU == "Not satisfied at all" | eu494$handle_vaccination_EU == "Fairly dissatisfied"] <- "Bad"
eu494$handle_vaccination_EU_2cat [eu494$handle_vaccination_EU == "Don't know"] <- NA
eu494$handle_vaccination_EU_2cat <- as.factor(eu494$handle_vaccination_EU_2cat)

eu494$handle_vaccination_government_cont[eu494$handle_vaccination_government == "Not satisfied at all"] <- 0
eu494$handle_vaccination_government_cont[eu494$handle_vaccination_government == "Fairly dissatisfied"] <- 1
eu494$handle_vaccination_government_cont[eu494$handle_vaccination_government == "Fairly satisfied"] <- 2
eu494$handle_vaccination_government_cont[eu494$handle_vaccination_government == "Very satisfied"] <- 3
eu494$handle_vaccination_government_cont[eu494$handle_vaccination_government == "Don't know"] <-NA

eu494$handle_vaccination_EU_cont[eu494$handle_vaccination_EU == "Not satisfied at all"] <- 0
eu494$handle_vaccination_EU_cont[eu494$handle_vaccination_EU == "Fairly dissatisfied"] <- 1
eu494$handle_vaccination_EU_cont[eu494$handle_vaccination_EU == "Fairly satisfied"] <- 2
eu494$handle_vaccination_EU_cont[eu494$handle_vaccination_EU == "Very satisfied"] <- 3
eu494$handle_vaccination_EU_cont[eu494$handle_vaccination_EU == "Don't know"] <-NA



eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "0"] <- 0
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "1"] <- 1
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "10"] <- 10
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "12"] <- 12
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "13"] <- 13
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "14"] <-14
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "15"] <- 15
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "2"] <- 2
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "20"] <- 20
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "3"] <- 3
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "4"] <- 4
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "5"] <- 5
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "6"] <- 6 
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "7"] <- 7
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "8"] <- 8
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "9"] <- 9
eu494$Household_size_15_less_cont[eu494$Household_size_15_less == "DK/Refusal"] <- NA

eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "1"] <- 1
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "10"] <- 10
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "11"] <- 11
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "12"] <- 12
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "13"] <- 13
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "15"] <-15
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "16"] <- 16
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "2"] <- 2
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "26"] <- 26
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "29"] <- 29
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "3"] <- 3
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "30"] <- 30
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "4"] <- 4
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "5"] <- 5
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "6"] <- 6 
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "7"] <- 7
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "8"] <- 8
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "9"] <- 9
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "DK"] <- NA
eu494$Household_size_15_more_cont[eu494$Household_size_15_more == "Refusal"] <- NA

eu494$Household_size_cont <- eu494$Household_size_15_more_cont + eu494$Household_size_15_less_cont


eu494$Country_rec <- eu494$country
eu494$Country_rec<- fct_drop(eu494$Country_rec, only = c("DE-E Germany East (NO SEPARATE SAMPLE)",
                                                         "LI - Liechtenstein (NOT INCLUDED)", "IS - Iceland (NOT INCLUDED)", "CH - Switzerland (NOT INCLUDED)", "NO - Norway (NOT INCLUDED)",
                                                         "RS - Serbia (NOT INCLUDED", "ME - Montenegro (NOT INCLUDED)", "MK - Makedonia/FYROM (NOT INCLUDED)", "CY-TCC - Cyprus TCC (NOT INCLUDED)",
                                                         "TR - Turkey (NOT INCLUDED)", "-", "AL - Albania (NOT INCLUDED)", "MD - Moldavia (NOT INCLUDED)", "IL - Israel (NOT INCLUDED)",
                                                         "US - USA (NOT INCLUDED)", " (OUTDATED CODE FOR NORWAY)"))


eu494$Info_EU_cont[eu494$Info_EU == "The European Union"] <- 1
eu494$Info_EU_cont[eu494$Info_EU == "Not mentioned"] <- 0
eu494$Info_government_cont[eu494$Info_government == "The (NATIONALITY) government"] <- 1
eu494$Info_government_cont[eu494$Info_government == "Not mentioned"] <- 0
eu494$Info_health_authorities_cont[eu494$Info_health_authorities == "The (NATIONALITY) health authorities"] <- 1
eu494$Info_health_authorities_cont[eu494$Info_health_authorities == "Not mentioned"] <- 0
eu494$Info_local_public_authorities_cont[eu494$Info_local_public_authorities == "The regional or local public authorities"] <- 1
eu494$Info_local_public_authorities_cont[eu494$Info_local_public_authorities == "Not mentioned"] <- 0
eu494$Info_health_professionals_cont[eu494$Info_health_professionals == " Health professionals, doctors, nurses and pharmacists"] <- 1
eu494$Info_health_professionals_cont[eu494$Info_health_professionals == "Not mentioned"] <- 0
eu494$Info_Media_cont[eu494$Info_Media == "Media (television, radio, newspapers)"] <- 1
eu494$Info_Media_cont[eu494$Info_Media == "Not mentioned"] <- 0
eu494$Info_websites_cont[eu494$Info_websites == "Websites"] <- 1
eu494$Info_websites_cont[eu494$Info_websites == "Not mentioned"] <- 0
eu494$Info_online_social_networks_cont[eu494$Info_online_social_networks == "Online social networks"] <- 1
eu494$Info_online_social_networks_cont[eu494$Info_online_social_networks == "Not mentioned"] <- 0
eu494$Info_people_cont[eu494$Info_people == "People around you (colleagues, friends and family)"] <- 1
eu494$Info_people_cont[eu494$Info_people == "Not mentioned"] <- 0





# Country-level data
Country_data <- read.spss("C:/Users/anton/Desktop/Country_data.sav", to.data.frame = T, use.missings = F)

stringency_data <- read.csv("C:/Users/anton/Desktop/stringency_index.csv", header = T, dec = ".", sep = ",")
stringency_data <- stringency_data[c(2,3,510:515)]
names(stringency_data)[1] <- "Abr"
stringency_data$Abr <- as.character(stringency_data$Abr); stringency_data$Abr <- as.factor(stringency_data$Abr)
names(stringency_data)[3:8] <- c("stringency21May2021", "stringency22May2021", "stringency23May2021", 
                                 "stringency24May2021", "stringency25May2021", "stringency26May2021")
stringency_data <- stringency_data[which(stringency_data$country_name == "Austria" | stringency_data$country_name == "Belgium" 
                                         | stringency_data$country_name == "Bulgaria" | stringency_data$country_name == "Cyprus"
                                         | stringency_data$country_name == "Croatia" | stringency_data$country_name == "Czech Republic"
                                         | stringency_data$country_name == "Denmark" | stringency_data$country_name == "Estonia"
                                         | stringency_data$country_name == "Finland" | stringency_data$country_name == "France"
                                         | stringency_data$country_name == "Germany" | stringency_data$country_name == "Greece"
                                         | stringency_data$country_name == "Hungary" | stringency_data$country_name == "Ireland"
                                         | stringency_data$country_name == "Italy" | stringency_data$country_name == "Latvia"
                                         | stringency_data$country_name == "Lithuania" | stringency_data$country_name == "Luxembourg"
                                         | stringency_data$country_name == "Malta" | stringency_data$country_name == "Netherlands"
                                         | stringency_data$country_name == "Poland" | stringency_data$country_name == "Portugal"
                                         | stringency_data$country_name == "Romania" | stringency_data$country_name == "Slovak Republic"
                                         | stringency_data$country_name == "Slovenia" | stringency_data$country_name == "Spain"
                                         | stringency_data$country_name == "Sweden" ), ]



confirmed_cases_data <- read.csv("C:/Users/anton/Desktop/confirmed_cases.csv", header = T, dec = ".", sep = ",")
confirmed_cases_data <- confirmed_cases_data[c(2,3,510:515)]
names(confirmed_cases_data)[1] <- "Abr"
confirmed_cases_data$Abr <- as.character(confirmed_cases_data$Abr); confirmed_cases_data$Abr <- as.factor(confirmed_cases_data$Abr)
names(confirmed_cases_data)[3:8] <- c("confirmed_cases21May2021", "confirmed_cases22May2021", "confirmed_cases23May2021", 
                                 "confirmed_cases24May2021", "confirmed_cases25May2021", "confirmed_cases26May2021")
confirmed_cases_data <- confirmed_cases_data[which(confirmed_cases_data$country_name == "Austria" | confirmed_cases_data$country_name == "Belgium" 
                                         | confirmed_cases_data$country_name == "Bulgaria" | confirmed_cases_data$country_name == "Cyprus"
                                         | confirmed_cases_data$country_name == "Croatia" | confirmed_cases_data$country_name == "Czech Republic"
                                         | confirmed_cases_data$country_name == "Denmark" | confirmed_cases_data$country_name == "Estonia"
                                         | confirmed_cases_data$country_name == "Finland" | confirmed_cases_data$country_name == "France"
                                         | confirmed_cases_data$country_name == "Germany" | confirmed_cases_data$country_name == "Greece"
                                         | confirmed_cases_data$country_name == "Hungary" | confirmed_cases_data$country_name == "Ireland"
                                         | confirmed_cases_data$country_name == "Italy" | confirmed_cases_data$country_name == "Latvia"
                                         | confirmed_cases_data$country_name == "Lithuania" | confirmed_cases_data$country_name == "Luxembourg"
                                         | confirmed_cases_data$country_name == "Malta" | confirmed_cases_data$country_name == "Netherlands"
                                         | confirmed_cases_data$country_name == "Poland" | confirmed_cases_data$country_name == "Portugal"
                                         | confirmed_cases_data$country_name == "Romania" | confirmed_cases_data$country_name == "Slovak Republic"
                                         | confirmed_cases_data$country_name == "Slovenia" | confirmed_cases_data$country_name == "Spain"
                                         | confirmed_cases_data$country_name == "Sweden" ), ]




confirmed_deaths_data <- read.csv("C:/Users/anton/Desktop/confirmed_deaths.csv", header = T, dec = ".", sep = ",")
confirmed_deaths_data <- confirmed_deaths_data[c(2,3,510:515)]
names(confirmed_deaths_data)[1] <- "Abr"
confirmed_deaths_data$Abr <- as.character(confirmed_deaths_data$Abr); confirmed_deaths_data$Abr <- as.factor(confirmed_deaths_data$Abr)
names(confirmed_deaths_data)[3:8] <- c("confirmed_deaths21May2021", "confirmed_deaths22May2021", "confirmed_deaths23May2021", 
                                 "confirmed_deaths24May2021", "confirmed_deaths25May2021", "confirmed_deaths26May2021")
confirmed_deaths_data <- confirmed_deaths_data[which(confirmed_deaths_data$country_name == "Austria" | confirmed_deaths_data$country_name == "Belgium" 
                                         | confirmed_deaths_data$country_name == "Bulgaria" | confirmed_deaths_data$country_name == "Cyprus"
                                         | confirmed_deaths_data$country_name == "Croatia" | confirmed_deaths_data$country_name == "Czech Republic"
                                         | confirmed_deaths_data$country_name == "Denmark" | confirmed_deaths_data$country_name == "Estonia"
                                         | confirmed_deaths_data$country_name == "Finland" | confirmed_deaths_data$country_name == "France"
                                         | confirmed_deaths_data$country_name == "Germany" | confirmed_deaths_data$country_name == "Greece"
                                         | confirmed_deaths_data$country_name == "Hungary" | confirmed_deaths_data$country_name == "Ireland"
                                         | confirmed_deaths_data$country_name == "Italy" | confirmed_deaths_data$country_name == "Latvia"
                                         | confirmed_deaths_data$country_name == "Lithuania" | confirmed_deaths_data$country_name == "Luxembourg"
                                         | confirmed_deaths_data$country_name == "Malta" | confirmed_deaths_data$country_name == "Netherlands"
                                         | confirmed_deaths_data$country_name == "Poland" | confirmed_deaths_data$country_name == "Portugal"
                                         | confirmed_deaths_data$country_name == "Romania" | confirmed_deaths_data$country_name == "Slovak Republic"
                                         | confirmed_deaths_data$country_name == "Slovenia" | confirmed_deaths_data$country_name == "Spain"
                                         | confirmed_deaths_data$country_name == "Sweden" ), ]

Country_data$Abr <- fct_expand(Country_data$Abr, c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC",
                                                   "HUN", "IRL", "ITA", "HRV", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU",
                                                   "SVK", "SVN", "SWE")) 

Country_data$Abr[Country_data$Abr == "AUT                     "] <- "AUT"
Country_data$Abr[Country_data$Abr == "BEL                     "] <- "BEL"
Country_data$Abr[Country_data$Abr == "BGR                     "] <- "BGR"
Country_data$Abr[Country_data$Abr == "CYP                     "] <- "CYP"
Country_data$Abr[Country_data$Abr == "CZE                     "] <- "CZE"
Country_data$Abr[Country_data$Abr == "DEU                     "] <- "DEU"
Country_data$Abr[Country_data$Abr == "DNK                     "] <- "DNK"
Country_data$Abr[Country_data$Abr == "ESP                     "] <- "ESP"
Country_data$Abr[Country_data$Abr == "EST                     "] <- "EST" 
Country_data$Abr[Country_data$Abr == "FIN                     " ] <- "FIN"
Country_data$Abr[Country_data$Abr == "FRA                     "] <- "FRA"
Country_data$Abr[Country_data$Abr == "GRC                     "] <- "GRC"
Country_data$Abr[Country_data$Abr == "HUN                     " ] <- "HUN"
Country_data$Abr[Country_data$Abr == "IRL                     "] <- "IRL"
Country_data$Abr[Country_data$Abr == "ITA                     " ] <- "ITA"
Country_data$Abr[Country_data$Abr == "KRV                     "] <- "HRV" 
Country_data$Abr[Country_data$Abr == "LTU                     "] <- "LTU"
Country_data$Abr[Country_data$Abr == "LUX                     "] <- "LUX"
Country_data$Abr[Country_data$Abr == "LVA                     " ] <- "LVA"
Country_data$Abr[Country_data$Abr == "MLT                     "] <- "MLT"
Country_data$Abr[Country_data$Abr == "NLD                     " ] <- "NLD"
Country_data$Abr[Country_data$Abr == "POL                     "] <- "POL"
Country_data$Abr[Country_data$Abr == "PRT                     " ] <- "PRT"
Country_data$Abr[Country_data$Abr == "ROU                     " ] <- "ROU"
Country_data$Abr[Country_data$Abr == "SVK                     "] <-  "SVK"
Country_data$Abr[Country_data$Abr == "SVN                     "] <- "SVN" 
Country_data$Abr[Country_data$Abr == "SWE                     "] <- "SWE"
Country_data$Abr<- fct_drop(Country_data$Abr, only = c("AUT                     ", "BEL                     ", "BGR                     ", 
                                                       "CYP                     ", "CZE                     ", "DEU                     ", 
                                                       "DNK                     ", "ESP                     ", "EST                     ", 
                                                       "FIN                     ", "FRA                     ", "GRC                     ",
                                                       "HUN                     ", "IRL                     ", "ITA                     ", 
                                                       "KRV                     ", "LTU                     ", "LUX                     ", 
                                                       "LVA                     ", "MLT                     ", "NLD                     ", 
                                                       "POL                     ", "PRT                     ", "ROU                     ",
                                                       "SVK                     ", "SVN                     ", "SWE                     "))

Country_data2 <- merge(Country_data, stringency_data, by = "Abr", all = T)
Country_data2 <- merge(Country_data2, confirmed_cases_data, by = "Abr",  all = T)
Country_data2 <- merge(Country_data2, confirmed_deaths_data, by = "Abr",  all = T)

vTert = quantile(Country_data2$Govern_health_spending_Perc, c(0:3/3), )
Country_data2$Govern_health_spending_terciles = with(Country_data2, 
                                                     cut(Govern_health_spending_Perc, 
                                                         vTert, 
                                                         include.lowest = T, 
                                                         labels = c("Low", "Medium", "High")))

eu494$Govern_health_spending_terciles[eu494$country == "AT - Austria"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "AT - Austria          "]
eu494$Govern_health_spending_terciles[eu494$country == "BE - Belgium"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "BE - Belgium          "]
eu494$Govern_health_spending_terciles[eu494$country == "BG - Bulgaria"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "BG - Bulgaria         "]
eu494$Govern_health_spending_terciles[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$Govern_health_spending_terciles[eu494$country == "CZ - Czech Republic"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "CZ - Czech Republic   "]
eu494$Govern_health_spending_terciles[eu494$country == "DE - Germany"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "DE Germany            "]
eu494$Govern_health_spending_terciles[eu494$country == "DK - Denmark"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "DK - Denmark          "]
eu494$Govern_health_spending_terciles[eu494$country == "EE - Estonia"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "EE - Estonia          "]
eu494$Govern_health_spending_terciles[eu494$country == "ES -Spain"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "ES -Spain             "]
eu494$Govern_health_spending_terciles[eu494$country == "FI - Finland"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "FI - Finland          "]
eu494$Govern_health_spending_terciles[eu494$country == "FR - France"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "FR - France           "]
eu494$Govern_health_spending_terciles[eu494$country == "GR - Greece"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "GR - Greece           "]
eu494$Govern_health_spending_terciles[eu494$country == "HR - Croatia"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "HR - Croatia          "]
eu494$Govern_health_spending_terciles[eu494$country == "HU - Hungary"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "HU - Hungary          "]
eu494$Govern_health_spending_terciles[eu494$country == "IE - Ireland"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "IE - Ireland          "]
eu494$Govern_health_spending_terciles[eu494$country == "IT - Italy"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "IT - Italy            "]
eu494$Govern_health_spending_terciles[eu494$country == "LT - Lithuania"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "LT - Lithuania        "]
eu494$Govern_health_spending_terciles[eu494$country == "LU - Luxembourg"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "LU - Luxembourg       "]
eu494$Govern_health_spending_terciles[eu494$country == "LV - Latvia"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "LV - Latvia           "]
eu494$Govern_health_spending_terciles[eu494$country == "MT - Malta"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "MT - Malta            "]
eu494$Govern_health_spending_terciles[eu494$country == "NL - The Netherlands"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "NL - The Netherlands  "]
eu494$Govern_health_spending_terciles[eu494$country == "PL - Poland"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "PL - Poland           "]
eu494$Govern_health_spending_terciles[eu494$country == "PT - Portugal"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "PT - Portugal         "]
eu494$Govern_health_spending_terciles[eu494$country == "RO - Romania"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "RO - Romania          "]
eu494$Govern_health_spending_terciles[eu494$country == "SE - Sweden"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "SE - Sweden           "]
eu494$Govern_health_spending_terciles[eu494$country == "SI - Slovenia"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "SI - Slovenia         "]
eu494$Govern_health_spending_terciles[eu494$country == "SK - Slovakia"] <- Country_data2$Govern_health_spending_terciles[Country_data2$Country == "SK - Slovakia         "]

eu494$Govern_health_spending_terciles <- as.factor(eu494$Govern_health_spending_terciles)



# Computing means of time series country-level data
Country_data2$stringency_mean <- (Country_data2$stringency21May2021 + Country_data2$stringency22May2021 + Country_data2$stringency23May2021 +
    Country_data2$stringency24May2021 + Country_data2$stringency25May2021 + Country_data2$stringency26May2021)/6
Country_data2$stringency_mean_0_1 <- Country_data2$stringency_mean/100

Country_data2$confirmed_cases_mean <- (Country_data2$confirmed_cases21May2021 + Country_data2$confirmed_cases22May2021 + Country_data2$confirmed_cases23May2021 +
                                      Country_data2$confirmed_cases24May2021 + Country_data2$confirmed_cases25May2021 + Country_data2$confirmed_cases26May2021)/6
Country_data2$confirmed_cases_mean_100k <- (Country_data2$confirmed_cases_mean/Country_data2$Pop2020)*100000

Country_data2$confirmed_deaths_mean <- (Country_data2$confirmed_deaths21May2021 + Country_data2$confirmed_deaths22May2021 + Country_data2$confirmed_deaths23May2021 +
                                      Country_data2$confirmed_deaths24May2021 + Country_data2$confirmed_deaths25May2021 + Country_data2$confirmed_deaths26May2021)/6
Country_data2$confirmed_deaths_mean_100k <- (Country_data2$confirmed_deaths_mean/Country_data2$Pop2020)*100000

Country_data2$confirmed_cases_mean_100k_z <- scale(Country_data2$confirmed_cases_mean_100k, center = T, scale = T)
Country_data2$confirmed_deaths_mean_100k_z <- scale(Country_data2$confirmed_deaths_mean_100k, center = T, scale = T)



eu494$stringency_mean[eu494$country == "AT - Austria"] <- Country_data2$stringency_mean[Country_data2$Country == "AT - Austria          "]
eu494$stringency_mean[eu494$country == "BE - Belgium"] <- Country_data2$stringency_mean[Country_data2$Country == "BE - Belgium          "]
eu494$stringency_mean[eu494$country == "BG - Bulgaria"] <- Country_data2$stringency_mean[Country_data2$Country == "BG - Bulgaria         "]
eu494$stringency_mean[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$stringency_mean[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$stringency_mean[eu494$country == "CZ - Czech Republic"] <- Country_data2$stringency_mean[Country_data2$Country == "CZ - Czech Republic   "]
eu494$stringency_mean[eu494$country == "DE - Germany"] <- Country_data2$stringency_mean[Country_data2$Country == "DE Germany            "]
eu494$stringency_mean[eu494$country == "DK - Denmark"] <- Country_data2$stringency_mean[Country_data2$Country == "DK - Denmark          "]
eu494$stringency_mean[eu494$country == "EE - Estonia"] <- Country_data2$stringency_mean[Country_data2$Country == "EE - Estonia          "]
eu494$stringency_mean[eu494$country == "ES -Spain"] <- Country_data2$stringency_mean[Country_data2$Country == "ES -Spain             "]
eu494$stringency_mean[eu494$country == "FI - Finland"] <- Country_data2$stringency_mean[Country_data2$Country == "FI - Finland          "]
eu494$stringency_mean[eu494$country == "FR - France"] <- Country_data2$stringency_mean[Country_data2$Country == "FR - France           "]
eu494$stringency_mean[eu494$country == "GR - Greece"] <- Country_data2$stringency_mean[Country_data2$Country == "GR - Greece           "]
eu494$stringency_mean[eu494$country == "HR - Croatia"] <- Country_data2$stringency_mean[Country_data2$Country == "HR - Croatia          "]
eu494$stringency_mean[eu494$country == "HU - Hungary"] <- Country_data2$stringency_mean[Country_data2$Country == "HU - Hungary          "]
eu494$stringency_mean[eu494$country == "IE - Ireland"] <- Country_data2$stringency_mean[Country_data2$Country == "IE - Ireland          "]
eu494$stringency_mean[eu494$country == "IT - Italy"] <- Country_data2$stringency_mean[Country_data2$Country == "IT - Italy            "]
eu494$stringency_mean[eu494$country == "LT - Lithuania"] <- Country_data2$stringency_mean[Country_data2$Country == "LT - Lithuania        "]
eu494$stringency_mean[eu494$country == "LU - Luxembourg"] <- Country_data2$stringency_mean[Country_data2$Country == "LU - Luxembourg       "]
eu494$stringency_mean[eu494$country == "LV - Latvia"] <- Country_data2$stringency_mean[Country_data2$Country == "LV - Latvia           "]
eu494$stringency_mean[eu494$country == "MT - Malta"] <- Country_data2$stringency_mean[Country_data2$Country == "MT - Malta            "]
eu494$stringency_mean[eu494$country == "NL - The Netherlands"] <- Country_data2$stringency_mean[Country_data2$Country == "NL - The Netherlands  "]
eu494$stringency_mean[eu494$country == "PL - Poland"] <- Country_data2$stringency_mean[Country_data2$Country == "PL - Poland           "]
eu494$stringency_mean[eu494$country == "PT - Portugal"] <- Country_data2$stringency_mean[Country_data2$Country == "PT - Portugal         "]
eu494$stringency_mean[eu494$country == "RO - Romania"] <- Country_data2$stringency_mean[Country_data2$Country == "RO - Romania          "]
eu494$stringency_mean[eu494$country == "SE - Sweden"] <- Country_data2$stringency_mean[Country_data2$Country == "SE - Sweden           "]
eu494$stringency_mean[eu494$country == "SI - Slovenia"] <- Country_data2$stringency_mean[Country_data2$Country == "SI - Slovenia         "]
eu494$stringency_mean[eu494$country == "SK - Slovakia"] <- Country_data2$stringency_mean[Country_data2$Country == "SK - Slovakia         "]





eu494$stringency_mean_0_1[eu494$country == "AT - Austria"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "AT - Austria          "]
eu494$stringency_mean_0_1[eu494$country == "BE - Belgium"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "BE - Belgium          "]
eu494$stringency_mean_0_1[eu494$country == "BG - Bulgaria"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "BG - Bulgaria         "]
eu494$stringency_mean_0_1[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$stringency_mean_0_1[eu494$country == "CZ - Czech Republic"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "CZ - Czech Republic   "]
eu494$stringency_mean_0_1[eu494$country == "DE - Germany"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "DE Germany            "]
eu494$stringency_mean_0_1[eu494$country == "DK - Denmark"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "DK - Denmark          "]
eu494$stringency_mean_0_1[eu494$country == "EE - Estonia"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "EE - Estonia          "]
eu494$stringency_mean_0_1[eu494$country == "ES -Spain"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "ES -Spain             "]
eu494$stringency_mean_0_1[eu494$country == "FI - Finland"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "FI - Finland          "]
eu494$stringency_mean_0_1[eu494$country == "FR - France"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "FR - France           "]
eu494$stringency_mean_0_1[eu494$country == "GR - Greece"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "GR - Greece           "]
eu494$stringency_mean_0_1[eu494$country == "HR - Croatia"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "HR - Croatia          "]
eu494$stringency_mean_0_1[eu494$country == "HU - Hungary"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "HU - Hungary          "]
eu494$stringency_mean_0_1[eu494$country == "IE - Ireland"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "IE - Ireland          "]
eu494$stringency_mean_0_1[eu494$country == "IT - Italy"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "IT - Italy            "]
eu494$stringency_mean_0_1[eu494$country == "LT - Lithuania"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "LT - Lithuania        "]
eu494$stringency_mean_0_1[eu494$country == "LU - Luxembourg"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "LU - Luxembourg       "]
eu494$stringency_mean_0_1[eu494$country == "LV - Latvia"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "LV - Latvia           "]
eu494$stringency_mean_0_1[eu494$country == "MT - Malta"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "MT - Malta            "]
eu494$stringency_mean_0_1[eu494$country == "NL - The Netherlands"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "NL - The Netherlands  "]
eu494$stringency_mean_0_1[eu494$country == "PL - Poland"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "PL - Poland           "]
eu494$stringency_mean_0_1[eu494$country == "PT - Portugal"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "PT - Portugal         "]
eu494$stringency_mean_0_1[eu494$country == "RO - Romania"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "RO - Romania          "]
eu494$stringency_mean_0_1[eu494$country == "SE - Sweden"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "SE - Sweden           "]
eu494$stringency_mean_0_1[eu494$country == "SI - Slovenia"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "SI - Slovenia         "]
eu494$stringency_mean_0_1[eu494$country == "SK - Slovakia"] <- Country_data2$stringency_mean_0_1[Country_data2$Country == "SK - Slovakia         "]





eu494$confirmed_cases_mean_100k[eu494$country == "AT - Austria"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "AT - Austria          "]
eu494$confirmed_cases_mean_100k[eu494$country == "BE - Belgium"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "BE - Belgium          "]
eu494$confirmed_cases_mean_100k[eu494$country == "BG - Bulgaria"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "BG - Bulgaria         "]
eu494$confirmed_cases_mean_100k[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$confirmed_cases_mean_100k[eu494$country == "CZ - Czech Republic"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "CZ - Czech Republic   "]
eu494$confirmed_cases_mean_100k[eu494$country == "DE - Germany"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "DE Germany            "]
eu494$confirmed_cases_mean_100k[eu494$country == "DK - Denmark"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "DK - Denmark          "]
eu494$confirmed_cases_mean_100k[eu494$country == "EE - Estonia"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "EE - Estonia          "]
eu494$confirmed_cases_mean_100k[eu494$country == "ES -Spain"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "ES -Spain             "]
eu494$confirmed_cases_mean_100k[eu494$country == "FI - Finland"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "FI - Finland          "]
eu494$confirmed_cases_mean_100k[eu494$country == "FR - France"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "FR - France           "]
eu494$confirmed_cases_mean_100k[eu494$country == "GR - Greece"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "GR - Greece           "]
eu494$confirmed_cases_mean_100k[eu494$country == "HR - Croatia"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "HR - Croatia          "]
eu494$confirmed_cases_mean_100k[eu494$country == "HU - Hungary"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "HU - Hungary          "]
eu494$confirmed_cases_mean_100k[eu494$country == "IE - Ireland"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "IE - Ireland          "]
eu494$confirmed_cases_mean_100k[eu494$country == "IT - Italy"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "IT - Italy            "]
eu494$confirmed_cases_mean_100k[eu494$country == "LT - Lithuania"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "LT - Lithuania        "]
eu494$confirmed_cases_mean_100k[eu494$country == "LU - Luxembourg"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "LU - Luxembourg       "]
eu494$confirmed_cases_mean_100k[eu494$country == "LV - Latvia"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "LV - Latvia           "]
eu494$confirmed_cases_mean_100k[eu494$country == "MT - Malta"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "MT - Malta            "]
eu494$confirmed_cases_mean_100k[eu494$country == "NL - The Netherlands"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "NL - The Netherlands  "]
eu494$confirmed_cases_mean_100k[eu494$country == "PL - Poland"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "PL - Poland           "]
eu494$confirmed_cases_mean_100k[eu494$country == "PT - Portugal"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "PT - Portugal         "]
eu494$confirmed_cases_mean_100k[eu494$country == "RO - Romania"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "RO - Romania          "]
eu494$confirmed_cases_mean_100k[eu494$country == "SE - Sweden"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "SE - Sweden           "]
eu494$confirmed_cases_mean_100k[eu494$country == "SI - Slovenia"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "SI - Slovenia         "]
eu494$confirmed_cases_mean_100k[eu494$country == "SK - Slovakia"] <- Country_data2$confirmed_cases_mean_100k[Country_data2$Country == "SK - Slovakia         "]





eu494$confirmed_cases_mean_100k_z[eu494$country == "AT - Austria"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "AT - Austria          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "BE - Belgium"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "BE - Belgium          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "BG - Bulgaria"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "BG - Bulgaria         "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$confirmed_cases_mean_100k_z[eu494$country == "CZ - Czech Republic"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "CZ - Czech Republic   "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "DE - Germany"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "DE Germany            "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "DK - Denmark"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "DK - Denmark          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "EE - Estonia"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "EE - Estonia          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "ES -Spain"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "ES -Spain             "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "FI - Finland"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "FI - Finland          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "FR - France"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "FR - France           "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "GR - Greece"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "GR - Greece           "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "HR - Croatia"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "HR - Croatia          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "HU - Hungary"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "HU - Hungary          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "IE - Ireland"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "IE - Ireland          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "IT - Italy"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "IT - Italy            "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "LT - Lithuania"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "LT - Lithuania        "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "LU - Luxembourg"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "LU - Luxembourg       "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "LV - Latvia"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "LV - Latvia           "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "MT - Malta"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "MT - Malta            "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "NL - The Netherlands"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "NL - The Netherlands  "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "PL - Poland"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "PL - Poland           "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "PT - Portugal"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "PT - Portugal         "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "RO - Romania"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "RO - Romania          "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "SE - Sweden"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "SE - Sweden           "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "SI - Slovenia"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "SI - Slovenia         "]
eu494$confirmed_cases_mean_100k_z[eu494$country == "SK - Slovakia"] <- Country_data2$confirmed_cases_mean_100k_z[Country_data2$Country == "SK - Slovakia         "]





eu494$confirmed_deaths_mean_100k[eu494$country == "AT - Austria"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "AT - Austria          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "BE - Belgium"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "BE - Belgium          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "BG - Bulgaria"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "BG - Bulgaria         "]
eu494$confirmed_deaths_mean_100k[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$confirmed_deaths_mean_100k[eu494$country == "CZ - Czech Republic"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "CZ - Czech Republic   "]
eu494$confirmed_deaths_mean_100k[eu494$country == "DE - Germany"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "DE Germany            "]
eu494$confirmed_deaths_mean_100k[eu494$country == "DK - Denmark"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "DK - Denmark          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "EE - Estonia"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "EE - Estonia          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "ES -Spain"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "ES -Spain             "]
eu494$confirmed_deaths_mean_100k[eu494$country == "FI - Finland"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "FI - Finland          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "FR - France"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "FR - France           "]
eu494$confirmed_deaths_mean_100k[eu494$country == "GR - Greece"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "GR - Greece           "]
eu494$confirmed_deaths_mean_100k[eu494$country == "HR - Croatia"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "HR - Croatia          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "HU - Hungary"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "HU - Hungary          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "IE - Ireland"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "IE - Ireland          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "IT - Italy"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "IT - Italy            "]
eu494$confirmed_deaths_mean_100k[eu494$country == "LT - Lithuania"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "LT - Lithuania        "]
eu494$confirmed_deaths_mean_100k[eu494$country == "LU - Luxembourg"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "LU - Luxembourg       "]
eu494$confirmed_deaths_mean_100k[eu494$country == "LV - Latvia"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "LV - Latvia           "]
eu494$confirmed_deaths_mean_100k[eu494$country == "MT - Malta"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "MT - Malta            "]
eu494$confirmed_deaths_mean_100k[eu494$country == "NL - The Netherlands"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "NL - The Netherlands  "]
eu494$confirmed_deaths_mean_100k[eu494$country == "PL - Poland"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "PL - Poland           "]
eu494$confirmed_deaths_mean_100k[eu494$country == "PT - Portugal"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "PT - Portugal         "]
eu494$confirmed_deaths_mean_100k[eu494$country == "RO - Romania"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "RO - Romania          "]
eu494$confirmed_deaths_mean_100k[eu494$country == "SE - Sweden"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "SE - Sweden           "]
eu494$confirmed_deaths_mean_100k[eu494$country == "SI - Slovenia"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "SI - Slovenia         "]
eu494$confirmed_deaths_mean_100k[eu494$country == "SK - Slovakia"] <- Country_data2$confirmed_deaths_mean_100k[Country_data2$Country == "SK - Slovakia         "]





eu494$confirmed_deaths_mean_100k_z[eu494$country == "AT - Austria"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "AT - Austria          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "BE - Belgium"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "BE - Belgium          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "BG - Bulgaria"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "BG - Bulgaria         "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "CY - Cyprus (Republic)"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "CY - Cyprus (Republic)"]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "CZ - Czech Republic"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "CZ - Czech Republic   "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "DE - Germany"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "DE Germany            "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "DK - Denmark"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "DK - Denmark          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "EE - Estonia"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "EE - Estonia          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "ES -Spain"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "ES -Spain             "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "FI - Finland"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "FI - Finland          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "FR - France"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "FR - France           "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "GR - Greece"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "GR - Greece           "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "HR - Croatia"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "HR - Croatia          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "HU - Hungary"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "HU - Hungary          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "IE - Ireland"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "IE - Ireland          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "IT - Italy"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "IT - Italy            "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "LT - Lithuania"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "LT - Lithuania        "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "LU - Luxembourg"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "LU - Luxembourg       "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "LV - Latvia"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "LV - Latvia           "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "MT - Malta"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "MT - Malta            "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "NL - The Netherlands"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "NL - The Netherlands  "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "PL - Poland"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "PL - Poland           "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "PT - Portugal"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "PT - Portugal         "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "RO - Romania"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "RO - Romania          "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "SE - Sweden"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "SE - Sweden           "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "SI - Slovenia"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "SI - Slovenia         "]
eu494$confirmed_deaths_mean_100k_z[eu494$country == "SK - Slovakia"] <- Country_data2$confirmed_deaths_mean_100k_z[Country_data2$Country == "SK - Slovakia         "]



# Filter - removing individual with missing information or "Don't know" responses in Sources of Information variable
eu494 <- eu494[-c(which(eu494$Info_online_social_networks == "Not mentioned" & eu494$Info_people == "Not mentioned" & 
                                  eu494$Info_EU == "Not mentioned" & eu494$Info_government == "Not mentioned" & eu494$Info_health_authorities == "Not mentioned" & eu494$Info_local_public_authorities == "Not mentioned" & 
                                  eu494$Info_health_professionals == "Not mentioned" & eu494$Info_Media == "Not mentioned" & eu494$Info_websites == "Not mentioned" &
                                  eu494$Info_DK == "Don't know")), ]



#### Latent class analyses ####
library(poLCA)

eu494$Info_EU_cont_2[eu494$Info_EU_cont == 0 ] <- 1
eu494$Info_EU_cont_2[eu494$Info_EU_cont == 1 ] <- 2
eu494$Info_government_cont_2[eu494$Info_government_cont == 0 ] <- 1
eu494$Info_government_cont_2[eu494$Info_government_cont == 1 ] <- 2
eu494$Info_health_authorities_cont_2[eu494$Info_health_authorities_cont == 0 ] <- 1
eu494$Info_health_authorities_cont_2[eu494$Info_health_authorities_cont == 1 ] <- 2
eu494$Info_local_public_authorities_cont_2[eu494$Info_local_public_authorities_cont == 0 ] <- 1
eu494$Info_local_public_authorities_cont_2[eu494$Info_local_public_authorities_cont == 1 ] <- 2
eu494$Info_health_professionals_cont_2[eu494$Info_health_professionals_cont == 0 ] <- 1
eu494$Info_health_professionals_cont_2[eu494$Info_health_professionals_cont == 1 ] <- 2
eu494$Info_Media_cont_2[eu494$Info_Media_cont == 0 ] <- 1
eu494$Info_Media_cont_2[eu494$Info_Media_cont == 1 ] <- 2
eu494$Info_websites_cont_2[eu494$Info_websites_cont == 0 ] <- 1
eu494$Info_websites_cont_2[eu494$Info_websites_cont == 1 ] <- 2
eu494$Info_online_social_networks_cont_2[eu494$Info_online_social_networks_cont == 0 ] <- 1
eu494$Info_online_social_networks_cont_2[eu494$Info_online_social_networks_cont == 1 ] <- 2
eu494$Info_people_cont_2[eu494$Info_people_cont == 0 ] <- 1
eu494$Info_people_cont_2[eu494$Info_people_cont == 1 ] <- 2

Latent_variables = cbind(Info_EU_cont_2, Info_government_cont_2, Info_health_authorities_cont_2, Info_local_public_authorities_cont_2, 
                         Info_health_professionals_cont_2, Info_Media_cont_2, Info_websites_cont_2, Info_online_social_networks_cont_2,
                         Info_people_cont_2) ~ 1


M1 <- poLCA(Latent_variables, eu494, nclass = 1, verbose = F, na.rm = T, maxiter = 10000, nrep = 2)
M1
poLCA.entropy(M1)

##RELATIVE ENTROPY
##Numerator:
nume.E1 <- -sum(M1$posterior * log(M1$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E1 <- 22994*log(1)
##Relative Entropy
Entro1 <- 1-(nume.E1/deno.E1)
Entro1



M2 <- poLCA(Latent_variables, eu494, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2
poLCA.entropy(M2)

##RELATIVE ENTROPY
##Numerator:
nume.E2 <- -sum(M2$posterior * log(M2$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E2 <- 22994*log(2)
##Relative Entropy
Entro2 <- 1-(nume.E2/deno.E2)
Entro2



M3 <- poLCA(Latent_variables, eu494, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3
poLCA.entropy(M3)

##RELATIVE ENTROPY
##Numerator:
nume.E3 <- -sum(M3$posterior * log(M3$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E3 <- 22994*log(3)
##Relative Entropy
Entro3 <- 1-(nume.E3/deno.E3)
Entro3



M4 <- poLCA(Latent_variables, eu494, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M4
poLCA.entropy(M4)


##RELATIVE ENTROPY
##Numerator:
nume.E4 <- -sum(M4$posterior * log(M4$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E4 <- 22994*log(4)
##Relative Entropy
Entro4 <- 1-(nume.E4/deno.E4)
Entro4




M5 <- poLCA(Latent_variables, eu494, nclass = 5, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M5
poLCA.entropy(M5)



##RELATIVE ENTROPY
##Numerator:
nume.E5 <- -sum(M5$posterior * log(M5$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E5 <- 22994*log(5)
##Relative Entropy
Entro5 <- 1-(nume.E5/deno.E5)
Entro5




M6 <- poLCA(Latent_variables, eu494, nclass = 6, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M6
poLCA.entropy(M6)


##RELATIVE ENTROPY
##Numerator:
nume.E6 <- -sum(M6$posterior * log(M6$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E6 <- 22994*log(6)
##Relative Entropy
Entro6 <- 1-(nume.E6/deno.E6)
Entro6



M7 <- poLCA(Latent_variables, eu494, nclass = 7, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M7
poLCA.entropy(M7)


##RELATIVE ENTROPY
##Numerator:
nume.E7 <- -sum(M7$posterior * log(M7$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E7 <- 22994*log(7)
##Relative Entropy
Entro7 <- 1-(nume.E7/deno.E7)
Entro7



M8 <- poLCA(Latent_variables, eu494, nclass = 8, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M8
poLCA.entropy(M8)


##RELATIVE ENTROPY
##Numerator:
nume.E8 <- -sum(M8$posterior * log(M8$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E8 <- 22994*log(8)
##Relative Entropy
Entro8 <- 1-(nume.E8/deno.E8)
Entro8


M9 <- poLCA(Latent_variables, eu494, nclass = 9, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M9
poLCA.entropy(M9)


##RELATIVE ENTROPY
##Numerator:
nume.E9 <- -sum(M9$posterior * log(M9$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E9 <- 22994*log(9)
##Relative Entropy
Entro9 <- 1-(nume.E9/deno.E9); Entro9




## Assigning latent class to each participant in the survey and making some plots
library(descr)


# Four LCA groups
eu494$predclass_4 <- M4$predclass
eu494$predclass_4  <- as.factor(eu494$predclass_4)

crosstab(eu494$predclass_4, eu494$Info_EU,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_government,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_health_authorities,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_local_public_authorities,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_health_professionals,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_Media,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_websites,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_online_social_networks,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_4, eu494$Info_people,weight = eu494$w1, prop.r = T,digits = 2, plot = F)




eu494$joint_safety [eu494$predclass_4 == "1" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "1_yes_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "1" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "1_yes_later"
eu494$joint_safety [eu494$predclass_4 == "1" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "1_yes_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "1" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "1_no_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "1" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "1_no_later"
eu494$joint_safety [eu494$predclass_4 == "1" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "1_no_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "2" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "2_yes_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "2" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "2_yes_later"
eu494$joint_safety [eu494$predclass_4 == "2" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "2_yes_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "2" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "2_no_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "2" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "2_no_later"
eu494$joint_safety [eu494$predclass_4 == "2" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "2_no_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "3" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "3_yes_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "3" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "3_yes_later"
eu494$joint_safety [eu494$predclass_4 == "3" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "3_yes_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "3" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "3_no_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "3" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "3_no_later"
eu494$joint_safety [eu494$predclass_4 == "3" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "3_no_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "4" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "4_yes_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "4" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "4_yes_later"
eu494$joint_safety [eu494$predclass_4 == "4" & eu494$vaccine_safe_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "4_yes_notvaccinated"
eu494$joint_safety [eu494$predclass_4 == "4" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "4_no_vaccinated"
eu494$joint_safety [eu494$predclass_4 == "4" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "4_no_later"
eu494$joint_safety [eu494$predclass_4 == "4" & eu494$vaccine_safe_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "4_no_notvaccinated"
eu494$joint_safety <- as.factor(eu494$joint_safety)



eu494$joint_effect [eu494$predclass_4 == "1" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "1_yes_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "1" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "1_yes_later"
eu494$joint_effect [eu494$predclass_4 == "1" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "1_yes_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "1" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "1_no_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "1" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "1_no_later"
eu494$joint_effect [eu494$predclass_4 == "1" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "1_no_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "2" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "2_yes_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "2" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "2_yes_later"
eu494$joint_effect [eu494$predclass_4 == "2" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "2_yes_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "2" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "2_no_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "2" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "2_no_later"
eu494$joint_effect [eu494$predclass_4 == "2" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "2_no_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "3" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "3_yes_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "3" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "3_yes_later"
eu494$joint_effect [eu494$predclass_4 == "3" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "3_yes_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "3" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "3_no_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "3" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "3_no_later"
eu494$joint_effect [eu494$predclass_4 == "3" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "3_no_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "4" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "4_yes_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "4" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Some time later"] <- "4_yes_later"
eu494$joint_effect [eu494$predclass_4 == "4" & eu494$vaccine_effective_2cat == "Yes" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "4_yes_notvaccinated"
eu494$joint_effect [eu494$predclass_4 == "4" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "4_no_vaccinated"
eu494$joint_effect [eu494$predclass_4 == "4" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Some time later"] <- "4_no_later"
eu494$joint_effect [eu494$predclass_4 == "4" & eu494$vaccine_effective_2cat == "No" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "4_no_notvaccinated"
eu494$joint_effect <- as.factor(eu494$joint_effect)




eu494$joint_govern [eu494$predclass_4 == "1" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "1_yes_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "1" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Some time later"] <- "1_yes_later"
eu494$joint_govern [eu494$predclass_4 == "1" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "1_yes_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "1" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "1_no_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "1" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Some time later"] <- "1_no_later"
eu494$joint_govern [eu494$predclass_4 == "1" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "1_no_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "2" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "2_yes_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "2" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Some time later"] <- "2_yes_later"
eu494$joint_govern [eu494$predclass_4 == "2" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "2_yes_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "2" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "2_no_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "2" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Some time later"] <- "2_no_later"
eu494$joint_govern [eu494$predclass_4 == "2" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "2_no_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "3" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "3_yes_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "3" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Some time later"] <- "3_yes_later"
eu494$joint_govern [eu494$predclass_4 == "3" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "3_yes_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "3" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "3_no_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "3" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Some time later"] <- "3_no_later"
eu494$joint_govern [eu494$predclass_4 == "3" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "3_no_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "4" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "4_yes_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "4" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Some time later"] <- "4_yes_later"
eu494$joint_govern [eu494$predclass_4 == "4" & eu494$handle_vaccination_EU_2cat == "Good" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "4_yes_notvaccinated"
eu494$joint_govern [eu494$predclass_4 == "4" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Willing to be vaccinated"] <- "4_no_vaccinated"
eu494$joint_govern [eu494$predclass_4 == "4" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Some time later"] <- "4_no_later"
eu494$joint_govern [eu494$predclass_4 == "4" & eu494$handle_vaccination_EU_2cat == "Bad" & eu494$date_vacination_3cat == "Not willing to be vaccinated"] <- "4_no_notvaccinated"
eu494$joint_govern <- as.factor(eu494$joint_govern)






# Three LCA groups
eu494$predclass_3 <- M3$predclass
eu494$predclass_3  <- as.factor(eu494$predclass_3)

crosstab(eu494$predclass_3, eu494$Info_EU,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_government,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_health_authorities,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_local_public_authorities,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_health_professionals,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_Media,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_websites,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_online_social_networks,weight = eu494$w1, prop.r = T,digits = 2, plot = F)
crosstab(eu494$predclass_3, eu494$Info_people,weight = eu494$w1, prop.r = T,digits = 2, plot = F)




#### Propensity score matching method with CBPS #### 
"https://cran.r-project.org/web/packages/CBPS/index.html"
library(CBPS)
set.seed(123456)

data_complete_4LCA <- eu494[complete.cases(eu494$predclass_4), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$Age), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$Gender), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$Educ_5cat), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$Employment), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$Type_community), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$Household_size_cont), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$vaccine_as_child), ]
data_complete_4LCA <- data_complete_4LCA[complete.cases(data_complete_4LCA$vaccine_as_adult), ]


Matching_4LCA <- CBPS(predclass_4 ~ Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont +
                          vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                          confirmed_deaths_mean_100k_z + stringency_mean, 
                      data = data_complete_4LCA, na.action = na.exclude, method = "over", sample.weights = data_complete_4LCA$w1)
summary(Matching_4LCA); plot(Matching_4LCA)
data_complete_4LCA$CBPS_weights_4LCA <- Matching_4LCA$weights
data_complete_4LCA <- data_complete_4LCA[c("serialid","CBPS_weights_4LCA")]

eu494 <- merge(eu494, data_complete_4LCA, by =  "serialid", all = T)




#### Descriptive statistics ####
library(survey)
TotalPop2020 <- sum(Country_data2$Pop2020)
Country_data2$weight_Pop2020 <- (Country_data2$Pop2020)/TotalPop2020

desc.w <- svydesign(ids = ~1, data = eu494, weights = eu494$w1)
desc.c <- svydesign(ids = ~1, data = Country_data2, weights = Country_data2$weight_Pop2020)

## ALL 
# Dependent variables
svytable(~vaccine_effective, design = desc.w);prop.table(svytable(~vaccine_effective, design = desc.w))
svytable(~vaccine_effective_2cat, design = desc.w);prop.table(svytable(~vaccine_effective_2cat, design = desc.w))

svytable(~vaccine_safe, design = desc.w);prop.table(svytable(~vaccine_safe, design = desc.w))
svytable(~vaccine_safe_2cat, design = desc.w);prop.table(svytable(~vaccine_safe_2cat, design = desc.w))

svytable(~date_vacination, design = desc.w);prop.table(svytable(~date_vacination, design = desc.w))
svytable(~date_vacination_3cat, design = desc.w);prop.table(svytable(~date_vacination_3cat, design = desc.w))

svytable(~handle_vaccination_government_2cat, design = desc.w);prop.table(svytable(~handle_vaccination_government_2cat, design = desc.w))


svytable(~joint_safety, design = desc.w);prop.table(svytable(~joint_safety, design = desc.w))
svytable(~joint_effect, design = desc.w);prop.table(svytable(~joint_effect, design = desc.w))
svytable(~joint_govern, design = desc.w);prop.table(svytable(~joint_govern, design = desc.w))


# Independent varibles
svytable(~predclass_4, design = desc.w);prop.table(svytable(~predclass_4, design = desc.w))


# Covariates
svymean(~Age, design = desc.w);sqrt(svyvar(~Age, design = desc.w))
svytable(~Age_3clusters2, design = desc.w);prop.table(svytable(~Age_3clusters2, design = desc.w))

svytable(~Gender, design = desc.w);prop.table(svytable(~Gender, design = desc.w))

svytable(~Educ_5cat, design = desc.w);prop.table(svytable(~Educ_5cat, design = desc.w))
svytable(~Employment, design = desc.w);prop.table(svytable(~Employment, design = desc.w))
svytable(~Type_community, design = desc.w);prop.table(svytable(~Type_community, design = desc.w))
svymean(~Household_size_cont, design = desc.w, na.rm = T);sqrt(svyvar(~Household_size_cont, design = desc.w))

svytable(~vaccine_as_child, design = desc.w);prop.table(svytable(~vaccine_as_child, design = desc.w))
svytable(~vaccine_as_adult, design = desc.w);prop.table(svytable(~vaccine_as_adult, design = desc.w))


svytable(~Govern_health_spending_terciles, design = desc.c);prop.table(svytable(~Govern_health_spending_terciles, design = desc.c))
svymean(~Govern_health_spending_Perc, design = desc.c);sqrt(svyvar(~Govern_health_spending_Perc, design = desc.c))
svymean(~stringency_mean, design = desc.c);sqrt(svyvar(~stringency_mean, design = desc.c))
svymean(~confirmed_deaths_mean_100k, design = desc.c);sqrt(svyvar(~confirmed_deaths_mean_100k, design = desc.c))
svymean(~confirmed_cases_mean_100k, design = desc.c);sqrt(svyvar(~confirmed_cases_mean_100k, design = desc.c))
svymean(~confirmed_deaths_mean_100k_z, design = desc.c);sqrt(svyvar(~confirmed_deaths_mean_100k, design = desc.c))
svymean(~confirmed_cases_mean_100k_z, design = desc.c);sqrt(svyvar(~confirmed_cases_mean_100k, design = desc.c))



## Cross-table descriptive statistics
library(descr)
library(Hmisc)

eu494 %>%  group_by(predclass_4) %>% 
    summarise(n = n(), 
              mean_housesize = wtd.mean(Household_size_cont, weights = w1, na.rm = T),
              sd_housesize = sqrt(wtd.var(Household_size_cont, weights = w1, na.rm = T)),
              
              mean_age = wtd.mean(Age, weights = w1, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w1, na.rm = T)),
              
              mean_stringency = wtd.mean(stringency_mean, weights = w1, na.rm = T),
              sd_stringency = sqrt(wtd.var(stringency_mean, weights = w1, na.rm = T)),
              
              mean_cases = wtd.mean(confirmed_cases_mean_100k, weights = w1, na.rm = T),
              sd_cases = sqrt(wtd.var(confirmed_cases_mean_100k, weights = w1, na.rm = T)),
              
              mean_deaths = wtd.mean(confirmed_deaths_mean_100k, weights = w1, na.rm = T),
              sd_deaths = sqrt(wtd.var(confirmed_deaths_mean_100k, weights = w1, na.rm = T)),
              
              mean_cases_z = wtd.mean(confirmed_cases_mean_100k_z, weights = w1, na.rm = T),
              sd_cases_z = sqrt(wtd.var(confirmed_cases_mean_100k_z, weights = w1, na.rm = T)),
              
              mean_deaths_z = wtd.mean(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T),
              sd_deaths_z = sqrt(wtd.var(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T)))

summary(aov(Age ~ predclass_4, data = eu494, weight=eu494$w1))
summary(aov(Household_size_cont ~ predclass_4, data = eu494, weight=eu494$w1))
summary(aov(stringency_mean ~ predclass_4, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k ~ predclass_4, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k ~ predclass_4, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k_z ~ predclass_4, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k_z ~ predclass_4, data = eu494, weight=eu494$w1))


crosstab(eu494$predclass_4, eu494$Age_3clusters2,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$Gender,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$Educ_5cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$Employment,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$Type_community,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$vaccine_as_child,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$vaccine_as_adult,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$handle_vaccination_government_2cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$vaccine_safe_2cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$vaccine_effective_2cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$date_vacination_3cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$predclass_4, eu494$Govern_health_spending_terciles,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)



eu494 %>%  group_by(vaccine_safe_2cat) %>% 
    summarise(n = n(), 
              mean_housesize = wtd.mean(Household_size_cont, weights = w1, na.rm = T),
              sd_housesize = sqrt(wtd.var(Household_size_cont, weights = w1, na.rm = T)),
              
              mean_age = wtd.mean(Age, weights = w1, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w1, na.rm = T)),
              
              mean_stringency = wtd.mean(stringency_mean, weights = w1, na.rm = T),
              sd_stringency = sqrt(wtd.var(stringency_mean, weights = w1, na.rm = T)),
              
              mean_cases = wtd.mean(confirmed_cases_mean_100k, weights = w1, na.rm = T),
              sd_cases = sqrt(wtd.var(confirmed_cases_mean_100k, weights = w1, na.rm = T)),
              
              mean_deaths = wtd.mean(confirmed_deaths_mean_100k, weights = w1, na.rm = T),
              sd_deaths = sqrt(wtd.var(confirmed_deaths_mean_100k, weights = w1, na.rm = T)),
              
              mean_cases_z = wtd.mean(confirmed_cases_mean_100k_z, weights = w1, na.rm = T),
              sd_cases_z = sqrt(wtd.var(confirmed_cases_mean_100k_z, weights = w1, na.rm = T)),
              
              mean_deaths_z = wtd.mean(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T),
              sd_deaths_z = sqrt(wtd.var(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T)))

summary(aov(Age ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))
summary(aov(Household_size_cont ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))
summary(aov(stringency_mean ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k_z ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k_z ~ vaccine_safe_2cat, data = eu494, weight=eu494$w1))


crosstab(eu494$vaccine_safe_2cat, eu494$Age_3clusters2,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$Gender,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$Educ_5cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$Employment,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$Type_community,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$vaccine_as_child,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$vaccine_as_adult,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$date_vacination_3cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_safe_2cat, eu494$Govern_health_spending_terciles,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)



eu494 %>%  group_by(vaccine_effective_2cat) %>% 
    summarise(n = n(), 
              mean_housesize = wtd.mean(Household_size_cont, weights = w1, na.rm = T),
              sd_housesize = sqrt(wtd.var(Household_size_cont, weights = w1, na.rm = T)),
              
              mean_age = wtd.mean(Age, weights = w1, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w1, na.rm = T)),
              
              mean_stringency = wtd.mean(stringency_mean, weights = w1, na.rm = T),
              sd_stringency = sqrt(wtd.var(stringency_mean, weights = w1, na.rm = T)),
              
              mean_cases = wtd.mean(confirmed_cases_mean_100k, weights = w1, na.rm = T),
              sd_cases = sqrt(wtd.var(confirmed_cases_mean_100k, weights = w1, na.rm = T)),
              
              mean_deaths = wtd.mean(confirmed_deaths_mean_100k, weights = w1, na.rm = T),
              sd_deaths = sqrt(wtd.var(confirmed_deaths_mean_100k, weights = w1, na.rm = T)),
              
              mean_cases_z = wtd.mean(confirmed_cases_mean_100k_z, weights = w1, na.rm = T),
              sd_cases_z = sqrt(wtd.var(confirmed_cases_mean_100k_z, weights = w1, na.rm = T)),
              
              mean_deaths_z = wtd.mean(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T),
              sd_deaths_z = sqrt(wtd.var(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T)))

summary(aov(Age ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))
summary(aov(Household_size_cont ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))
summary(aov(stringency_mean ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k_z ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k_z ~ vaccine_effective_2cat, data = eu494, weight=eu494$w1))


crosstab(eu494$vaccine_effective_2cat, eu494$Age_3clusters2,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$Gender,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$Educ_5cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$Employment,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$Type_community,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$vaccine_as_child,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$vaccine_as_adult,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$date_vacination_3cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$vaccine_effective_2cat, eu494$Govern_health_spending_terciles,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)



eu494 %>%  group_by(handle_vaccination_government_2cat) %>% 
    summarise(n = n(), 
              mean_housesize = wtd.mean(Household_size_cont, weights = w1, na.rm = T),
              sd_housesize = sqrt(wtd.var(Household_size_cont, weights = w1, na.rm = T)),
              
              mean_age = wtd.mean(Age, weights = w1, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w1, na.rm = T)),
              
              mean_stringency = wtd.mean(stringency_mean, weights = w1, na.rm = T),
              sd_stringency = sqrt(wtd.var(stringency_mean, weights = w1, na.rm = T)),
              
              mean_cases = wtd.mean(confirmed_cases_mean_100k, weights = w1, na.rm = T),
              sd_cases = sqrt(wtd.var(confirmed_cases_mean_100k, weights = w1, na.rm = T)),
              
              mean_deaths = wtd.mean(confirmed_deaths_mean_100k, weights = w1, na.rm = T),
              sd_deaths = sqrt(wtd.var(confirmed_deaths_mean_100k, weights = w1, na.rm = T)),
              
              mean_cases_z = wtd.mean(confirmed_cases_mean_100k_z, weights = w1, na.rm = T),
              sd_cases_z = sqrt(wtd.var(confirmed_cases_mean_100k_z, weights = w1, na.rm = T)),
              
              mean_deaths_z = wtd.mean(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T),
              sd_deaths_z = sqrt(wtd.var(confirmed_deaths_mean_100k_z, weights = w1, na.rm = T)))

summary(aov(Age ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))
summary(aov(Household_size_cont ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))
summary(aov(stringency_mean ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_cases_mean_100k_z ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))
summary(aov(confirmed_deaths_mean_100k_z ~ handle_vaccination_government_2cat, data = eu494, weight=eu494$w1))


crosstab(eu494$handle_vaccination_government_2cat, eu494$Age_3clusters2,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$Gender,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$Educ_5cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$Employment,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$Type_community,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$vaccine_as_child,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$vaccine_as_adult,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$date_vacination_3cat,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)
crosstab(eu494$handle_vaccination_government_2cat, eu494$Govern_health_spending_terciles,weight = eu494$w1, prop.c = T,digits = 1, plot = F, chisq = T)



#### Regression Analyses ####
library("moments")
library("sjPlot")

### Reference groups
eu494$vaccine_safe_2cat <- relevel(eu494$vaccine_safe_2cat, ref = "No")
eu494$vaccine_effective_2cat <- relevel(eu494$vaccine_effective_2cat, ref = "No")
eu494$handle_vaccination_government_2cat <- relevel(eu494$handle_vaccination_government_2cat, ref = "Bad")
eu494$date_vacination_3cat <- relevel(eu494$date_vacination_3cat, ref = "Not willing to be vaccinated")
eu494$predclass_4 <- relevel(eu494$predclass_4, ref = "3")
eu494$predclass_3 <- relevel(eu494$predclass_3, ref = "2")

eu494$Gender <- relevel(eu494$Gender, ref = "Male")
eu494$Educ_5cat <- relevel(eu494$Educ_5cat, ref = "Up to 15 years")
eu494$Type_community <- relevel(eu494$Type_community, ref = "Rural area or village")
eu494$Employment <- relevel(eu494$Employment, ref = "Not working")
eu494$vaccine_as_child <- relevel(eu494$vaccine_as_child, ref = "No")
eu494$vaccine_as_adult <- relevel(eu494$vaccine_as_adult, ref = "No")

eu494$Govern_health_spending_terciles <- relevel(eu494$Govern_health_spending_terciles, ref = "1") # Low health spending


### AIM 1: multivariate logistic regression analysis to estimate the association between sources of information and beliefs about vaccine safety and vaccine efficacy
## Models
# Safety of vaccines
model11_unadj <- glm(vaccine_safe_2cat ~ predclass_4, data = eu494,
                           family = binomial, na.action = na.exclude)

model11_survey_weighted <- glm(vaccine_safe_2cat ~ predclass_4, data = eu494,
                family = binomial, na.action = na.exclude, weights = w1)

model11_adjusment_cases <- glm(vaccine_safe_2cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont + 
                    vaccine_as_child + vaccine_as_adult + Govern_health_spending_terciles + 
                       confirmed_cases_mean_100k_z + stringency_mean, 
                   data = eu494, family = binomial, na.action = na.exclude, weights = w1)

model11_adjusment_deaths <- glm(vaccine_safe_2cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont + 
                       vaccine_as_child + vaccine_as_adult + Govern_health_spending_terciles + 
                        confirmed_deaths_mean_100k_z + stringency_mean, 
                   data = eu494, family = binomial, na.action = na.exclude, weights = w1)

model11_CBPS <- glm(vaccine_safe_2cat ~ predclass_4,
                         data = eu494, family = quasibinomial, na.action = na.exclude, weights = eu494$CBPS_weights_4LCA)



tab_model(model11_unadj, model11_survey_weighted, model11_adjusment_cases, 
          model11_adjusment_deaths, model11_CBPS,
          digits.re = 3)



# Effectiveness of vaccines
model12_unadj <- glm(vaccine_effective_2cat ~ predclass_4, data = eu494, 
                           family = binomial, na.action = na.exclude)

model12_survey_weighted <- glm(vaccine_effective_2cat ~ predclass_4, data = eu494,
                     family = binomial, na.action = na.exclude, weights = w1)

model12_adjusment_cases <- glm(vaccine_effective_2cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont + 
                        vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                        confirmed_cases_mean_100k_z + stringency_mean, 
                    data = eu494, family = binomial, na.action = na.exclude, weights = w1)

model12_adjusment_deaths <- glm(vaccine_effective_2cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont + 
                        vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                        confirmed_deaths_mean_100k_z + stringency_mean, 
                    data = eu494, family = binomial, na.action = na.exclude, weights = w1)

model12_CBPS <- glm(vaccine_effective_2cat ~ predclass_4, 
                         data = eu494, family = quasibinomial, na.action = na.exclude, weights = eu494$CBPS_weights_4LCA)



tab_model(model12_unadj, model12_survey_weighted, model12_adjusment_cases, 
          model12_adjusment_deaths, model12_CBPS,
          digits.re = 3)



# Pandemic handling of national governments
model13_unadj <- glm(handle_vaccination_government_2cat ~ predclass_4, data = eu494, 
                           family = binomial, na.action = na.exclude)

model13_survey_weighted <- glm(handle_vaccination_government_2cat ~ predclass_4, data = eu494, 
                     family = binomial, na.action = na.exclude, weights = w1)

model13_adjusment_cases <- glm(handle_vaccination_government_2cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont + 
                        vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                        confirmed_cases_mean_100k_z + stringency_mean, 
                    data = eu494, family = binomial, na.action = na.exclude, weights = w1)

model13_adjusment_deaths <- glm(handle_vaccination_government_2cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont + 
                        vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                        confirmed_deaths_mean_100k_z + stringency_mean, 
                    data = eu494, family = binomial, na.action = na.exclude, weights = w1)

model13_CBPS <- glm(handle_vaccination_government_2cat ~ predclass_4, 
                         data = eu494, family = quasibinomial, na.action = na.exclude, weights = eu494$CBPS_weights_4LCA)



tab_model(model13_unadj, model13_survey_weighted, model13_adjusment_cases, 
          model13_adjusment_deaths, model13_CBPS,
          digits.re = 3)




### AIM 2: multinomial logistic regression to estimate the association between sources of information and willingness to be vaccinated/vaccine rates, checking for mediation effects with the safety and efficacy of vaccines, adjusting for the covariates
library("nnet")

model2_unadj <- multinom(date_vacination_3cat ~ predclass_4, data = eu494, na.action = na.exclude)

model2_survey_weighted <- multinom(date_vacination_3cat ~ predclass_4 , data = eu494, na.action = na.exclude, weights = w1)

model2_adjusment_cases <- multinom(date_vacination_3cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont +
                           vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                            confirmed_cases_mean_100k_z + stringency_mean, 
                       data = eu494, na.action = na.exclude, weights = w1)

model2_adjusment_deaths <- multinom(date_vacination_3cat ~ predclass_4 + Age + Gender + Educ_5cat + Employment + Type_community + Household_size_cont +
                           vaccine_as_child + vaccine_as_adult  + Govern_health_spending_terciles + 
                            confirmed_deaths_mean_100k_z + stringency_mean, 
                       data = eu494, na.action = na.exclude, weights = w1)

tab_model(model2_unadj, digits.re = 3)
tab_model(model2_survey_weighted, digits.re = 3)
tab_model(model2_adjusment_cases, digits.re = 3)
tab_model(model2_adjusment_deaths, digits.re = 3)


model2_CBPS_1 <- glm(date_vacination_3cat ~ predclass_4, 
                          data=subset(eu494, date_vacination_3cat =="Not willing to be vaccinated" | date_vacination_3cat =="Some time later"),
                          family = quasibinomial, na.action = na.exclude, weights = CBPS_weights_4LCA)

model2_CBPS_2 <- glm(date_vacination_3cat ~ predclass_4, 
                          data=subset(eu494, date_vacination_3cat =="Not willing to be vaccinated" | date_vacination_3cat =="Willing to be vaccinated"),
                          family = quasibinomial, na.action = na.exclude, weights = CBPS_weights_4LCA)

tab_model(model2_CBPS_1, digits.re = 3)
tab_model(model2_CBPS_2, digits.re = 3)




#### Structural Equation Modelling Analyses and Mediation analyses with lavaan ####
library(lavaan)

# Creating dummy variables for 4-LCA groups variable
eu494$predclass_4_group1 <- ifelse(eu494$predclass_4 == "1", 1, 0)
eu494$predclass_4_group2 <- ifelse(eu494$predclass_4 == "2", 1, 0)
eu494$predclass_4_group4 <- ifelse(eu494$predclass_4 == "4", 1, 0)

eu494$vaccine_safe_cont_2 [ eu494$vaccine_safe_2cat  == "No" ] <- 0
eu494$vaccine_safe_cont_2 [ eu494$vaccine_safe_2cat  == "Yes" ] <- 1

eu494$vaccine_effective_cont_2 [ eu494$vaccine_effective_2cat  == "No" ] <- 0
eu494$vaccine_effective_cont_2 [ eu494$vaccine_effective_2cat  == "Yes" ] <- 1

eu494$handle_vaccination_government_cont_2 [ eu494$handle_vaccination_government_2cat  == "Bad" ] <- 0
eu494$handle_vaccination_government_cont_2 [ eu494$handle_vaccination_government_2cat  == "Good" ] <- 1

eu494$date_vacination_3cat_cont [ eu494$date_vacination_3cat  == "Not willing to be vaccinated" ] <- 0
eu494$date_vacination_3cat_cont [ eu494$date_vacination_3cat  == "Some time later" ] <- 1
eu494$date_vacination_3cat_cont [ eu494$date_vacination_3cat  == "Willing to be vaccinated" ] <- 2


eu494_2 <- eu494[complete.cases(eu494$predclass_4), ]
eu494_2 <- eu494_2[complete.cases(eu494_2$vaccine_safe_cont_2), ]
eu494_2 <- eu494_2[complete.cases(eu494_2$vaccine_effective_cont_2), ]
eu494_2 <- eu494_2[complete.cases(eu494_2$handle_vaccination_government_cont_2), ]
eu494_2 <- eu494_2[complete.cases(eu494_2$date_vacination_3cat_cont), ]
eu494_2 <- eu494_2[complete.cases(eu494_2$CBPS_weights_4LCA), ]


# Safety of vaccines
SEM_safe <- '#simple mediation
vaccine_safe_2cat ~ a*predclass_4_group1 + d*predclass_4_group2 + e*predclass_4_group4
date_vacination_3cat_cont ~ b*vaccine_safe_2cat
date_vacination_3cat_cont ~ c*predclass_4_group1 + f*predclass_4_group2 + g*predclass_4_group4

#indirect effect
ab:=a*b
db:=d*b
eb:=e*b

#total effect
total_1:=c+(a*b)
total_2:=f+(d*b)
total_3:=g+(e*b)
'

fitmod_safe <- sem(SEM_safe, data = eu494_2, std.ov = T, ordered = c("vaccine_safe_2cat"), sampling.weights = "CBPS_weights_4LCA",
                   se="boot", estimator="WLS")
summary(fitmod_safe, fit.measures=T, rsquare=T, ci=T)




# Effectiveness of vaccines
SEM_effective <- '#simple mediation
vaccine_effective_2cat ~ a*predclass_4_group1 + d*predclass_4_group2 + e*predclass_4_group4
date_vacination_3cat_cont ~ b*vaccine_effective_2cat
date_vacination_3cat_cont ~ c*predclass_4_group1 + f*predclass_4_group2 + g*predclass_4_group4

#indirect effect
ab:=a*b
db:=d*b
eb:=e*b

#total effect
total_1:=c+(a*b)
total_2:=f+(d*b)
total_3:=g+(e*b)'


fitmod_effective <- sem(SEM_effective, data = eu494_2, std.ov = T, ordered = c("vaccine_effective_2cat"), sampling.weights = "CBPS_weights_4LCA",
                        se="boot",estimator="WLS")
summary(fitmod_effective, fit.measures=T, rsquare=T, ci=T)



# Pandemic handling of national governments
SEM_govern <- '#simple mediation
handle_vaccination_government_cont_2 ~ a*predclass_4_group1 + d*predclass_4_group2 + e*predclass_4_group4
date_vacination_3cat_cont ~ b*handle_vaccination_government_cont_2
date_vacination_3cat_cont ~ c*predclass_4_group1 + f*predclass_4_group2 + g*predclass_4_group4

#indirect effect
ab:=a*b
db:=d*b
eb:=e*b

#total effect
total_1:=c+(a*b)
total_2:=f+(d*b)
total_3:=g+(e*b)
'

fitmod_govern <- sem(SEM_govern, data = eu494_2, std.ov = T, ordered = c("handle_vaccination_government_cont_2"), sampling.weights = "CBPS_weights_4LCA",
                     se="boot", estimator="WLS")
summary(fitmod_govern, fit.measures=T, rsquare=T, ci=T)

