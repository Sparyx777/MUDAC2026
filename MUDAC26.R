library(tidyverse)
library(dplyr)
library(ggplot2)

MMGdata_MN_Onlyv5 <- read.csv("MMGdata_MN_Onlyv5.csv")
org_FS <- read_csv("FoodShelfdatav2.csv")

FeedMurica <- as.data.frame(MMGdata_MN_Onlyv5)
FeedMurica

foodShelves <- as.data.frame(org_FS)
foodShelves

site_list<- unique(foodShelves$"_id_site")
site_count <- length(site_list)
names(foodShelves)

CONames <- as_tibble(foodShelves$County)
CONames

foodShelves <- rename(foodShelves, Site_Id = "_id_site", CallID = "__ID", Year = "_Year", Month = "_Month", Bank_ID = "_id_bank", YYYYMM = "_YearMonth")
names(foodShelves)

foodShelves <- foodShelves |> mutate(site_Id = as.character(Site_Id))
foodShelves <- foodShelves |> separate_wider_delim(cols = County, delim = "-", names = c("CO_NAME", "CODivision"), cols_remove = FALSE)
class(foodShelves$site_Id)
names(foodShelves)

clean_foodShelves <- foodShelves |>  drop_na(PoundsPerIndiv)

avg_pounds_per_site <- clean_foodShelves |>  group_by(site_Id) |>  summarise(average_pounds = mean(PoundsPerIndiv, na.rm = TRUE))

avg_pounds_per_site
max(avg_pounds_per_site$site_Id)

Q1 <- quantile(avg_pounds_per_site$average_pounds, .25)
Q3 <- quantile(avg_pounds_per_site$average_pounds, .75)
IQR_val <- IQR(avg_pounds_per_site$average_pounds)
avg_pounds_per_site <- avg_pounds_per_site |>  
  filter(average_pounds >= (quantile(average_pounds, 0.25)-1.5*IQR(average_pounds)) & 
           average_pounds <= (quantile(average_pounds, 0.75)+1.5 * IQR(average_pounds)))


ggplot(avg_pounds_per_site, aes(x = site_Id, y = average_pounds, fill = site_Id)) +
  geom_bar(stat = "identity")+
   labs(x = "Sites", 
       y = "Average Pounds")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

LBS_PER_MEAL = 1.2 #https://www.feedingamerica.org/ways-to-give/faq/about-our-claims

MMG2024 <- read.csv("MMGdata_MN_Only2024 (1).csv")
MMG2024v2 <- as.data.frame(MMG2024)
MMG2024v2

colnames(FeedMurica)
colnames(MMG2024v2)

MMG2024v2 <- MMG2024v2 |> 
  rename("weekly.pounds.of.food.needed" = Weekly.pounds,
         "X..of.Meals.needed.by.FI" = X..of.Meals,
         Month.LBS.FD.per.Individual.Short =
           Month.LBS.FD.Per.Individual.Short)

FeedMurica <- bind_rows(FeedMurica, MMG2024v2)


FeedMurica <- FeedMurica |> 
  rename("IndividualsReg" = X..of.Food.Insecure.Persons.Overall, "PoundsReg" = Month.LBS.FD.per.Individual.Short)
FeedMurica

MNCOHH <- read.csv("MNCounty_HH_Totals.csv") #Minnesota Counties Households = MN CO HH = MNCOHH, 2024-2019 data (5 year estimates)
MNCOHH

MNCOHH <- MNCOHH |> 
  rename("CO_Name" = County)

FeedMurica_MNCOHH <- FeedMurica |> 
  left_join(MNCOHH, by = "CO_Name")

