library(tidyverse)
library(dplyr)
library(ggplot2)

MMGdata_MN_Onlyv4 <- read.csv("MMGdata_MN_Onlyv4.csv")
org_FS <- read_csv("FoodShelfdatav2.csv")

FeedMurica <- as.data.frame(MMGdata_MN_Onlyv4)
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


foodShelves |> factor(foodShelves$Site_Id)
foodShelves <- foodShelves |> mutate(site_Id = as.character(Site_Id))
foodShelves <- foodShelves |> separate_wider_delim(cols = County, delim = "-", names = c("CO_NAME", "CODivision"), cols_remove = FALSE)


clean_foodShelves <- foodShelves |>  drop_na(PoundsPerIndiv)

avg_pounds_per_site <- clean_foodShelves |>  group_by(site_Id) |>  summarise(average_pounds = mean(PoundsPerIndiv, na.rm = TRUE))

avg_pounds_per_site
max(avg_pounds_per_site$site_Id)

Q1 <- quantile(avg_pounds_per_site$average_pounds, .25)
Q3 <- quantile(avg_pounds_per_site$average_pounds, .75)
IQR_val <= IQR(avg_pounds_per_site$average_pounds)
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

FeedMurica





