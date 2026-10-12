library(tidyverse)
library(dplyr)
library(ggplot2)


FeedMurica <- as.data.frame(MMGdata_MN_Onlyv3)
FeedMurica

foodShelves <- as.data.frame(org_FSStats_org_FSStats)
foodShelves

site_list<- unique(foodShelves$"_id_site")
site_count <- length(site_list)
names(foodShelves)

foodShelves <- rename(foodShelves, Site_Id = "_id_site", CallID = "__ID", Year = "_Year", Month = "_Month", Bank_ID = "_id_bank", YYYYMM = "_YearMonth")
names(foodShelves)

foodShelves |> factor(foodShelves$Site_Id)
foodshelves <- foodshelves |> mutate(site_Id = as.character(site_Id))


clean_foodShelves <- foodShelves |>  drop_na(PoundsPerIndiv)

avg_pounds_per_site <- clean_foodShelves |>  group_by(Site_Id) |>  summarise(average_pounds = mean(PoundsPerIndiv, na.rm = TRUE))

avg_pounds_per_site

ggplot(avg_pounds_per_site, aes(x = Site_Id, y = average_pounds)) +
  geom_point(fill = "blue")+
  labs(x = "Sites", 
       y = "Average Pounds")
max(avg_pounds_per_site$Site_Id)
