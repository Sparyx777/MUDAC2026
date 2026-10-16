rm(list = ls())

install.packages(c("readxl", "scales"))

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)

snap <- read_xlsx("mn-snap.xlsx")

foodshelves <- read_xlsx("Foodshelf_Q5.xlsx")

foodshelves <- foodshelves %>%
  mutate(County = recode(County, "Saint Louis" = "St. Louis"))

snap <- snap %>%
  mutate(CountyTribeName = recode(CountyTribeName, "Lake of Woods" = "Lake of the Woods"))


foodshelves <- foodshelves %>%
  mutate(County = case_when(
      County %in% c("Hennepin-Minneapolis", "Hennepin-Suburbs") ~ "Hennepin",
      
      County %in% c("Ramsey-Saint Paul", "Ramsey-Suburbs") ~ "Ramsey",
      TRUE ~ County
    )
  )

# Our MN SNAP data contains 4 tribes that are not included in the foodshelves dataset.
# This is a limitation of the data coverage. We removed them because there are no
# corresponding observations within the foodshelves dataset.
snap <- snap %>%
  filter(
    !CountyTribeName %in% c(
      "Mille Lacs Band",
      "Red Lake Nation",
      "White Earth Band",
      "MNPrairie"
    )
  )

# Remove unspecified counties
foodshelves <- foodshelves %>%
  filter(County != "N/A")

# Filter to only include 1/2022 to 7/2026 to match up with data coverage in foodshelves dataset.
snap <- snap %>%
  filter(
    (Year > 2022 & Year < 2026) |
      (Year == 2022 & Month >= 1) |
      (Year == 2026 & Month <= 7)
  )

foodshelves <- foodshelves %>%
  mutate(
    IndividualsTotal = as.numeric(gsub(",", "", IndividualsTotal)),
    HouseholdsTotal  = as.numeric(gsub(",", "", HouseholdsTotal)),
    PoundsTotal      = as.numeric(gsub(",", "", PoundsTotal))
  )

foodshelves_cm <- foodshelves %>%
  group_by(County, year, month) %>%
  summarize(
    IndividualsTotal = sum(IndividualsTotal, na.rm = TRUE),
    HouseholdsTotal = sum(HouseholdsTotal, na.rm = TRUE),
    PoundsTotal = sum(PoundsTotal, na.rm = TRUE),
    .groups = "drop"
  )

food_snap <- foodshelves_cm %>%
  left_join(snap, by = c(
      "County" = "CountyTribeName",
      "year" = "Year",
      "month" = "Month"
    ))

# These four counties were excluded because corresponding observations 
# were available within the data coverage of the SNAP dataset.
setdiff(
  c("Dodge", "Grant", "Steele", "Waseca"),
  unique(snap$CountyTribeName)
)

food_snap <- food_snap %>%
  filter(!is.na(Total_Net))


food_snap <- food_snap %>%
  mutate(
    # Average SNAP benefit
    SNAP_Per_Person =Total_Net / PersonCount,
    SNAP_Per_Case =Total_Net / CaseCount,
    
    # Foodshelf utilization
    FoodShelf_Per_SNAP_Person = IndividualsTotal / PersonCount,
    FoodShelf_Per_SNAP_Case = HouseholdsTotal / CaseCount,
    
    #Foodshelf usage relative to the number of SNAP participants
    FoodShelf_Per_Person = IndividualsTotal / PersonCount
  )

# Color code the counties
# The 7 twin cities counties to show against the other 75
tc_county_colors <- c(
  "Anoka" = "blue",  
  "Carver" = "#FF0800",
  "Dakota" = "#008000",
  "Hennepin" = "#FE7311", #pioneer orange
  "Ramsey" = "yellow",
  "Scott" = "#D8BFD8",
  "Washington" = "#FF00FF"
  )

food_snap <- food_snap %>%
  mutate(County_Group = ifelse(
    County %in% names(tc_county_colors),County,"Other Counties"
    ))

# Does location appear to matter? Yes. We also see that the non-metro areas seem
# to have higher foodshelf usage compared to the metro counties of the Twin Cities.
# There also appears to be a negative relationship between individuals served and
# SNAP benefits received.
ggplot(food_snap, aes(x = SNAP_Per_Person, y = FoodShelf_Per_Person, color = County_Group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_manual(values = c( tc_county_colors, "Other Counties" = "grey80")) +
  scale_y_log10(labels = comma) +
  labs(
    title = "SNAP Benefits and Food Shelf Usage",
    x = "Average SNAP Benefit (Dollars $) per Participant",
    y = "Individuals Served (log scale)",
    color = "County"
  ) + 
  theme_minimal()

county_slopes <- food_snap %>%
  group_by(County, County_Group) %>%
  summarize(Slope = coef(lm(log1p(IndividualsTotal) ~ SNAP_Per_Person))[2], .groups = "drop")

ggplot(county_slopes, aes(x = reorder(County, Slope), y = Slope, fill = County_Group)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c( tc_county_colors, "Other Counties" = "grey80")) +
  labs(
    title = "County-Specific Relationship Between SNAP Benefits and Food Shelf Usage",
    x = "",
    y = "Slope",
    fill = "County"
  ) +
  theme_minimal()

food_snap <- food_snap %>%
  arrange(County, year, month) %>%
  group_by(County) %>%
  mutate(
    SNAP_Per_Person_Change = SNAP_Per_Person - lag(SNAP_Per_Person),
    FoodShelf_Change = IndividualsTotal - lag(IndividualsTotal)
  ) %>%
  ungroup()

ggplot(food_snap,aes(x = SNAP_Per_Person_Change, y = FoodShelf_Change)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm")

# Changes in SNAP benefit levels appear to be driven by statewide 
# changes over time rather than county-to-county differences.
ggplot(food_snap,aes(x = SNAP_Per_Person, y = IndividualsTotal, color = factor(year))) +
  scale_y_log10(labels = comma) +
  geom_point(alpha = 0.5)

ggplot(food_snap, aes(x = SNAP_Per_Person, y = IndividualsTotal, color = factor(year))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10(labels = comma)

#SNAP trends by county
food_snap %>%
  filter(County %in% c(
      "Hennepin",
      "Ramsey",
      "Anoka",
      "Dakota",
      "Washington",
      "Scott",
      "Carver")) %>%
  ggplot(aes(x = SNAP_Per_Person, y = IndividualsTotal, color = County)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
  facet_wrap(~County) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values = tc_county_colors)

fixed_by_county_model <- lm(
  log1p(IndividualsTotal) ~
    SNAP_Per_Person +
    factor(County) +
    factor(year) +
    factor(month),
  data = food_snap
)

# Interpretation:
# A $10 increase in average SNAP benefits per participant is associated with 
# a 1.2% decrease in foodshelf use.
summary(fixed_by_county_model)

