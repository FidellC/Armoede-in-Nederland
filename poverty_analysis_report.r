---
title: "Provincial Poverty in the Netherlands"
author: "Timon de Graaf, Mels Dings, Michiel Mast, Sam van Lummel, Stijn, Seppe Naus, Duncan Schröder"
date: "`r Sys.Date()`"
output: pdf_document
---

# Set-up your environment

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

```{r packages}
install.packages("tidyverse")
install.packages("giscoR")
install.packages("sf")
install.packages("viridis")

library(tidyverse)
library(giscoR)
library(sf)
library(viridis)
```

# Part 1 - Identify a Social Problem

## 1.1 Describe the Social Problem

This project investigates trends in poverty across Dutch provinces from 2011 onward, using CBS microdata on low-income households. We analyze both the absolute number and the relative percentage of people in poverty to determine whether provincial disparities are narrowing or widening. This analysis helps identify where poverty is structurally persistent versus where progress is being made.

Poverty remains a serious concern in the Netherlands. National averages hide the fact that provinces like Groningen, Flevoland, and Limburg face disproportionately high poverty rates. This issue affects children particularly hard. Our project aims to provide clearer insights into both geographic and demographic patterns of poverty.

# Part 2 - Data Sourcing

## 2.1 Load in the data

```{r loading_data}
poverty_data <- read_csv("combined_poverty_data.csv")
names(poverty_data)[1] <- "Inkomensgrens"
```

## 2.2 Provide a short summary of the dataset(s)

```{r summary_data}
summary(poverty_data)
head(poverty_data)
```

## 2.3 Describe the type of variables included

- **Inkomensgrens**: Low-income threshold
- **Duur inkomenspositie**: Duration of low-income status
- **Kenmerken van huishoudens**: Type of households
- **Regio's**: Province
- **Perioden**: Year
- **Particuliere huishoudens (x 1 000)**: Number of low-income households
- **Personen (x 1 000)**: Number of people in poverty
- **Minderjarige kinderen (x 1 000)**: Number of children in poverty
- **Relatieve (%)** columns: Percentages of each group in poverty
- **year**: Manually added year variable

# Part 3 - Quantifying

## 3.1 Data Cleaning

```{r cleaning}
poverty_data <- poverty_data %>%
  mutate(across(
    c(`Particuliere huishoudens (x 1 000)`, `Personen (x 1 000)`,
      `Minderjarige kinderen (x 1 000)`, `Particuliere huishoudens relatief (%)`,
      `Personen relatief (%)`, `Minderjarige kinderen relatief (%)`),
    ~ as.numeric(str_replace_all(., ",", "."))
  ))
```

## 3.2 Generate Necessary Variables

```{r new_variables}
poverty_data <- poverty_data %>%
  arrange(`Regio's`, Perioden) %>%
  group_by(`Regio's`) %>%
  mutate(
    poverty_rate_change = `Personen relatief (%)` - lag(`Personen relatief (%)`),
    poverty_ratio_children_to_total = `Minderjarige kinderen relatief (%)` / `Personen relatief (%)`
  ) %>% ungroup()
```

## 3.3 Visualize Temporal Variation

```{r plot1}
ggplot(poverty_data, aes(x = Perioden, y = `Personen relatief (%)`, color = `Regio's`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2011, 2014, 2017)) +
  labs(title = "Poverty Rate Over Time by Province", x = "Year", y = "Poverty Rate (%)") +
  theme_minimal()
```

```{r plot2}
ggplot(poverty_data, aes(x = Perioden, y = poverty_rate_change, color = `Regio's`)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = c(2011, 2014, 2017)) +
  labs(title = "Year-on-Year Change in Poverty Rate", x = "Year", y = "Change (%)") +
  theme_minimal()
```

```{r plot3}
ggplot(poverty_data, aes(x = Perioden, y = poverty_ratio_children_to_total, color = `Regio's`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Child-to-Total Poverty Ratio", x = "Year", y = "Ratio") +
  theme_minimal()
```

## 3.4 Visualize Spatial Variation

```{r map_data}
nl_map <- gisco_get_nuts(country = "NL", nuts_level = 2, year = 2021)
poverty_2017 <- poverty_data %>%
  filter(Perioden == 2017) %>%
  mutate(Regio_clean = case_when(
    `Regio's` == "FryslÃƒÂ¢n (PV)" ~ "Friesland (NL)",
    `Regio's` == "Groningen (PV)" ~ "Groningen",
    `Regio's` == "Drenthe (PV)" ~ "Drenthe",
    `Regio's` == "Overijssel (PV)" ~ "Overijssel",
    `Regio's` == "Flevoland (PV)" ~ "Flevoland",
    `Regio's` == "Gelderland (PV)" ~ "Gelderland",
    `Regio's` == "Utrecht (PV)" ~ "Utrecht",
    `Regio's` == "Noord-Holland (PV)" ~ "Noord-Holland",
    `Regio's` == "Zuid-Holland (PV)" ~ "Zuid-Holland",
    `Regio's` == "Zeeland (PV)" ~ "Zeeland",
    `Regio's` == "Noord-Brabant (PV)" ~ "Noord-Brabant",
    `Regio's` == "Limburg (PV)" ~ "Limburg (NL)",
    TRUE ~ NA_character_
  ))

map_data <- nl_map %>%
  left_join(poverty_2017, by = c("NAME_LATN" = "Regio_clean"))

ggplot(map_data) +
  geom_sf(aes(fill = `Personen relatief (%)`), color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Poverty Rate by Province (2017)", fill = "% in Poverty") +
  theme_minimal()
```

## 3.5 Visualize Sub-Population Variation

```{r barplot_subgroup}
poverty_data %>%
  filter(Perioden == 2017) %>%
  ggplot(aes(x = reorder(`Regio's`, -poverty_ratio_children_to_total), 
             y = poverty_ratio_children_to_total, fill = `Regio's`)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Child-to-Total Poverty Ratio by Province (2017)", x = "Province", y = "Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
```

## 3.6 Event Analysis

```{r event_analysis}
ggplot(poverty_data, aes(x = Perioden, y = `Personen relatief (%)`, color = `Regio's`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") +
  annotate("text", x = 2014, y = max(poverty_data$`Personen relatief (%)`, na.rm = TRUE),
           label = "Post-Austerity", vjust = -0.5, color = "red", angle = 90) +
  labs(title = "Poverty Rate and the 2014 Turning Point", x = "Year", y = "Poverty Rate (%)") +
  theme_minimal()
```

# Part 4 - Discussion

## 4.1 Discuss Your Findings

_<<Full discussion text from assignment report can be pasted here>>_

# Part 5 - Reproducibility

## 5.1 GitHub Repository Link

[GitHub Repository](https://github.com/FidellC/Armoede-in-Nederland/tree/main)

## 5.2 References

_Same as in final report._
