# Week 2/3: Initial Data Exploration ====
# Author: [Dayna Arbon]
# Date: [08/02/26]

# Load packages ====
library("tidyverse")
library("here")
library("naniar")
library("janitor")
library("skimr")
library("performance")
library("lmtest")
library("see")
library("emmeans")
# Load data ====
cuckoo_raw <- read_csv(here( "data", "cuckoo.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(cuckoo_raw)
summary(cuckoo_raw)
skim(cuckoo_raw)

# no missing data

# React table====
# view interactive table of data
view(cuckoo_raw)
head(cuckoo_raw)

# Plotting linear graph====
ggplot(cuckoo_raw, aes(x = mass, y = beg, colour = species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()

# Fit a linear model with interaction====
  
  cuckoo_lm <- lm(beg ~ mass * species, data = cuckoo_raw)

summary(cuckoo_lm)

check_model(cuckoo_lm, detrend = FALSE)
# Error in loadNamespace(x) : there is no package called ‘see’
# installed package "see" at top.

check_model(cuckoo_lm, detrend = FALSE)

# The residuals vs fitted plot shows clear heteroscedasticity—variance increases with fitted values. 
# The Q-Q plot shows deviation from normality. 
# The model predicts negative begging calls for small nestlings

# Generate predictions====
# installed "emmeans" package.
predictions_lm <- emmeans(cuckoo_lm, 
                          specs = ~ mass + species,
                          at = list(mass = seq(0, 40, by = 5))) |>
  as_tibble()

# Check for negative predictions====
predictions_lm |> 
  filter(emmean < 0)

performance::check_model(cuckoo_lm, 
                         detrend = FALSE)

