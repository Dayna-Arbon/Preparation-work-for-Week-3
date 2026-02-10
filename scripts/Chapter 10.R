# Week 2/3: Initial Data Exploration ====
# Author: [Dayna Arbon]
# Date: [08/02/26]
# How does nestling mass affect begging rates between the different species?

# Load packages ====
library("gitcreds")
library("usethis")
library("tidyverse")
library("here")
library("naniar")
library("janitor")
library("skimr")
library("performance")
library("lmtest")
library("see")
library("emmeans")
library("MASS") 
library("broom")
library("dplyr")
# Load data ====
cuckoo_raw <- read_csv(here( "data", "cuckoo.csv"))

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
ggplot(cuckoo_raw, aes(x = Mass, y = Beg, colour = Species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()

# Fit a linear model with interaction====
  
  cuckoo_lm <- lm(Beg ~ Mass * Species, data = cuckoo_raw)

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
                          specs = ~ Mass + Species,
                          at = list(Mass = seq(0, 40, by = 5))) |>
  as_tibble()

# Check for negative predictions====
predictions_lm |> 
  filter(emmean < 0)

performance::check_model(cuckoo_lm, 
                         detrend = FALSE)


# additive model====

cuckoo_glm_add <- glm(Beg ~ Mass + Species, 
                      data = cuckoo_raw, 
                      family = poisson(link = "log"))

summary(cuckoo_glm_add)

# generate predictions ====

predictions_poisson <- emmeans(cuckoo_glm_add,
                               specs = ~ Mass + Species,
                               at = list(Mass = seq(0, 40, by = 5)),
                               type = "response") |>
  as_tibble()

ggplot(predictions_poisson, aes(x = Mass, y = rate, colour = Species)) +
  geom_line(linewidth = 1) +
  geom_point(data = cuckoo_raw, aes(y = Beg), alpha = 0.5) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds",
       title = "Poisson GLM: Additive model") +
  theme_minimal()

#  No negative predictions, Variance increases with mean. 

check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE)
# Q-Q plot: Are deviance residuals approximately normal? ====

check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE,
            check = "qq")

# Residuals vs fitted: Is there remaining pattern? =====

check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE,
            check = "homogeneity")

# Dispersion check: What does this plot show? =====

check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE,
            check = "overdispersion")

# Calculate dispersion manually ====

dispersion_add <- cuckoo_glm_add$deviance / cuckoo_glm_add$df.residual
dispersion_add

# [1] 9.225245

# performance model check ====
check_overdispersion(cuckoo_glm_add)

# Overdispersion detected.
# The dispersion ratio is [~7-9]

# Hypothesis: The interaction is missing ====

ggplot(cuckoo_raw, aes(x = Mass, y = Beg, colour = Species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()
# Warbler calling rate increases more slowly with mass than cuckoo rate.

# Fit the model with Mass × Species interaction ====

cuckoo_glm_int <- glm(Beg ~ Mass * Species, 
                      data = cuckoo_raw, 
                      family = poisson(link = "log"))

summary(cuckoo_glm_int)

# Compare dispersion between models:

dispersion_comparison <- tibble(
  Model = c("Additive (no interaction)", 
            "Interaction included"),
  Dispersion = c(
    cuckoo_glm_add$deviance / cuckoo_glm_add$df.residual,
    cuckoo_glm_int$deviance / cuckoo_glm_int$df.residual))

dispersion_comparison

# Additive (no interaction)       9.23
# Interaction included            9.28

# Has dispersion reduced?
check_overdispersion(cuckoo_glm_int)
# Overdispersion detected.

# Dispersion has not meaningfully reduced. 
# Adding the biologically meaningful interaction has captured substantial variance by better modelling the mean structure.

# Key insight: Much “overdispersion” is actually model misspecification. 
# Always investigate whether: 1. Important covariates are missing 2. 
# Relationships are non-linear (consider polynomials) 3. Interactions exist between predictors

# However, residual overdispersion remains (φ̂ ≈ [1.5-2]), suggesting variance still exceeds the mean even with correct mean specification.

# Comparing model predictions ====

anova(cuckoo_glm_add, cuckoo_glm_int)

# Generate predictions for both models
pred_additive <- emmeans(cuckoo_glm_add,
                         specs = ~ Mass + Species,
                         at = list(Mass = seq(0, 40, by = 1)),
                         type = "response") |>
  as_tibble() |>
  mutate(Model = "Additive")

pred_interaction <- emmeans(cuckoo_glm_int,
                            specs = ~ Mass + Species,
                            at = list(Mass = seq(0, 40, by = 1)),
                            type = "response") |>
  as_tibble() |>
  mutate(Model = "Interaction")

predictions_combined <- bind_rows(pred_additive, pred_interaction)

ggplot(predictions_combined, aes(x = Mass, y = rate, colour = Species)) +
  geom_point(data = cuckoo_raw, aes(y = Beg), alpha = 0.4) +
  geom_line(aes(linetype = Model), linewidth = 1) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)",
       y = "Begging calls per 6 seconds",
       title = "Model comparison: Additive vs Interaction") +
  theme_minimal() +
  theme(legend.position = "right")+
  facet_wrap(~Model)


# Addressing remaining model checks ====

check_model(cuckoo_glm_int, 
            residual_type = "normal",
            detrend = FALSE)

# Quasi-Poisson:====
  
  cuckoo_quasi <- glm(Beg ~ Mass * Species, 
                      data = cuckoo_raw, 
                      family = quasipoisson(link = "log"))

summary(cuckoo_quasi)

# The quasilikelihood model just adds a dispersion term to our SE so model fits do not change

# Negative binomial:====
  
cuckoo_negbin <- glm.nb(Beg ~ Mass * Species, 
                        data = cuckoo_raw)

summary(cuckoo_negbin)

# AIC ====
AIC(cuckoo_glm_int, cuckoo_negbin)
# cuckoo_glm_int  4 615.8282
# cuckoo_negbin   5 357.2955

check_model(cuckoo_negbin, detrend = FALSE) 


# Extract coefficients with confidence intervals

coef_poisson <- tidy(cuckoo_glm_int, conf.int = TRUE) |>
  mutate(Model = "Poisson")

coef_quasi <- tidy(cuckoo_quasi, conf.int = TRUE) |>
  mutate(Model = "Quasi-Poisson")

coef_negbin <- tidy(cuckoo_negbin, conf.int = TRUE) |>
  mutate(Model = "Negative Binomial")

coef_comparison <- bind_rows(coef_poisson, coef_quasi, coef_negbin) |>
  filter(term == "Mass:SpeciesWarbler") |>  
  dplyr::select(Model, estimate, std.error, conf.low, conf.high, p.value)

coef_comparison

# Visualise the uncertainty:

ggplot(coef_comparison, aes(x = Model, y = estimate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             colour = "red", linewidth = 1) +
  labs(y = "Interaction coefficient (Mass:SpeciesWarbler)",
       x = "",
       title = "Same data, different inferences") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank()) +
  coord_flip()

# The problem: significance depends on model choice.

coef_comparison |>
  mutate(
    Significant = if_else(p.value < 0.05, "Yes", "No"),
    `CI crosses zero` = if_else(conf.low < 0 & conf.high > 0, "Yes", "No")
  ) |>
  dplyr::select(Model, estimate, p.value, Significant, `CI crosses zero`)


## Final model predictions
predictions_final <- emmeans(cuckoo_quasi,
                             specs = ~ Mass + Species,
                             at = list(Mass = seq(0, 40, by = 0.5)),
                             type = "response") |>
  as_tibble()

fig_main <- ggplot(predictions_final,
                   aes(x = Mass, y = rate,
                       colour = Species, fill = Species)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL),
              alpha = 0.15, colour = NA) +
  # Mean estimate
  geom_line(linewidth = 1.2) +
  # Raw data
  geom_point(data = cuckoo_raw,
             aes(y = Beg),
             size = 2.5,
             alpha = 0.6) +
  
  scale_colour_manual(values = c("Cuckoo" = "darkorange", "Warbler" = "steelblue"),
                      labels = c("Cuckoo", "Reed warbler")
                      
  ) +
  scale_fill_manual(values = c("Cuckoo" = "darkorange", "Warbler" = "steelblue"),
                    labels = c("Cuckoo", "Reed warbler")
                    
  ) +
  scale_x_continuous(breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  
  labs(
    x = "Nestling mass (g)",
    y = "Begging calls per 6 seconds",
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12)


fig_main

# Exponentiated coefficients (rate ratios)
tidy(cuckoo_quasi, exponentiate = TRUE, conf.int = TRUE)

# Additive model

cuckoo_quasi_add <- glm(Beg ~ Mass + Species, family = poisson(link = "log"), data = cuckoo_raw)

tidy(cuckoo_quasi_add, exponentiate = TRUE, conf.int = TRUE)

# Predictions at specific mass values
pred_key_masses <- emmeans(cuckoo_quasi,
                           specs = ~ Species + Mass,
                           at = list(Mass = c(10, 40)),
                           type = "response") |>
  as_tibble()

pred_key_masses

# Rate ratios: how much does calling increase per gram?
# For cuckoos: exp(β_Mass)
# For warblers: exp(β_Mass + β_Mass:SpeciesWarbler)

# Formal test of interaction
drop1(cuckoo_quasi, test = "F")

cuckoo_raw |>
  group_by(Species) |>
  summarise(
    n = n(),
    n_zeros = sum(Beg == 0),
    prop_zeros = n_zeros / n,
    pct_zeros = prop_zeros * 100)