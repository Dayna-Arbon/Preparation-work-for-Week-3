# Week 2/3: Initial Data Exploration ====
# Author: [Dayna Arbon]
# Date: [08/02/26]

# Load packages ====
library("tidyverse")
library("here")
library("naniar")
library("janitor")
library("skimr")
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


