
# load packages -----------------------------------------------------------
library(here)
library(psych)
library(tidyverse)
library(misinformation) # remotes::install_github("Lingtax/misinformation")
library(gendercoder)
library(tableone)

# read in data ------------------------------------------------------------

df <-  read_csv(here::here("data", "2021_heterodox_test.csv"))
meta <- read_csv(here::here("data", "2021_heterodox_meta.csv"))
# It may be useful to look into the misinformation package and its meta_rename() function
df <- meta_rename(df, meta, name_raw, name_clean)

# do some data cleaning / manipulation ------------------------------------

# things like collating items within scales, & coding the gender item (recommend the gendercoder package)
df <-  df %>% 
  select(-one_of(paste0("beliefs_", c(1, 3:18, 20:22, 24:40))), 
         -starts_with(c("npi_", "bdw_"))) #%>% 
  #mutate(gender = gendercoder::recode_gender(gender, dictionary = many_levels_en)),
        #homophily = (avoid01 + avoid02 + avoid03)/3)

CreateTableOne(vars = c("age", "gender", "education"), factorVars = c("gender", "education"), data = df)
# analyse -----------------------------------------------------------------

df %>% 
  select(starts_with("bdw_")) %>% 
  omega()


# draw figures ------------------------------------------------------------


