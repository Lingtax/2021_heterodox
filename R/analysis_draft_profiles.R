
# load packages -----------------------------------------------------------
library(here)
library(psych)
library(tidyverse)
library(tidyLPA)
library(GPArotation)
library(misinformation)
library(gendercoder)

# read in data ------------------------------------------------------------

df <-  read_csv(here::here("data", "2021_heterodox_data.csv"))
meta <- read_csv(here::here("data", "2021_heterodox_meta.csv"))
# It may be useful to look into the misinformation package and its meta_rename() function
df <- meta_rename(df, meta, name_raw, name_clean)

# do some data cleaning / manipulation ------------------------------------

# things like collating items within scales, & coding the gender item (recommend the gendercoder package)
df <-  df %>% 
  mutate(gender = gendercoder::recode_gender(gender, dictionary = "broad_en"),
         npi = (npi_1 + npi_2 + npi_3 + npi_4 + npi_5 + npi_6 + npi_7 + 
           npi_8 + npi_9 + npi_10 + npi_11 + npi_12 + npi_13 + npi_14 + 
           npi_15 + npi_16)
           
           )

# analyse -----------------------------------------------------------------

# Factors
beliefs <- df %>% select(starts_with("beliefs_"))

fa.parallel(beliefs, fa = "fa")
factors3 <-  fa(beliefs, nfactors = 3, rotate = "oblimin") # vary nfactors based on the output of the prior
factors <-  fa(beliefs, nfactors = 5, rotate = "oblimin") # vary nfactors based on the output of the prior
factors
factors3$Structure

df <- cbind.data.frame(df, factors$scores)

# Profiles
test <- df %>% 
  select(MR1:MR3) %>%
  estimate_profiles(1:4, models = c(1:2))
test
plot(test)

#  extract the profile allocation results
out <-test$model_2_class_2$dff
# attach them to the main data frame for subsequent analyses
df <- df %>% cbind.data.frame(
  (out %>% select(Class, CPROB1:CPROB2)))

df %>% 
  group_by(Class) %>% 
  summarise(c1_prob = mean(CPROB1, na.rm=TRUE), c2_prob = mean(CPROB2, na.rm=TRUE))

#plot the pattern of factor scores by profile. I can fix this later.
out %>% 
  filter(!is.na(Class)) %>% 
  pivot_longer(MR1:MR3) %>% 
  mutate(Factor = case_when(
    name == "MR1" ~ "Alternative health beliefs",
    name == "MR2" ~ "Conspiratorial science rejection",
    name == "MR5" ~ "Paranormal beliefs", 	
    name == "MR4" ~ "Eastern health practices", 
    name == "MR3" ~ "Institutional mistrust"
  ), 
  Class = as.factor(ifelse(Class == 1, "Orthodox", "Heterodox"))) %>% 
  ggplot(aes(y = value, x = Factor, fill = Class)) + 
  geom_violin(alpha = 0.5) +
  geom_jitter(alpha = .2) + 
  theme_classic() + 
  ylab("Z-scores") +
  coord_flip()
#+ 
theme(axis.text.x = element_text(angle = 20))

# draw figures ------------------------------------------------------------


