 
library(usethis)
library(gitcreds)

usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::use_github()

#######################################

datos = rnorm(2000)
hist(datos)
summary(datos)

x = y + 1

#######################################
# clase 4

library(reticulate)
use_python("/home... /anaconda3/bin/python")

py_run_string("def Psq (x):
              value = x*x
              return(value)")
py$PSq(3)

##### R

typeof(...)

# %>% # lo que esta a la izquierda pasa a la derecha

library(tidyverse)
library(palmerpenguins)

penguins
glimpse(penguins)

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)

penguins_df = penguins %>%
  filter(!is.na(sex)) %>% 
  select(-island)

glimpse(penguins_df)


library(tidymodels)
set.seed(123)
penguin_split = initial_split(penguins_df, strata = sex)
penguin_train = training(penguin_split)
penguin_test  = testing(penguin_split)
penguin_split


set.seed(123)
penguin_boot = bootstraps(penguin_train)
penguin_boot


library(parsnip)
#parsnip_addin()   --> definir modelo a traves de interfas

glm_spec = logistic_reg() %>% 
  set_engine("glm")
glm_spec

rf_spec = rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("ranger")
rf_spec
  
  
  
  

