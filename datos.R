#######################################
# clase 1 y 2 (token, vinculo R con github)

library(usethis)
library(gitcreds)

usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::use_github()

# usethis::git_sitrep() # indica datos personales del set

#######################################

datos = rnorm(2000)
hist(datos)
summary(datos)

x = y + 1

#######################################
# clase 3





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

glm_spec = logistic_reg() %>% # regresion logistica
  set_engine("glm")           # paquete utilizado
glm_spec

rf_spec = rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("ranger")
rf_spec
  
#######################################
# clase 5

penguin_wf = workflow() %>% 
  add_formula(sex ~ .)
penguin_wf

glm_rs = penguin_wf %>% 
  add_model(glm_spec) %>% 
  fit_resamples(
    resamples = penguin_boot, 
    control = control_resamples(save_pred = TRUE)
    )
glm_rs

rf_rs = penguin_wf %>% 
  add_model(rf_spec) %>% 
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )
rf_rs

collect_metrics(glm_rs)
collect_metrics(rf_rs)

glm_rs %>% 
  conf_mat_resampled()

penguin_final = penguin_wf %>% 
  add_model(glm_spec) %>% 
  last_fit(penguin_split)
penguin_final

collect_metrics(penguin_final)

collect_predictions(penguin_final) %>% 
  conf_mat(sex, .pred_class)

penguin_final$.workflow[[1]] %>% 
  tidy(exponentiate = TRUE)


library(esquisse) # interfaz para ocupar ggplot2

# ejemplo: ----

library(tidytuesdayR)
library(skimr)
library(themis)

library(tidyverse)
library(tidymodels)

# datos ----

tt_data = tidytuesdayR::tt_load(2020, week = 39)
tt_data$members %>% 
  skimr::skim()   # detalle descriptivo sobre variables

climbers_df = tt_data$members %>%  
  select(member_id, peak_name, season, year, sex, age, 
         citizenship,expedition_role, hired, solo, 
         oxygen_used, success, died) %>%  
  filter((!is.na(sex) & !is.na(citizenship) & 
          !is.na(peak_name) & !is.na(expedition_role)) == T) %>%  
  mutate(across(where(~ is.character(.) | is.logical(.)), 
                as.factor))

climbers_df

# Data split ----
set.seed(2023)
climbers_split = initial_split(climbers_df, prop = 0.8, 
                               strata = died)
## Conjunto de entrenamiento ----
train_set = training(climbers_split)

## Conjunto de prueba ----
train_set = testing(climbers_split)

## Control validation ----

climbers_fold = train_set %>% 
  vfold_cv(v = 10, repeats = 1, strata = died)

# Receta ----

mod_recipe = recipe(formula = died ~ ., data = train_set)

mod_recipe = mod_recipe %>% 
  update_role(member_id, new_role = "id") %>% 
  step_impute_median(age) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_other(peak_name, citizenship, expedition_role, 
             threshold = 0.05) %>% 
  step_dummy(all_predictors(), -all_numeric(), one_hot = F) %>% 
  step_upsample(died, over_ratio = 0.2, seed = 2023, 
                skip = T)

## preparación ----
mod_recipe_prepped = prep(mod_recipe, retain = T)

## Bake ----
bake(mod_recipe_prepped, new_data = NULL)

# modelos ----
## Regresion logistica glm ----
log_cls = logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")
log_cls

## Regresion logistica glmnet ----
reg_log_cls = logistic_reg() %>% 
  set_engine("glmnet", family = "binomial") %>% 
  set_mode("classification") %>% 
  set_args(penalty = tune(), mixture = tune())
reg_log_cls

## Workflow ----
cls_wf = workflow() %>% 
  add_recipe(mod_recipe) %>% 
  add_model(reg_log_cls)
cls_wf

## Parametros ----
param_grid = grid_regular(penalty(), mixture(), 
                          levels = c(10, 10))
param_grid





