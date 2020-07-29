library(fairmodels)
library(DALEX)
library(ranger)
library(gbm)

data("compas")

# positive outcome - not being recidivist
two_yr_recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
y_numeric <- as.numeric(two_yr_recidivism) -1
compas$Two_yr_Recidivism <- two_yr_recidivism

df <- compas
df$Two_yr_Recidivism <- as.numeric(two_yr_recidivism) -1
gbm_model <- gbm(Two_yr_Recidivism ~., data = df)

lm_model <- glm(Two_yr_Recidivism~.,
                data=compas,
                family=binomial(link="logit"))

rf_model <- ranger(Two_yr_Recidivism ~.,
                   data = compas,
                   probability = TRUE)

explainer_lm  <- explain(lm_model, data = compas[,-1], y = y_numeric)
explainer_rf  <- explain(rf_model, data = compas[,-1], y = y_numeric)
explainer_gbm <- explain(gbm_model, data = compas[,-1], y = y_numeric)

fobject <- fairness_check(explainer_lm, explainer_rf, explainer_gbm,
                          protected = compas$Ethnicity,
                          privileged = "Caucasian")


library(dplyr)

fobject %>% stack_metrics() %>% plot()
fobject %>% fairness_radar() %>% plot()
fobject %>% fairness_heatmap() %>% plot()
fobject %>% fairness_pca() %>% plot()
fobject %>% choose_metric() %>% plot()
fobject %>% group_metric() %>% plot()
fobject %>% performance_and_fairness("STP") %>% plot()
fobject %>% all_cutoffs() %>% plot()
fobject %>% ceteris_paribus_cutoff("African_American") %>% plot()
fobject %>% ceteris_paribus_cutoff("African_American", cumulated = TRUE) %>% plot()











