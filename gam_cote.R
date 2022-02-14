# remotes::install_github("NOAA-EDAB/ecodata")
# remotes::install_github("James-Thorson-NOAA/mvtweedie")
# remotes::install_github("gavinsimpson/gratia")
# install.packages("readxl")

library(ggplot2)
library(mgcv)
library(mvtweedie)
library(dplyr)
library(gratia)
library(broom)


## Now with Common Terns
sst_dat <- readxl::read_excel("data/GOM_SST_1982_to_2021 - Vincent Saba - NOAA Federal.xlsx") %>% 
  tidyr::separate(t, c("Year", "month", "day"), sep = "-") %>% 
  group_by(Year) %>% 
  summarize(SST = mean(temp)) %>% 
  filter(Year >= 1988,
         Year <= 2021) %>% 
  mutate(Year = as.numeric(Year))

prey_freq <- ecodata::seabird_ne %>% 
  dplyr::filter(!stringr::str_detect(Var, "Productivity"),
                !stringr::str_detect(Var, "Sum")) %>% 
  tidyr::separate(Var,c("Island", "COTE", "Spp", "Extra"), sep = " ") %>% 
  mutate(Diet = paste(Spp, Extra, sep = " ")) %>% 
  mutate(Diet = gsub("NA", "", Diet),
         group = as.factor(Diet),
         site = as.factor(Island)) %>% 
  select(Year = Time,
         Response = Value,
         group,
         site) %>% 
  left_join(sst_dat)

# ggplot(prey_freq, aes(x = Year, y = Response, group = group)) +
#   geom_point(aes(color = group)) +
#   geom_line(aes(color = group)) +
#   facet_wrap(~site)

# Fit GAM

## from Jim T. 
# yeah, formula = group would give a global intercept and then all groups relative to the reference level
# whereas formula = 0 + group is identical to formula = group - 1, and expresses the same model but with redefined coefficients, i.e., the intercept for each group
# so its just a style thing really, same model, but with covariates expressed differently

cote_sst = mgcv::gam(formula = Response ~ 0 + group + s(Year, by = group, bs = "gp") + SST:group, 
                data = prey_freq, 
                family = "tw")
AIC(cote_sst)

cote_sst_isl = mgcv::gam(formula = Response ~ 0 + group + site + s(Year, by = group, bs = "gp") + SST:group, 
                     data = prey_freq, 
                     family = "tw")
AIC(cote_sst_isl)

# 
# cote_sst_isl = mgcv::gam(formula = Response ~ 0 + group + site + s(Year, by = site, bs = "gp") + s(Year, by = group, bs = "gp") + SST:group, 
#                          data = prey_freq, 
#                          family = "tw")


summary(cote_sst_isl)
draw(cote_sst_isl)

class(cote_sst) = c( "mvtweedie", class(cote_sst))


# Predict values
gom_newdata = expand.grid("group" = levels(prey_freq$group), 
                          # "site" = levels(prey_freq$site),
                          "Year" = min(prey_freq$Year):max(prey_freq$Year)) %>% 
  left_join(sst_dat)


gom_pred = predict(cote_sst,
                   se.fit = TRUE,
                   category_name = "group",
                   origdata = data.frame(prey_freq),
                   newdata = gom_newdata)

gom_newdata = cbind(gom_newdata, fit = gom_pred$fit, se.fit = gom_pred$se.fit )
gom_newdata$lower = gom_newdata$fit - gom_newdata$se.fit
gom_newdata$upper = gom_newdata$fit + gom_newdata$se.fit
# 
# gom_newdata_sst = expand.grid("group" = levels(prey_freq$group),
#                               "Year" = min(prey_freq$Year):max(prey_freq$Year)) %>%
#   left_join(sst_dat)
# 
# 
# gom_pred_sst = predict(gom_fit_sst,
#                    se.fit = TRUE,
#                    category_name = "group",
#                    origdata = data.frame(prey_freq),
#                    newdata = gom_newdata_sst)
# 
# gom_newdata_sst = cbind(gom_newdata_sst, fit = gom_pred_sst$fit, se.fit = gom_pred_sst$se.fit )
# gom_newdata_sst$lower = gom_newdata_sst$fit - gom_newdata_sst$se.fit
# gom_newdata_sst$upper = gom_newdata_sst$fit + gom_newdata_sst$se.fit

# Plot
theme_set(theme_bw())
ggplot(gom_newdata, aes(Year, fit, color = SST)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(group)) +
  scale_color_viridis_c(name = "SST") +
  ylim(0,1) +
  labs(y="Predicted proportion")

# Effects plot
effects_table <- tidy(cote_sst, parametric = TRUE) %>%
  filter(grepl("SST", term)) %>%
  mutate(across(term, ~gsub("(group|:SST_mean)","", .))) %>%
  mutate(across(term, ~reorder(factor(.), estimate)))

ggplot(effects_table, aes(y=term, x = estimate, xmin = estimate-2*std.error, xmax = estimate+2*std.error)) +
  geom_pointrange() +
  geom_vline(xintercept=0, lty =2)

