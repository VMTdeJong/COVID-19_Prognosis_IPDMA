remove(list = ls())

library(openxlsx)
library(metafor)
library(metamisc)
library(data.table)
library(mltools)
library(tidyr)

data_file <- "aggregate data/2211_meta.xlsx"
d <- read.xlsx(data_file, "Sheet2")

# leave g empty for calibration slope and calibration in the large. 
# use oecalc() for OE ratio, with g = "log(OE")
AUCs <- cbind(metamisc::ccalc(d$AUC, cstat.cilb = d$`AUC.(2.5.%)`, cstat.ciub = d$`AUC.(97.5.%)`, g = "log(cstat/(1-cstat))"),
              country = factor(d$Country), 
              model = d$Model)

# Remove lines with missing standard errors or extremely large SE
AUCs <- AUCs[(!is.na(AUCs$theta.se)) & AUCs$theta.se < 10^6, ]

AUCs$model <- as.factor(AUCs$model)



# Use dummy encoding / one-hot encoding, which allows a categorical variable
# to be used in a meta-regression.
AUCs_1h <- one_hot(as.data.table(AUCs))
class(AUCs_1h) <- "data.frame"
AUCs_1h <- AUCs_1h[ , which(!colnames(AUCs_1h) %in% c("model_4C", "country_United Kingdom", # Refs are 4c and UK
                                                   "theta", "theta.se", "theta.cilb", 
                                                   "theta.ciub", "theta.source", "theta.se.source"))]

# Fit meta-regression model. under mods we have the country and model
fit_one <- rma.uni(AUCs$theta, sei = AUCs$theta.se, mods = AUCs_1h, verbose = Inf, method = "REML", test = "knha")

# Make a new data set. Each model in each country
all_countries <- sort(unique(AUCs$country))
all_models <- sort(unique(AUCs$model))

country <- rep(all_countries, length(all_models))
model <- rep(all_models, each = length(all_countries))

# Same one hot encoding as before
new_countries_1h <- one_hot(as.data.table(country))
new_models_1h <- one_hot(as.data.table(model))
new_data <- as.data.frame(cbind(new_models_1h, new_countries_1h))

# Remove 4C and UK columns, as those are the defaults
new_data$`country_United Kingdom` <- NULL
new_data$model_4C <- NULL

# Predict values for each model in each country
# -2 because the transform does not apply to se and we don't need the se
predicted_AUCs <- cbind(
  metamisc:::inv.logit(as.data.frame(predict(fit_one, newmods = as.matrix(new_data)))[ ,-2]))
predicted_AUCs <- round(predicted_AUCs, 2)

predicted_AUCs_df <- data.frame("AUC" = 
                                  paste0(format(predicted_AUCs[, 1], digits = 2), 
                                         "(", 
                                         format(predicted_AUCs[, 2], digits = 2), 
                                         " to ", 
                                         format(predicted_AUCs[, 3], digits = 2), 
                                         ")"),
                                Model = model, 
                                Country = country)

predicted_AUCs_table <- pivot_wider(predicted_AUCs_df, names_from = c(Model), values_from = AUC)

print(predicted_AUCs_table, n = 100)
write.csv(predicted_AUCs_table, file = "meta regression results/predicted_AUCs_table.csv")

