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
cil <- data.frame(theta = d$Intercept, 
                     cilb = d$`Intercept.(2.5%)`, 
                     ciub = d$`Intercept.(97.5%)`,
                     country = factor(d$Country),
                     model = d$Model)
cil$theta.se <- ((cil$theta - cil$cilb) / qnorm(.975) + (cil$theta - cil$ciub) / qnorm(.025) ) / 2
cil$cilb <- NULL
cil$ciub <- NULL

# Remove lines with missing standard errors or extremely large SE
cil <- cil[(!is.na(cil$theta.se)) & cil$theta.se < 10^6 & (!is.na(cil$theta)), ]

# Make sure the same name is used for a model or country in all validations
cil$model[cil$model == "Bello-Chavolla (new patients)"] <- "Bello-Chavolla"
cil$model <- as.factor(cil$model)

# Use dummy encoding / one-hot encoding, which allows a categorical variable
# to be used in a meta-regression.
cil_1h <- one_hot(as.data.table(cil))
class(cil_1h) <- "data.frame"
cil_1h <- cil_1h[ , which(!colnames(cil_1h) %in% c("model_4C", "country_United Kingdom", # Refs are 4c and UK
                                                            "theta", "theta.se", "theta.cilb", 
                                                            "theta.ciub", "theta.source", "theta.se.source"))]

# Fit meta-regression model. under mods we have the country and model
fit_one <- rma.uni(cil$theta, sei = cil$theta.se, mods = cil_1h, verbose = Inf, method = "REML", test = "knha")

# Make a new data set. Each model in each country
all_countries <- sort(unique(cil$country))
all_models <- sort(unique(cil$model))

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
# -2 because the transform dcil not apply to se and we don't need the se
predicted_cil <- cbind(as.data.frame(predict(fit_one, newmods = as.matrix(new_data)))[ ,-2])
predicted_cil <- round(predicted_cil, 2)

predicted_cil_df <- data.frame("est" = 
                                    paste0(format(predicted_cil[, 1], digits = 2), 
                                           "(", 
                                           format(predicted_cil[, 2], digits = 2), 
                                           " to ", 
                                           format(predicted_cil[, 3], digits = 2), 
                                           ")"),
                                  Model = model, 
                                  Country = country)

predicted_cil_table <- pivot_wider(predicted_cil_df, names_from = c(Model), values_from = est)

print(predicted_cil_table, n = 100)
write.csv(predicted_cil_table, file = "meta regression results/predicted_cil_table.csv")



