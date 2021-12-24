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
OEs <- cbind(metamisc::oecalc(d$`O:E`, OE.cilb = d$`O:E.(2.5.%)`, OE.ciub = d$`O:E.(97.5%)`, g = "log(OE)"),
             country = factor(d$Country),
             model = d$Model)

# Remove lines with missing standard errors or extremely large SE
OEs <- OEs[(!is.na(OEs$theta.se)) & OEs$theta.se < 10^6 & (!is.na(OEs$theta)), ]

# Make sure the same name is used for a model or country in all validations
OEs$model[OEs$model == "Bello-Chavolla (new patients)"] <- "Bello-Chavolla"
OEs$model <- as.factor(OEs$model)



# Remove unused levels (i.e. the alternative spellings)
OEs <- droplevels(OEs)
OEs <- OEs[-which(names(OEs) %in% c("O", "E", "N"))]


# Use dummy encoding / one-hot encoding, which allows a categorical variable
# to be used in a meta-regression.
OEs_1h <- one_hot(as.data.table(OEs))
class(OEs_1h) <- "data.frame"
OEs_1h <- OEs_1h[ , which(!colnames(OEs_1h) %in% c("model_4C", "country_United Kingdom", # Refs are 4c and UK
                                                      "theta", "theta.se", "theta.cilb", 
                                                      "theta.ciub", "theta.source", "theta.se.source"))]

# Fit meta-regression model. under mods we have the country and model
fit_one <- rma.uni(OEs$theta, sei = OEs$theta.se, mods = OEs_1h, verbose = Inf, method = "REML", test = "knha")

# Make a new data set. Each model in each country
all_countries <- sort(unique(OEs$country))
all_models <- sort(unique(OEs$model))

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
predicted_OEs <- cbind(as.data.frame(predict(fit_one, newmods = as.matrix(new_data)))[ ,-2])
predicted_OEs <- round(exp(predicted_OEs), 2)

predicted_OEs_df <- data.frame("Est" = 
                                  paste0(predicted_OEs[, 1], 
                                         "(", 
                                         predicted_OEs[, 2], 
                                         " to ", 
                                         predicted_OEs[, 3], 
                                         ")"),
                                Model = model, 
                                Country = country)

predicted_OEs_table <- pivot_wider(predicted_OEs_df, names_from = c(Model), values_from = Est)

print(predicted_OEs_table[1:6, 1:3])
print(predicted_OEs_table, n = 100)
write.csv(predicted_OEs_table, file = "meta regression results/predicted_OEs_table.csv")



