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
slopes <- data.frame(theta = d$Slope, 
                 cilb = d$`Slope.(2.5%)`, 
                 ciub = d$`Slope.(97.5%)`,
                 country = factor(d$Country),
                 model = d$Model)
slopes$theta.se <- ((slopes$theta - slopes$cilb) / qnorm(.975) + (slopes$theta - slopes$ciub) / qnorm(.025) ) / 2
slopes$cilb <- NULL
slopes$ciub <- NULL

# Remove lines with missing standard errors or extremely large SE
slopes <- slopes[(!is.na(slopes$theta.se)) & slopes$theta.se < 10^6 & (!is.na(slopes$theta)), ]

# Make sure the same name is used for a model or country in all validations
slopes$model[slopes$model == "Bello-Chavolla (new patients)"] <- "Bello-Chavolla"
slopes$model <- as.factor(slopes$model)

# Use dummy encoding / one-hot encoding, which allows a categorical variable
# to be used in a meta-regression.
slopes_1h <- one_hot(as.data.table(slopes))
class(slopes_1h) <- "data.frame"
slopes_1h <- slopes_1h[ , which(!colnames(slopes_1h) %in% c("model_4C", "country_United Kingdom", # Refs are 4c and UK
                                                   "theta", "theta.se", "theta.cilb", 
                                                   "theta.ciub", "theta.source", "theta.se.source"))]

# Fit meta-regression model. under mods we have the country and model
fit_one <- rma.uni(slopes$theta, sei = slopes$theta.se, mods = slopes_1h, verbose = Inf, method = "REML", test = "knha")

# Make a new data set. Each model in each country
all_countries <- sort(unique(slopes$country))
all_models <- sort(unique(slopes$model))

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
# -2 because the transform dslopes not apply to se and we don't need the se
predicted_slopes <- cbind(as.data.frame(predict(fit_one, newmods = as.matrix(new_data)))[ ,-2])
predicted_slopes <- round(predicted_slopes, 2)

predicted_slopes_df <- data.frame("est" = 
                                 paste0(predicted_slopes[, 1], 
                                        "(", 
                                        predicted_slopes[, 2], 
                                        " to ", 
                                        predicted_slopes[, 3], 
                                        ")"),
                               Model = model, 
                               Country = country)

predicted_slopes_table <- pivot_wider(predicted_slopes_df, names_from = c(Model), values_from = est)

print(predicted_slopes_table, n = 100)
write.csv(predicted_slopes_table, file = "meta regression results/predicted_slopes_table.csv")



