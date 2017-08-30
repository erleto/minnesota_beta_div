library(nlme)

# Create df containing response variable, explanatory variables and coordinates 
# of polygon centroids
tax_model_variables <- dplyr::inner_join(units_merged_beta_subset@data[c('unit', 
  'tax_beta_div')], explanatory_variables)
tax_model_variables$x <- coordinates(units_merged_beta_subset)[, 1]
tax_model_variables$y <- coordinates(units_merged_beta_subset)[, 2]

# Original linear model
#tax_model <- lm(tax_beta_div ~ human_footprint + 
#    forest_loss + 
#    net_prim_prod + 
#    habitat_div + 
#    #mean_temp + 
#    #other_roads + 
#    #mean_prec + 
#    forest_point_freq, 
#  data = tax_model_variables)
summary(tax_model)
units_merged_beta_subset$tax_div_resid <- tax_model$residuals
car::vif(tax_model)

# Check for spatial autocorrelation
moran_mc_func(units_merged_beta_subset, tax_div_resid, 1, 999)
plot(gstat::variogram(tax_div_resid ~ 1, locations = coordinates(units_merged_beta_subset), 
  data = units_merged_beta_subset, cloud = FALSE, cutoff = 200000), type = "b")
moran_plot_func(units_merged_beta_subset, tax_div_resid, 1)

# Define model variables
f1 <- formula(tax_beta_div ~ human_footprint + 
    forest_loss + 
    net_prim_prod + 
    habitat_div + 
    forest_point_freq)
# Fitbasic linear model for comparisson
b1 <- gls(f1, data = tax_model_variables, method = 'ML')
summary(b1)
car::vif(b1)
# Fit linear models with varying spatial structures
b1a <- gls(f1, correlation = corSpher(form = ~ x + y, nugget = FALSE), method = 'ML', data = tax_model_variables)
b1b <- gls(f1, correlation = corLin(form = ~ x + y, nugget = FALSE), method = 'ML', data = tax_model_variables)
b1c <- gls(f1, correlation = corRatio(form = ~ x + y, nugget = FALSE), method = 'ML', data = tax_model_variables)
b1d <- gls(f1, correlation = corGaus(form = ~ x + y, nugget = FALSE), method = 'ML', data = tax_model_variables)
b1e <- gls(f1, correlation = corExp(form = ~ x + y, nugget = FALSE), method = 'ML', data = tax_model_variables)
# Compare AIC's of the different models
AIC(b1, b1a, b1b, b1c, b1d, b1e)

vario_base <- Variogram(b1, form = ~x + y, resType = "pearson", maxDist = 250000)
plot(vario_base, smooth = TRUE, ylim = c(0, 1.3))
vario_b1 <- Variogram(b1e, form = ~x + y, resType = "pearson", maxDist = 250000)
plot(vario_b1, smooth = FALSE, ylim = c(0, 1.3))
vario_b1_norm <- Variogram(b1e, form = ~x + y, resType = "normalized", maxDist = 250000)
plot(vario_b1_norm, smooth = FALSE, ylim = c(0, 1.3))

# Check spatial autocorrelation of model residuals
units_merged_beta_subset$tax_div_resid <- b1e$residuals
moran_mc_func(units_merged_beta_subset, tax_div_resid, 1, 999)
plot(gstat::variogram(tax_div_resid ~ 1, locations = coordinates(units_merged_beta_subset), 
  data = units_merged_beta_subset, cloud = FALSE, cutoff = 100000), type = "b")
moran_plot_func(units_merged_beta_subset, tax_div_resid, 1)

aaa <- tax_model_variables
coordinates(aaa) <- c("x", "y")

# Compute the sample variogram of our data (without covariate)
vario <- variogram(tax_beta_div ~ 1, aaa)
plot(vario)

counts.ivgm <- gstat::vgm(model = "Exp", nugget = 9, range = 100000, psill = 10)
counts.vgm <- fit.variogram(vario, model = counts.ivgm)
counts.vgm
plot(vario, counts.vgm, pch = "+", main = "Tax_beta_div", cex = 2)

m <- lm(tax_beta_div ~ human_footprint + 
    forest_loss + 
    net_prim_prod + 
    habitat_div + 
    forest_point_freq, 
  data = aaa)
# Compute the sample variogram of the residuals: residual spatial
# autocorrelation
vario.rs <- variogram(residuals(m) ~ 1, data = aaa)

# Fit a theoretical variogram model to the residuals, and give initial
# values
counts.rivgm <- gstat::vgm(model = "Sph", nugget = 6, range = 50000, psill = 8)
counts.rvgm <- fit.variogram(vario.rs, model = counts.rivgm)
counts.rvgm
plot(vario.rs, counts.rvgm, pc = "+", main = "Residuals", cex = 2)

# Try using the autoKrieg function from the automap package to estimate the form 
# of the spatial autocorrelation
library(automap)
bbb <- autoKrige(tax_beta_div ~ human_footprint + 
    forest_loss + 
    net_prim_prod + 
    habitat_div + 
    forest_point_freq, aaa, aaa)
plot(bbb)

library(spgwr)
tax_model_variables <- merge(units_merged_beta_subset[c('unit', 
  'tax_beta_div')], explanatory_variables)
gbwG <- spgwr::ggwr.sel(f1, data = tax_model_variables, family = "Gaussian", gweight = gwr.Gauss, verbose = FALSE)
ggwrG <- spgwr::ggwr(f1, data = tax_model_variables, family = "gaussian", bandwidth = gbwG, gweight = gwr.Gauss, se.fit = FALSE)
