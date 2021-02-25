mcycle <- MASS::mcycle

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
f <- as.formula(
  paste(mcycle$accel,
       paste(mcycle$times, collapse - "+"),
       sep = "~"))
print(f)
lm_mod <- lm(f, data = mcycle)
# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)