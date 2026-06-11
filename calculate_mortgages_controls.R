library(dplyr)

# Load the dataset (without setting any bandwidth)
mortgages <- causaldata::mortgages

# 1. Prepare the variables exactly like in the tutorial
mortgages_c <- 0

mortgages <- mortgages %>%
  mutate(
    x = qob_minus_kw - mortgages_c,
    above = ifelse(x >= 0, 1, 0),
    x_right = above * x
  ) %>%
  rename(D = 4) %>%
  mutate(x_treat = D * x)

# 2. Run the first-stage regressions using lm (adding nonwhite and factor(bpl) for state fixed effects)
mortgages_firststage_D <- lm(D ~ above + x + x_right + nonwhite + factor(bpl), data = mortgages)
mortgages_firststage_x_treat <- lm(x_treat ~ above + x + x_right + nonwhite + factor(bpl), data = mortgages)

# 3. Create a dataset with the predicted values for the second stage
mortgages_IVpredicted <- data.frame(
  D = predict(mortgages_firststage_D),
  x = mortgages$x,
  x_treat = predict(mortgages_firststage_x_treat),
  home_ownership = mortgages$home_ownership,
  nonwhite = mortgages$nonwhite,
  bpl = factor(mortgages$bpl)
)

# 4. Run the second-stage regression using lm
mortgages_secondstage <- lm(home_ownership ~ D + x + x_treat + nonwhite + bpl, data = mortgages_IVpredicted)

# 5. View the summary to find the point estimate for D
print(summary(mortgages_secondstage))
