library(dplyr)

# Load the dataset
mortgages <- causaldata::mortgages

# 1. Filter the dataset to apply the bandwidth of 6 quarters
# (If your result differs slightly from the options, try `< 6` instead of `<= 6`)
mortgages_bw6 <- mortgages %>%  
  filter(abs(qob_minus_kw) <= 6) 

# 2. Prepare the variables exactly like in the tutorial
mortgages_c <- 0

mortgages_bw6 <- mortgages_bw6 %>%
  mutate(
    x = qob_minus_kw - mortgages_c,
    above = ifelse(x >= 0, 1, 0),
    x_right = above * x
  ) %>%
  rename(D = 4) %>%
  mutate(x_treat = D * x)

# 3. Run the first-stage regressions using lm (adding nonwhite and factor(bpl) for state fixed effects)
mortgages_firststage_D <- lm(D ~ above + x + x_right + nonwhite + factor(bpl), data = mortgages_bw6)
mortgages_firststage_x_treat <- lm(x_treat ~ above + x + x_right + nonwhite + factor(bpl), data = mortgages_bw6)

# 4. Create a dataset with the predicted values for the second stage
mortgages_IVpredicted <- data.frame(
  D = predict(mortgages_firststage_D),
  x = mortgages_bw6$x,
  x_treat = predict(mortgages_firststage_x_treat),
  home_ownership = mortgages_bw6$home_ownership,
  nonwhite = mortgages_bw6$nonwhite,
  bpl = factor(mortgages_bw6$bpl)
)

# 5. Run the second-stage regression using lm
mortgages_secondstage <- lm(home_ownership ~ D + x + x_treat + nonwhite + bpl, data = mortgages_IVpredicted)

# 6. View the summary to find the point estimate for D
print(summary(mortgages_secondstage))
