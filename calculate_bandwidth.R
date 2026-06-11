# Load required libraries
library(rddtools)
library(dplyr)

# Load the dataset
data(house)

# 1. Filter the house dataset to apply the 0.4 bandwidth on either side of the cutoff (0)
house_bw <- house %>%
  filter(abs(x) <= 0.4)

# 2. Create the running variable (x), the treatment dummy (D), and the interaction (x_right) 
house_bw <- house_bw %>%
  mutate(
    x = x - 0,
    D = ifelse(x >= 0, 1, 0),
    x_right = D * x
  )

# 3. Run the linear regression on this new filtered dataset
house_reg_lm_bw <- lm(y ~ D + x + x_right, data = house_bw)

# 4. View the summary to find the treatment effect
print(summary(house_reg_lm_bw))
