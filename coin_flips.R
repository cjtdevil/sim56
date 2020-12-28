library(tidyverse)

set.seed(1995)

# Simulate 100 coin flips
df = data.frame(
  result = rbinom(1000,1,.5)
) %>%
  mutate(trial = row_number(),
         cumul = cumsum(result),
         cumul_perc = cumul/trial)
ggplot(df,aes(x=trial,y=cumul_perc)) +
  geom_line() + 
  geom_abline(intercept = 0.5,slope=0) +
  ylab("Cumulative Percent of Heads") +
  xlab("# of flips")
