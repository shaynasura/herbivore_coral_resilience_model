
## Response to Reviewers from submission to PLOS Computational Biology


# Filter the data to include values greater than 0.5
high_values <- your_data_frame %>%
  filter(Final_Coral > 0.5)

# Tally up how many occurrences there are for each unique value in 'Final_Coral'
value_counts <- high_values %>%
  group_by(Final_Coral) %>%
  tally()

# View the results
print(value_counts)


high_values <- qq_all %>% 
  filter(Final_Coral > 0.5)


value_counts <- high_values %>% 
  group_by(Final_Coral) %>% 
  tally()


binned_counts <- high_values %>% 
  mutate(Final_Coral_binned = cut(Final_Coral, breaks = seq(0.5, 0.9, by = 0.05))) %>% 
  group_by(Final_Coral_binned) %>% 
  tally()


sum(binned_counts$n)
# 4436 observations > 0.5

sum(binned_counts[4:5,2])
# 4027 observations between 0.65 - 0.75 final coral cover

4027 / 4436
# 0.9077998 of observations (aka 91%) of final coral cover values are within the 0.65 - 0.72 range.


max(high_values$Final_Coral)
# 0.7185964 is our maximum final coral cover value.