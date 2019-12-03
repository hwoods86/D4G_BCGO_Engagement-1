# Sankey Diagram
# Tutorial URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/
# Author: Andrew Macumber

#
## Goal
# Display engagement journeys using Sankey diagrams
##
#

#
## Define: "Never Attended" & "Not Old Enough"
member_engagement_fiscal <- member_engagement_fiscal %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, age_category, Eng_Level) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_wider(names_from = age_category, values_from = Eng_Level) %>%
  
  # Append the Members information
  inner_join(member_filtered, by = "d4g_member_id") %>%
  
  # replace any NaN with NA
  mutate_at(vars(-d4g_member_id), funs(ifelse(is.na(.), ifelse(age <= 10, "Not Old Enough",
                                                               ifelse(age <= 13, "Not Old Enough", "Never Attended")), .))) %>%
  
  # Remove age information and member id
  select(J, I, S)
##
#

#
## Count: All Journeys
journeys_all <- member_engagement_fiscal %>%
  
  # Group and count journeys
  group_by(J, I, S) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  
  # Remove 'Not Old Enough' Juniors
  filter(J != 'Not Old Enough') %>%
  
  # 'Not Old Enough' Intermediates, should be same for Senior
  within(S[I=="Not Old Enough"] <- 'Not Old Enough') %>%
  
  # 'Left' == (Junior Engaged to Int,Sen == 'Never Attended')
  within(I[I=="Never Attended" & S=="Never Attended"] <- "Left") %>%
  within(S[S=="Never Attended"] <- 'Left') %>%
  
  # Break apart 'New Members' and label inactive to active as 'Active'
  within(I[I=="Never Attended"] <- 'Inactive') %>%
  within(J[J=="Never Attended"] <- 'New Member') %>%
  within(I[J=="New Member" & I=="Inactive"] <- 'New Member') %>%
  within(I[I=="Inactive"] <- 'Active')

#
## Count: Junior to Intermediate Journeys
journeys_J_I <- journeys_all %>%
  
  # Select columns of interest
  select(J, I, Freq) %>%
  
  # Group and count journeys
  group_by(J, I) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Remove non-Juniors,Intermediates
  filter(J != "New Member" | I != "New Member") %>%
  
  # Convert Junior cateories to numbers
  within(J[J=="Active"] <- 0) %>%
  within(J[J=="New Member"] <- 1) %>%
  
  # Convert Intermediate categories to numbers
  within(I[I=="Active"] <- 2) %>%
  within(I[I=="Left"] <- 3) %>%
  within(I[I=="Not Old Enough"] <- 4) %>%
  within(I[I=="New Member"] <- 1)
##
#

#
## Count: Intermediate to Senior journeys
journeys_I_S <- journeys_all %>%
  
  # Select Intermediate and Senior
  select(I, S, Freq) %>%
  
  # Group and count
  group_by(I, S) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Remove unchanged categories
  filter(I != 'Left' | S != 'Left') %>%

  # Convert Intermediate categories to numbers
  within(I[I=="Active"] <- 2) %>%
  within(I[I=="New Member"] <- 1) %>%
  within(I[I=="Not Old Enough"] <- 4) %>%
  
  # Convert Senior(Sen.) categories to numbers
  within(S[S=="Active"] <- 5) %>%
  within(S[S=="Left"] <- 3) %>%
  within(S[S=="Not Old Enough"] <- 6)
##
#

#
## Rename columns for sankey function
names(journeys_J_I) <- c("source", "target", "value")
names(journeys_I_S) <- c("source", "target", "value")
##
#

#
## Create final dataframe, convert to numeric
sankey_data <- rbind(as.data.frame(journeys_J_I), as.data.frame(journeys_I_S))
i <- c(1, 2, 3)
sankey_data[ , i] <- apply(sankey_data[ , i], 2,  # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
##
#

#
## Create Sankey Plot

# Load Libraries
library(networkD3)

# Create your nodes
nodes = data.frame("name" = 
                     c("Junior", # Node 0
                       "New Member", # Node 1
                       "Intermediate", # Node 2
                       "Left",  # Node 3
                       "Not Old Enough",  # Node 4
                       "Senior", # Node 5
                       "Not Old Enough"  # Node 6
                       ))

# Plot
sankeyNetwork(Links = sankey_data,
              Nodes = nodes,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize= 18,
              nodeWidth = 30,
              units = 'Member')
##
#