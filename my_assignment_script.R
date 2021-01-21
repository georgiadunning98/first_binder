library(tidyverse)
library(ggrepel)
library(extrafont)
library(dplyr)
library(ggthemes)

my_messy_data <- read_csv("assignment_1_data.csv")

head(my_messy_data)

str(my_messy_data)

my_tidier_data <- my_messy_data %>%
  rename(Participant = ID) %>%
  pivot_longer(cols = c(Condition1, Condition2, Condition3, Condition4), 
               names_to = "Condition", 
               values_to = "RT")

head(my_tidier_data)

my_tidier_data %>%
  mutate(Condition = factor(Condition)) %>%
  head()

my_final_data <- my_tidier_data %>% 
  mutate(Condition = recode(Condition,
                            "Condition1" = "Number_Number",
                            "Condition2" = "Number_Letter", 
                            "Condition3" = "Letter_Number", 
                            "Condition4" = "Letter_Letter")) %>%
  separate(col = "Condition", into = c("Expectation", "Target"), sep = "_") %>%
  mutate(Expectation = factor(Expectation), Target = factor(Target))

head(my_final_data)

my_tidied_summary <- my_final_data %>%
  group_by(Expectation:Target) %>%
  summarise(mean_RT = mean(RT), median_RT = median(RT), sd_RT = sd(RT), min_RT = min(RT),max_RT = max(RT), number = n()) %>%
  arrange(-mean_RT) %>%
  as.data.frame()

my_tidied_summary

plot1 <- my_final_data %>% 
  ggplot(aes(x = Expectation:Target, y = RT, colour = Expectation:Target)) + 
  geom_violin() +
  geom_jitter(alpha = .5, width = .2) +
  geom_boxplot(alpha = .5) +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10)) +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  theme(plot.background = element_rect(fill = "gray90", linetype = 0),
        text = element_text(family = "Times New Roman")) +
  scale_colour_manual(values = colorRampPalette(c("orange", "darkmagenta", "turquoise"))(4)) +
  scale_x_discrete(labels = c("Expectation Number\nTarget Number", 
                              "Expectation Number\nTarget Letter", "Expectation Letter\nTarget Number", 
                              "Expectation Letter\nTarget Letter")) +
  labs(title = "Examining the effects of Expectation and Target on Reaction Time", 
       x = "Experimental Condition", 
       y = "Reaction Time (ms)")

plot1

plot2 <- my_final_data %>%
  ggplot(aes(x = RT, fill = Expectation:Target)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 5, binpositions = "all",
               stackdir = "center") +
  scale_y_continuous(NULL, breaks = NULL) +
  guides(fill = FALSE) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10)) +
  labs(title = "Reaction Time raw data for Congruent and Incongruent Conditions",
       x = "Reaction Time (Milliseconds)") +
  facet_wrap(~Expectation:Target, scales = "free")

plot2

plot3 <- my_final_data %>% 
  ggplot(aes(x = RT, fill = Expectation:Target)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  theme_igray() +
  guides(fill = FALSE) +
  theme(text = element_text(family = "Arial", face = "bold")) +
  labs( title = "Distribution of Reaction Time for each condition with Density Plots", 
        x = "Reaction Time (ms)", 
        y = "Density") +
  facet_wrap(~ Expectation:Target)

plot3


