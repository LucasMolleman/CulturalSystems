overview <- read.csv("data/2024-05-10 22-30-56/results.csv")

# how many rounds do they learn 
overview %>%
  filter(Round == 1 & EligibleTeachers != is.na(EligibleTeachers)) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_density()


p1 <- overview %>%
  filter(Round ==1 & ID ==6 & EligibleTeachers != is.na(EligibleTeachers)) %>%
  arrange(Timestep) 

p2 <- overview %>%
  filter(Round ==1 & ID ==1 & EligibleTeachers != is.na(EligibleTeachers)) %>%
  arrange(Timestep)

diff_SimSuccess <- diff(p1$SimilarityLearningSuccess)
diff_SimSuccess <- diff_SimSuccess[diff_SimSuccess > 0]
sum(diff_SimSuccess)/ length(diff_SimSuccess)
 
diff_Simfail <- diff(p1$SimilarityLearningFails)
diff_Simfail <- diff_Simfail[diff_Simfail > 0]
sum(diff_Simfail)/ length(diff_Simfail) 

theta = seq(0, 1, length.out = 1e3)
for (t in 1:length(p2$Timestep)) {
  plot(theta, dbeta(theta, 
                    1+p2$PayoffLearningSuccess[t], 
                    1+p2$PayoffLearningFails[t]), 
       type = "l", 
       lwd = 2,
       col = color_learningstrat1,
       xlim = c(0, 1),
       ylim = c(0,4),
       xlab = paste0("Beta(", p2$SimilarityLearningSuccess[t], "," ,p2$SimilarityLearningFails[t],")" ), 
       ylab = "")
  lines(theta, dbeta(theta, 
                     1+p2$SimilarityLearningSuccess[t],
                     1+p2$SimilarityLearningFails[t]),
        col = color_learningstrat2)
  lines(theta, dbeta(theta, 
                     1+p2$AgeLearningSuccess[t],
                     1+p2$AgeLearningFails[t]),
        col = color_learningstrat3)
  lines(theta, dbeta(theta, 
                     1+p2$ConformityLearningSuccess[t],
                     1+p2$ConformityLearningFails[t]),
        col = color_learningstrat4)
  Sys.sleep(.1)
}


---



## n per group

overview %>%
  group_by(MetaStrat) %>%
  summarize(Count_all = n()) %>%
  mutate(percent = Count_all/sum(Count_all))

overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat) %>%
  summarize(Count_all = n()) %>%
  mutate(percent = Count_all/sum(Count_all))
  

## control how many eligible teachers inds find per skill level ----
overview %>%
  group_by(MetaStrat, Skilllevel) %>%
  filter(!is.na(EligibleTeachers)) %>%
  reframe(EligibleTeachers = mean(EligibleTeachers)) %>%
  ggplot() +
  geom_line(aes(x = Skilllevel, y = EligibleTeachers, color = factor(MetaStrat, labels = labels_metatypes))) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color= labs_legend_meta) +
  geom_hline(yintercept=1, linetype="dashed", color = "grey")

overview %>%
  group_by(MetaStrat) %>%
  filter(!is.na(EligibleTeachers)) %>%
  reframe(EligibleTeachers = mean(EligibleTeachers)) 

overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat) %>%
  filter(!is.na(EligibleTeachers)) %>%
  reframe(EligibleTeachers = mean(EligibleTeachers)) 

overview %>%
  group_by(LearningStrat, Skilllevel) %>%
  filter(!is.na(EligibleTeachers)) %>%
  reframe(EligibleTeachers = mean(EligibleTeachers)) %>%
  ggplot() +
  geom_line(aes(x = Skilllevel, y = EligibleTeachers, color = factor(LearningStrat, labels = labels_learningstrat))) +
  theme_classic() +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat) +
  geom_hline(yintercept=1, linetype="dashed", color = "grey")

### rates and successes in SL and IL, and resets 

##by MetaStrat
overview %>%
  group_by(MetaStrat) %>%
  reframe(SLrate = mean(SLrate))

overview %>%
  group_by(MetaStrat) %>%
  filter(!is.na(SLsucess)) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(MetaStrat, labels=labels_metatypes), y = SLsucess)) +
  geom_col(fill = colors_meta) +
  labs(x = labs_legend_meta, y = "Social Learning Success Rate") +
  theme_classic()
  
overview %>%
  group_by(MetaStrat) %>%
  reframe(ILrate = mean(ILrate))

overview %>%
  group_by(MetaStrat) %>%
  filter(!is.na(ILsucess)) %>%
  reframe(ILsucess = mean(ILsucess)) %>%
  ggplot(aes(x = factor(MetaStrat, labels=labels_metatypes), y = ILsucess)) +
  geom_col(fill = colors_meta) +
  theme_classic() +
  labs(x = labs_legend_meta, y = "Individual Learning Success Rate")

overview %>%
  group_by(MetaStrat) %>%
  reframe(Resets = mean(Resets)) %>%
  ggplot(aes(x = factor(MetaStrat, labels=labels_metatypes), y = Resets)) +
  geom_col(fill = colors_meta) +
  theme_classic() +
  labs(x = labs_legend_meta)

##by Learning strategy
overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat) %>%
  reframe(SLrate = mean(SLrate))

overview %>%
  group_by(LearningStrat) %>%
  filter(!is.na(SLsucess)) %>%
  filter(MetaStrat == 1) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), y = SLsucess)) +
  geom_col(fill = colors_learningstrat) +
  labs(x = labs_legend_learningstrat, y = "Social Learning Success Rate") +
  theme_classic()

overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat) %>%
  filter(!is.na(SLsucess)) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), y = SLsucess, fill = factor(LearningStrat, labels=labels_learningstrat))) +
  geom_boxplot() +
  scale_fill_manual(values = colors_learningstrat) +
  stat_summary(fun = "mean", geom = "point", color = "white") +
  labs(x = labs_legend_learningstrat, y = "Social Learning Success Rate") +
  theme_classic() +
  theme(legend.position = "top")

overview %>%
  group_by(LearningStrat) %>%
  filter(MetaStrat == 1) %>%
  reframe(ILrate = mean(ILrate))

overview %>%
  group_by(LearningStrat) %>%
  filter(MetaStrat == 1) %>%
  filter(!is.na(ILsucess)) %>%
  reframe(ILsucess = mean(ILsucess))

overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat) %>%
  filter(!is.na(ILsucess)) %>%
  reframe(ILsucess = mean(ILsucess)) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), y = ILsucess)) +
  geom_col(fill = colors_learningstrat) +
  theme_classic() +
  labs(x = labs_legend_learningstrat, y = "Individual Learning Success Rate")

overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat) %>%
  filter(!is.na(ILsucess)) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), fill = factor(LearningStrat, labels=labels_learningstrat), y = ILsucess)) +
  geom_boxplot() +
  scale_fill_manual(values = colors_learningstrat) +
  stat_summary(fun = "mean", geom = "point", color = "white") +
  labs(x = labs_legend_learningstrat, y = "Individual Learning Success Rate",
       fill = labs_legend_learningstrat) +
  theme_classic() +
  theme(legend.position = "top")


overview %>%
  group_by(LearningStrat) %>%
  filter(MetaStrat == 1) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), fill = factor(LearningStrat, labels=labels_learningstrat), y = Resets)) +
  geom_boxplot() +
  scale_fill_manual(values = colors_learningstrat) +
  stat_summary(fun = "mean", geom = "point", color = "white") +
  labs(x = labs_legend_learningstrat, fill = labs_legend_learningstrat) +
  theme_classic() +
  theme(legend.position = "top")



### differences Meta strat ------------------------
# get mean skill level per Meta Strat
overview %>%
  group_by(MetaStrat, Timestep) %>%
  reframe(Skilllevel = mean(Skilllevel)) %>%
  mutate(MetaStrat = factor(MetaStrat, labels = labels_metatypes)) %>%
  ggplot() +
  geom_line(aes(x = Timestep, y = Skilllevel, color = factor(MetaStrat))) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color = labs_legend_meta, y = "Mean Skill Level")

# get mean payoff per Meta Strat
overview %>%
  group_by(MetaStrat, Timestep) %>%
  reframe(Payoff = mean(Payoff)) %>%
  mutate(MetaStrat = factor(MetaStrat, labels = labels_metatypes)) %>%
  ggplot() +
  geom_line(aes(x = Timestep, y = Payoff, color = factor(MetaStrat))) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color = labs_legend_meta, y = "Mean Payoff")


### differences per learning strat ------------------------

# get mean payoff per Learning Strat 
overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat, Timestep) %>%
  reframe(Payoff = mean(Payoff)) %>%
  mutate(LearningStrat = factor(LearningStrat, labels=labels_learningstrat)) %>%
  ggplot() +
  geom_line(aes(x = Timestep, y = Payoff, color = LearningStrat)) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color = labs_legend_learningstrat, y = "Mean Payoff") +
  theme_classic()

# get mean skill level per Learning Strat 
overview %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat, Timestep) %>%
  reframe(Skilllevel = mean(Skilllevel)) %>%
  mutate(LearningStrat = factor(LearningStrat, labels=labels_learningstrat)) %>%
  ggplot() +
  geom_line(aes(x = Timestep, y = Skilllevel, color = LearningStrat)) + 
  scale_color_manual(values = colors_learningstrat) +
  labs(color = labs_legend_learningstrat, y = "Mean Skill Level") +
  theme_classic()



# use of learning STrat for flexible learner over age
overview %>%
  filter(MetaStrat == 2) %>%
  group_by(Age, LearningStrat) %>%
  filter(!is.na(LearningStrat))%>%
  summarize(Count_all = n()) %>%
  mutate(LearningStratUse = Count_all/sum(Count_all)) %>%
  ggplot() +
  geom_smooth(aes(x = Age, y = LearningStratUse, color = factor(LearningStrat, labels=labels_learningstrat))) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Use of Learning Strategy in %") +
  geom_hline(yintercept=0.25, linetype="dashed", color = "grey") +
  theme_classic()

### overview over time --------------

## get overview of weights for integrative learner over age
time_overview <- overview %>%
  filter(MetaStrat == 3) %>%
  group_by(Timestep) %>%
  summarise_at(vars(12:19), list(Mean = mean))

# successes only
p_success_Weights <- time_overview %>%
  pivot_longer(
    cols = ends_with("Success_Mean"),
    names_to = "Strat",
    values_to = "Weights"
  )

p_success_Weights$Strat <- factor(p_success_Weights$Strat, levels=c("PayoffLearningSuccess_Mean", "SimilarityLearningSuccess_Mean", "AgeLearningSuccess_Mean", "ConformityLearningSuccess_Mean"), labels=labels_learningstrat)
  
p_success_Weights %>% ggplot() +
  geom_smooth(aes(x = Timestep, y = Weights, color = Strat)) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color='Learning Strategy') +
  theme_classic()
  

# failures only
p_fail_Weights <- time_overview %>%
  pivot_longer(
    cols = ends_with("Fails_Mean"),
    names_to = "Strat",
    values_to = "Weights"
  ) 

p_fail_Weights$Strat <- factor(p_fail_Weights$Strat, levels=c("PayoffLearningFails_Mean", "SimilarityLearningFails_Mean", "AgeLearningFails_Mean", "ConformityLearningFails_Mean"), labels=labels_learningstrat)

p_fail_Weights %>%
  ggplot() +
  geom_smooth(aes(x = Timestep, y = Weights, color = Strat)) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat) +
  theme_classic()

### both in one graph

p_Weights <- time_overview %>%
  pivot_longer(
    cols = 2:9,
    names_to = "Strat",
    values_to = "Weights"
  ) %>%
  mutate(outcome = ifelse(grepl("Success_Mean$", Strat), "pos", "fail"))

p_Weights$Strat <- factor(p_Weights$Strat, 
                          levels=c("PayoffLearningSuccess_Mean", "SimilarityLearningSuccess_Mean", "AgeLearningSuccess_Mean", "ConformityLearningSuccess_Mean", "PayoffLearningFails_Mean", "SimilarityLearningFails_Mean", "AgeLearningFails_Mean", "ConformityLearningFails_Mean"), 
                          labels=c("Success_pos", "Similarity_pos", "Age_pos", "Conformity_pos","Success_neg", "Similarity_neg", "Age_neg", "Conformity_neg"))
p_Weights$Strat <- fct_collapse(p_Weights$Strat, Success = c("Success_pos", "Success_neg"), Similarity = c("Similarity_pos", "Similarity_neg"), Age = c("Age_pos", "Age_neg"), Conformity = c("Conformity_pos","Conformity_neg"))


p_Weights %>%
  ggplot() +
  geom_smooth(aes(x = Timestep, y = Weights, color = Strat)) +
  scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
  labs(color=labs_legend_learningstrat) +
  theme_bw() +
  facet_wrap(~factor(outcome, levels = c("pos", "fail"), labels = c("Successful Attempts", "Failed Attempts")))


lastTimestep_weights <- time_overview %>%
  filter(Timestep == timesteps) 

theta = seq(0, 1, length.out = 1e3)  
Integrative_betas <- data.frame(Theta = theta, 
                                Payoff = dbeta(theta,
                                               1+lastTimestep_weights$PayoffLearningSuccess_Mean, 
                                               1+lastTimestep_weights$PayoffLearningFails_Mean
                                               ),
                                Similarity = dbeta(theta,
                                                   1+lastTimestep_weights$SimilarityLearningSuccess_Mean, 
                                                   1+lastTimestep_weights$SimilarityLearningFails_Mean),
                                Age = dbeta(theta, 
                                            1+lastTimestep_weights$AgeLearningSuccess_Mean,
                                            1+lastTimestep_weights$AgeLearningFails_Mean),
                                Conformity = dbeta(theta, 
                                                   1+lastTimestep_weights$ConformityLearningSuccess_Mean,
                                                   1+lastTimestep_weights$ConformityLearningFails_Mean)
                                ) %>%
  pivot_longer(Payoff:Conformity,
    names_to = "Strategy", 
    values_to = "Density"
  ) %>%
  mutate(Strategy = factor(Strategy, levels = c("Payoff", "Similarity", "Age", "Conformity") , labels = labels_learningstrat))


## Beta distributions integrative and flexible learner 

Integrative_betas %>%
  ggplot(aes(x = Theta, y = Density, color = Strategy)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat) +
  theme_classic()


time_overview_flex <- overview %>%
  filter(MetaStrat == 2) %>%
  filter(Timestep == timesteps) %>%
  summarise_at(vars(13:20), list(Mean = mean))

Flexible_Betas <- data.frame(Theta = theta, 
             Payoff = dbeta(theta,
                            1+time_overview_flex$PayoffLearningSuccess_Mean, 
                            1+time_overview_flex$PayoffLearningFails_Mean
             ),
             Similarity = dbeta(theta,
                                1+time_overview_flex$SimilarityLearningSuccess_Mean, 
                                1+time_overview_flex$SimilarityLearningFails_Mean),
             Age = dbeta(theta, 
                         1+time_overview_flex$AgeLearningSuccess_Mean,
                         1+time_overview_flex$AgeLearningFails_Mean),
             Conformity = dbeta(theta, 
                                1+time_overview_flex$ConformityLearningSuccess_Mean,
                                1+time_overview_flex$ConformityLearningFails_Mean)
  ) %>%
  pivot_longer(Payoff:Conformity,
               names_to = "Strategy", 
               values_to = "Density"
  ) %>%
  mutate(Strategy = factor(Strategy, levels = c("Payoff", "Similarity", "Age", "Conformity") , labels = labels_learningstrat))

Flexible_Betas %>%
  ggplot(aes(x = Theta, y = Density, color = Strategy)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat) +
  theme_classic()
  

Flexible_Betas <- Flexible_Betas %>%
  mutate(Learner = "flexible")
Integrative_betas <- Integrative_betas %>%
  mutate(Learner = "integrative")

all_Betas <- rbind.data.frame(Flexible_Betas, Integrative_betas)

all_Betas %>%
  ggplot(aes(x = Theta, y = Density, color = Strategy)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
  labs(color=labs_legend_learningstrat) +
  theme_bw() +
  facet_wrap(~factor(Learner, levels = c("flexible", "integrative"), labels = c("Flexible Learner", "Integrative Learner")))

#### plots for development of skill level over time in pop

## calculate payoff at start and end for whole pop
overview %>%
  select(Timestep, Payoff) %>%
  mutate(Timestep = factor(Timestep)) %>%
  filter(Timestep == 0 | Timestep == timesteps) %>%
  ggplot(aes(x=Payoff, color = Timestep)) + 
  geom_density() +
  theme_classic() +
  labs(color='Timestep') 

Payoff_diff <- overview %>%
  filter(Timestep == 0 | Timestep == timesteps) %>%
  group_by(Timestep) %>%
  reframe(Payoff = mean(Payoff)) 
  
Payoff_diff$Payoff[2] - Payoff_diff$Payoff[1]

# calculate skill level at start and end for whole pop
overview %>%
  select(Timestep, Skilllevel) %>%
  mutate(Timestep = factor(Timestep)) %>%
  filter(Timestep == 0 | Timestep == timesteps) %>%
  ggplot(aes(x=Skilllevel, color = Timestep)) + 
  geom_density() +
  theme_classic() +
  labs(color='Timestep') +
  geom_hline(yintercept=0.05, linetype="dashed", color = "grey")

Skilllevel_diff <- overview %>%
  filter(Timestep == 0 | Timestep == timesteps) %>%
  group_by(Timestep) %>%
  reframe(Skilllevel = mean(Skilllevel)) 

Skilllevel_diff$Skilllevel[2] - Skilllevel_diff$Skilllevel[1]

# filtered per meta strat last time step
overview %>%
  filter(Timestep == timesteps) %>%
  ggplot(aes(x=Skilllevel, color = factor(MetaStrat, labels = labels_metatypes))) + 
  geom_density() +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color=labs_legend_meta, y = "Density")

# first and last timestep per meta strat 
overview %>%
  select(Timestep, Skilllevel, MetaStrat) %>%
  mutate(Timestep = factor(Timestep)) %>%
  filter(Timestep == 0 | Timestep == timesteps) %>%
  ggplot(aes(x=Skilllevel, color = factor(MetaStrat, labels = labels_metatypes))) + 
  geom_density() +
  theme_bw() +
  scale_color_manual(values = colors_meta) +
  labs(color=labs_legend_meta, y = "Density") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End")))

# first and last timestep per learning strat 
overview %>%
  filter(MetaStrat == 1) %>%
  select(Timestep, Skilllevel, LearningStrat) %>%
  mutate(Timestep = factor(Timestep)) %>%
  filter(Timestep == 0 | Timestep == timesteps) %>%
  ggplot(aes(x=Skilllevel, color = factor(LearningStrat, labels = labels_learningstrat))) + 
  geom_density() +
  theme_bw() +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Density") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End")))


### get overview of development of each skill level
# Calculate frequencies for each Skilllevel at start
freq_timestep_0 <- overview %>%
  filter(Timestep == 0) %>%
  mutate(MetaStrat = factor(MetaStrat, labels = labels_metatypes)) %>%
  group_by(MetaStrat, Skilllevel) %>%
  summarise(freq_timestep_0 = n())

# Calculate frequencies for each Skilllevel at end
freq_timestep_last <- overview %>%
  filter(Timestep == timesteps) %>%
  mutate(MetaStrat = factor(MetaStrat, labels = labels_metatypes)) %>%
  group_by(MetaStrat, Skilllevel) %>%
  summarise(freq_timestep_last = n())

# Merge the frequency tables and calculate the difference
frequency_diff <- merge(freq_timestep_0, freq_timestep_last, by = c("MetaStrat", "Skilllevel"), all = TRUE) %>%
  mutate(diff = ifelse(is.na(freq_timestep_0), -freq_timestep_last, 
                       ifelse(is.na(freq_timestep_last), freq_timestep_0, freq_timestep_0 - freq_timestep_last)))


frequency_diff %>%
  ggplot(aes(x=Skilllevel, y = diff, color = MetaStrat)) + 
  geom_line(linetype = 1) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(x = "Skilllevel", y = "Frequency Difference", color=labs_legend_meta) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")

ggplot(frequency_diff, aes(x = Skilllevel, y = diff, fill = MetaStrat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Skilllevel", y = "Frequency Difference", fill = "MetaStrat") +
  ggtitle("Frequency Difference of Skilllevel by MetaStrat") +
  theme_classic() +
  scale_fill_manual(values = colors_meta) +
  labs(fill=labs_legend_meta) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")

### animate pop development

p_animate <- overview %>%
  select(Timestep, Skilllevel, MetaStrat) %>%
  group_by(Timestep) %>%
  ggplot(aes(x=Skilllevel, color = factor(MetaStrat, labels = labels_metatypes))) + 
  geom_density() +
  theme_bw() +
  scale_color_manual(values = colors_meta, 
                    name = labs_legend_meta) +
  facet_wrap(~factor(MetaStrat, labels = labels_metatypes)) +
  transition_time(Timestep) +
  ease_aes('linear')
