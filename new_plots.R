# how many rounds do they learn 
dev.new(width=5, height=4)
overview %>%
  filter(Timestep >= 1) %>%
  group_by(Round, ID) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_density(linewidth = 1) +
  theme_classic() +
  labs(x = "Number of Learning Rounds", y = "Density")

overview %>%
  filter(Timestep >= 1) %>%
  group_by(Round, ID) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  reframe(n = mean(n))

ggsave("Learning_Rounds_Density.png")

## control how many eligible teachers inds find per skill level ----
overview %>%
  filter(Timestep >= 1) %>%
  group_by(MetaStrat, Skilllevel) %>%
  reframe(EligibleTeachers = mean(EligibleTeachers)) %>%
  ggplot() +
  geom_line(aes(x = Skilllevel, y = EligibleTeachers, color = factor(MetaStrat, labels = labels_metatypes)), linewidth = 1) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color= labs_legend_meta) +
  geom_hline(yintercept=1, linetype="dashed", color = "grey")



# --- SL Success 
overview %>%
  group_by(MetaStrat) %>%
  filter(!is.na(SLsucess)) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(MetaStrat, labels=labels_metatypes), y = SLsucess)) +
  geom_col(fill = colors_meta) +
  labs(x = labs_legend_meta, y = "Social Learning Success Rate") +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,1))

ggsave("SLSuccessMeta.png", width = 12.5, height = 12.5, units = "cm")


overview %>%
  filter(MetaStrat == 2 | MetaStrat == 3 | MetaStrat == 1 & LearningStrat == 2) %>%
  group_by(MetaStrat) %>%
  filter(!is.na(SLsucess)) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(MetaStrat, labels=labels_metatypes), y = SLsucess)) +
  geom_col(fill = colors_meta) +
  labs(x = labs_legend_meta, y = "Social Learning Success Rate") +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,1))

           
dev.new(width=5, height=4)
overview %>%
  group_by(LearningStrat) %>%
  filter(!is.na(SLsucess)) %>%
  filter(MetaStrat == 1) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), y = SLsucess)) +
  geom_col(fill = colors_learningstrat) +
  labs(x = labs_legend_learningstrat, y = "Social Learning Success Rate") +
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,1))
ggsave("SLSuccessLearningStrat.png", width = 12.5, height = 12.5, units = "cm")


SL_successLStrats <- overview %>%
  group_by(LearningStrat) %>%
  filter(!is.na(SLsucess)) %>%
  filter(MetaStrat == 1) %>%
  reframe(SLsucess = mean(SLsucess))


### differences Meta strat ------------------------
# get mean skill level per Meta Strat
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat, 
       Skilllevel, 
       .direction = "down") %>%
  group_by(MetaStrat, Timestep) %>%
  reframe(Skilllevel = mean(Skilllevel)) %>%
  mutate(MetaStrat = factor(MetaStrat, labels = labels_metatypes)) %>%
  ggplot() +
  geom_line(aes(x = Timestep, y = Skilllevel, color = factor(MetaStrat)), linewidth = 1.5) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color = labs_legend_meta, y = "Mean Skill Level") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) 
ggsave("SkillsMeta.png", width = 13, height = 12.5, units = "cm")

# get mean payoff per Meta Strat
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat, 
       Payoff, 
       .direction = "down") %>%
  group_by(MetaStrat, Timestep) %>%
  reframe(Payoff = mean(Payoff)) %>%
  mutate(MetaStrat = factor(MetaStrat, labels = labels_metatypes)) %>%
  ggplot() +
  geom_line(aes(x = Timestep, y = Payoff, color = factor(MetaStrat)), linewidth = 1.5) +
  theme_classic() +
  scale_color_manual(values = colors_meta) +
  labs(color = labs_legend_meta, y = "Mean Payoff") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("PayoffMeta.png", width = 13, height = 12.5, units = "cm")

### differences per learning strat ------------------------

# get mean payoff per Learning Strat 
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       LearningStrat,
       Payoff, 
       .direction = "down") %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat, Timestep) %>%
  reframe(Payoff = mean(Payoff)) %>%
  mutate(LearningStrat = factor(LearningStrat, labels=labels_learningstrat)) %>%
  ggplot() +
  geom_smooth(aes(x = Timestep, y = Payoff, color = LearningStrat), linewidth = 1.5) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color = labs_legend_learningstrat, y = "Mean Payoff") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
                          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
                          axis.text.x=element_text(size=10, vjust=0.5),
                          axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("PayoffLearningStrat.png", width = 13, height = 12.5, units = "cm")

# get mean skill level per Learning Strat 
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       LearningStrat,
       Skilllevel, 
       .direction = "down") %>%
  filter(MetaStrat == 1) %>%
  group_by(LearningStrat, Timestep) %>%
  reframe(Skilllevel = mean(Skilllevel)) %>%
  mutate(LearningStrat = factor(LearningStrat, labels=labels_learningstrat)) %>%
  ggplot() +
  geom_smooth(aes(x = Timestep, y = Skilllevel, color = LearningStrat), linewidth = 1.5) + 
  scale_color_manual(values = colors_learningstrat) +
  labs(color = labs_legend_learningstrat, y = "Mean Skill Level") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("SkillsLearningStrat.png", width = 13, height = 12.5, units = "cm")

# use of learning STrat for flexible learner over age ---- 
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
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("FlexibleLearnerOverAge.png")


overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       LearningStrat, 
       Timestep,
       .direction = "down") %>%
  filter(MetaStrat == 2) %>%
  group_by(Timestep, LearningStrat) %>%
  filter(!is.na(LearningStrat))%>%
  summarize(Count_all = n()) %>%
  mutate(LearningStratUse = Count_all/sum(Count_all)) %>%
  ggplot() +
  geom_smooth(aes(x = Timestep, y = LearningStratUse, color = factor(LearningStrat, labels=labels_learningstrat)), linewidth =2) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Use of Learning Strategy in %") +
  geom_hline(yintercept=0.25, linetype="dashed", color = "grey") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("FlexibleLearnerOverTimestep.png", width = 15, height = 12.5, units = "cm")

## get overview of weights for integrative learner over timstep
time_overview <- overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       Age, 
       PayoffLearningSuccess,
       PayoffLearningFails,
       SimilarityLearningSuccess,
       SimilarityLearningFails,
       AgeLearningSuccess,
       AgeLearningFails,
       ConformityLearningSuccess,
       ConformityLearningFails,
       .direction = "down") %>%
  filter(MetaStrat == 3) %>%
  group_by(Timestep) %>%
  summarise_at(vars(12:19), list(Mean = mean))

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
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  facet_wrap(~factor(outcome, levels = c("pos", "fail"), labels = c("Successful Attempts", "Failed Attempts"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"))

ggsave("IntegrativeLearnerOverTimestep.png", width = 17, height = 12.5, units = "cm")


# ---
lastTimestep_weights <- time_overview %>%
  filter(Timestep == max(Timestep)) 

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
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       Age, 
       PayoffLearningSuccess,
       PayoffLearningFails,
       SimilarityLearningSuccess,
       SimilarityLearningFails,
       AgeLearningSuccess,
       AgeLearningFails,
       ConformityLearningSuccess,
       ConformityLearningFails,
       .direction = "down") %>%
  filter(MetaStrat == 2) %>%
  group_by(Timestep) %>%
  summarise_at(vars(12:19), list(Mean = mean)) %>%
  filter(Timestep == max(Timestep))


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

# dev.new(width=50, height=4)
all_Betas %>%
  ggplot(aes(x = Theta, y = Density, color = Strategy)) +
  geom_vline(xintercept= SL_successLStrats$SLsucess[1], linetype="dashed", color = color_learningstrat1) +
  geom_vline(xintercept= SL_successLStrats$SLsucess[2], linetype="dashed", color = color_learningstrat2) +
  geom_vline(xintercept= SL_successLStrats$SLsucess[3], linetype="dashed", color = color_learningstrat3) +
  geom_vline(xintercept= SL_successLStrats$SLsucess[4], linetype="dashed", color = color_learningstrat4) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
  labs(color=labs_legend_learningstrat) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  facet_wrap(~factor(Learner, levels = c("flexible", "integrative"), 
                     labels = c("Flexible Learner", "Integrative Learner"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(0,1,0.1))

ggsave("allBetas.png", width = 30, height = 13, units = "cm")

## --- calculate posterior means

integrative_postMeans <- lastTimestep_weights %>%
  mutate(Success = PayoffLearningSuccess_Mean/(PayoffLearningSuccess_Mean + PayoffLearningFails_Mean),
         Similarity = SimilarityLearningSuccess_Mean/(SimilarityLearningSuccess_Mean + SimilarityLearningFails_Mean),
         Age = AgeLearningSuccess_Mean/(AgeLearningSuccess_Mean + AgeLearningFails_Mean),
         Conformity = ConformityLearningSuccess_Mean/(ConformityLearningSuccess_Mean+ConformityLearningFails_Mean)
  )

round(integrative_postMeans$Conformity, 3)


flexible_postMeans <- time_overview_flex %>%
  mutate(Success = PayoffLearningSuccess_Mean/(PayoffLearningSuccess_Mean + PayoffLearningFails_Mean),
         Similarity = SimilarityLearningSuccess_Mean/(SimilarityLearningSuccess_Mean + SimilarityLearningFails_Mean),
         Age = AgeLearningSuccess_Mean/(AgeLearningSuccess_Mean + AgeLearningFails_Mean),
         Conformity = ConformityLearningSuccess_Mean/(ConformityLearningSuccess_Mean+ConformityLearningFails_Mean)
  )

round(flexible_postMeans$Success, 3)
round(flexible_postMeans$Similarity, 3)
round(flexible_postMeans$Age, 3)
round(flexible_postMeans$Conformity, 3)



# --- pop 

# first and last timestep per meta strat 
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       Age, 
       Skilllevel,
       .direction = "down") %>%
  select(Timestep, Skilllevel, MetaStrat) %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  ggplot(aes(x=Skilllevel, color = factor(MetaStrat, labels = labels_metatypes))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_meta) +
  labs(color=labs_legend_meta, y = "Density") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"))
 ggsave("PopDevMeta.png", width = 30, height = 13, units = "cm")

# first and last timestep per learning strat 
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       LearningStrat,
       Age, 
       Skilllevel,
       .direction = "down") %>%
  filter(MetaStrat == 1) %>%
  select(Timestep, Skilllevel, LearningStrat) %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  ggplot(aes(x=Skilllevel, color = factor(LearningStrat, labels = labels_learningstrat))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Density") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"))
ggsave("PopDevLStrat.png", width = 30, height = 13, units = "cm")

### -- animation

p_animate <- overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       LearningStrat,
       Skilllevel,
       .direction = "down") %>%
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

p_animate
anim_save("PopDev.gif", p_animate)



overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       Age, 
       .direction = "down") %>%
  select(Timestep, Age, MetaStrat) %>%
  filter(Timestep == max(Timestep)) %>%
  ggplot(aes(x=Age, color = factor(MetaStrat, labels = labels_metatypes))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_meta) +
  labs(color=labs_legend_meta, y = "Density")




Skilllevel_diff <- overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Skilllevel, 
       .direction = "down") %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Timestep) %>%
  reframe(Skilllevel = mean(Skilllevel)) 

Skilllevel_diff$Skilllevel[2] - Skilllevel_diff$Skilllevel[1]


overview %>%
  group_by(Round, ID) %>%
  select(Round, ID, Timestep, Skilllevel) %>%
  mutate(startTime = min(Timestep),
         endTime = max(Timestep)) %>%
  filter(Timestep == startTime | Timestep == endTime) %>%
  mutate(StartSkill = min(Skilllevel),
         EndSkill = max(Skilllevel)) %>%
  mutate(SkillDiff = EndSkill - StartSkill) %>%
  filter(Timestep == 0 & Skilllevel == StartSkill) %>%
  #filter(StartSkill > 20) %>%
  ggplot(aes(x = SkillDiff, color = factor(StartSkill))) +
  geom_density()


max(overview$Resets)
40*reset_rate
max(overview$Age)


overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Age,
       Skilllevel,
       .direction = "down") %>%
  select(Age, Skilllevel, Round, Timestep) %>%
  group_by(Round, Timestep) %>%
  reframe(CorrAgeSkill = cor(Age, Skilllevel)) %>%
  ggplot(aes(x = Timestep, y = CorrAgeSkill)) +
  geom_line()

  
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Age,
       Skilllevel,
       .direction = "down") %>%
  select(Age, Skilllevel, Round, Timestep) %>%
  group_by(Round, Timestep) %>%
  reframe(CorrAgeSkill = cor(Age, Skilllevel)) %>%
  ggplot(aes(x = Timestep, y = CorrAgeSkill)) +
  geom_line()

## flat tree 

overview %>%
  filter(Timestep > 0, 
         MetaStrat == 1,
         !is.na(AttemptedSkill)) %>%
  group_by(Round, ID) %>%
  mutate(LearningStep = 1:n()) %>%
  group_by(LearningStrat, LearningStep) %>% 
  reframe(AttemptedSkill = mean(AttemptedSkill)) %>%
  ggplot(aes(x = LearningStep, y = AttemptedSkill, color = factor(LearningStrat, labels = labels_learningstrat))) +
  geom_smooth(linewidth = 2) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Attempted Skill", x = "Learning Attempt")

overview %>%
  filter(MetaStrat == 1,
         !is.na(AttemptedSkill)) %>%
  group_by(Round, ID) %>%
  mutate(LearningStep = 1:n()) %>%
  group_by(LearningStrat, LearningStep) %>% 
  reframe(Payoff = mean(Payoff)) %>%
  ggplot(aes(x = LearningStep, y = Payoff, color = factor(LearningStrat, labels = labels_learningstrat))) +
  geom_smooth(linewidth = 2) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, x = "Learning Attempt")
