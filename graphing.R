## Graphing
## Hannah Armstrong

library(ggplot2)
library(ggpubr)
library(dplyr)

## Load data

strategysuccess1 <- read.csv("StrategySuccess")
SLSpayoff1 <- read.csv("SLSPayoff")
meantraits1 <- read.csv("MeanTraitsInSystem")

strategysuccess2 <- read.csv("StrategySuccess")
SLSpayoff2 <- read.csv("SLSPayoff")
meantraits2 <- read.csv("MeanTraitsInSystem")

## Mean and SD of payoffs

# bf = 1

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 0])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 0])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 1])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 1])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 2])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 2])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 3])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 3])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 4])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 1 & strategysuccess1$SLS == 4])

# bf = 2

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 0])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 0])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 1])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 1])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 2])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 2])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 3])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 3])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 4])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 2 & strategysuccess2$SLS == 4])

# bf = 4

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 0])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 0])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 1])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 1])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 2])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 2])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 3])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 3])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 4])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 4 & strategysuccess1$SLS == 4])

# bf = 8

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 0])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 0])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 1])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 1])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 2])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 2])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 3])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 3])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 4])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 8 & strategysuccess2$SLS == 4])

# bf = 16

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 0])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 0])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 1])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 1])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 2])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 2])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 3])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 3])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 4])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 16 & strategysuccess2$SLS == 4])

# bf = 32

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 0])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 0])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 1])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 1])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 2])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 2])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 3])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 3])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 4])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 32 & strategysuccess1$SLS == 4])

# bf = 64

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 0])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 0])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 1])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 1])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 2])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 2])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 3])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 3])

mean(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 4])
sd(strategysuccess2$TotalPayoff[strategysuccess2$Branching == 64 & strategysuccess2$SLS == 4])

# bf = 128

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 0])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 0])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 1])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 1])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 2])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 2])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 3])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 3])

mean(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 4])
sd(strategysuccess1$TotalPayoff[strategysuccess1$Branching == 128 & strategysuccess1$SLS == 4])


## Payoff versus SLS (for each branching factor)

# bf = 1

bf1 <- strategysuccess1[strategysuccess1$Branching == 1,]

boxplot(TotalPayoff ~ SLS, data = bf1, main = "Total Payoff per SLS (bf = 1)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig1_sig <- ggplot(data = bf1, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 1)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("0", "4"), 
                                 c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), 
                                 c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig1 <- ggplot(data = bf1, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 1") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf1 <- aov(TotalPayoff ~ as.factor(SLS), data = bf1)
summary(modelbf1)
TukeyHSD(modelbf1)

# bf = 2

bf2 <- strategysuccess2[strategysuccess2$Branching == 2,]

boxplot(TotalPayoff ~ SLS, data = bf2, main = "Total Payoff per SLS (bf = 2)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig2_sig <- ggplot(data = bf2, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 2)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("0", "4"), 
                                 c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), 
                                 c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")


fig2 <- ggplot(data = bf2, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

motheme_classic2()modelbf2 <- aov(TotalPayoff ~ as.factor(SLS), data = bf2)
summary(modelbf2)
TukeyHSD(modelbf2)

# bf = 4

bf4 <- strategysuccess1[strategysuccess1$Branching == 4,]

boxplot(TotalPayoff ~ SLS, data = bf4, main = "Total Payoff per SLS (bf = 4)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig3_sig <- ggplot(data = bf4, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 4)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("0", "4"), 
                                 c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), 
                                 c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig3 <- ggplot(data = bf4, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 4") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf4 <- aov(TotalPayoff ~ as.factor(SLS), data = bf4)
summary(modelbf4)
TukeyHSD(modelbf4)

# bf = 8

bf8 <- strategysuccess2[strategysuccess2$Branching == 8,]

boxplot(TotalPayoff ~ SLS, data = bf8, main = "Total Payoff per SLS (bf = 8)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig4_sig <- ggplot(data = bf8, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 8)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("0", "4"), 
                                 c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), 
                                 c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig4 <- ggplot(data = bf8, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 8") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf8 <- aov(TotalPayoff ~ as.factor(SLS), data = bf8)
summary(modelbf8)
TukeyHSD(modelbf8)

# bf = 16

bf16 <- strategysuccess2[strategysuccess2$Branching == 16,]

boxplot(TotalPayoff ~ SLS, data = bf16, main = "Total Payoff per SLS (bf = 16)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig5_sig <- ggplot(data = bf16, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 16)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("0", "4"), 
                                 c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), 
                                 c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig5 <- ggplot(data = bf16, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 16") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf16 <- aov(TotalPayoff ~ as.factor(SLS), data = bf16)
summary(modelbf16)
TukeyHSD(modelbf16)

# bf = 32

bf32 <- strategysuccess1[strategysuccess1$Branching == 32,]

boxplot(TotalPayoff ~ SLS, data = bf32, main = "Total Payoff per SLS (bf = 32)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig6_sig <- ggplot(data = bf32, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 32)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("0", "4"), 
                                 c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), 
                                 c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig6 <- ggplot(data = bf32, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 32") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf32 <- aov(TotalPayoff ~ as.factor(SLS), data = bf32)
summary(modelbf32)
TukeyHSD(modelbf32)

# bf = 64

bf64 <- strategysuccess2[strategysuccess2$Branching == 64,]

boxplot(TotalPayoff ~ SLS, data = bf64, main = "Total Payoff per SLS (bf = 64)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig7_sig <- ggplot(data = bf64, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 64)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("0", "2"), c("0", "3"), c("1", "2"), 
                                 c("1", "4"), c("2", "3"), c("2", "4"), c("3", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig7 <- ggplot(data = bf64, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 64") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf64 <- aov(TotalPayoff ~ as.factor(SLS), data = bf64)
summary(modelbf64)
TukeyHSD(modelbf64)

# bf = 128

bf128 <- strategysuccess1[strategysuccess1$Branching == 128,]

boxplot(TotalPayoff ~ SLS, data = bf128, main = "Total Payoff per SLS (bf = 128)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

fig8_sig <- ggplot(data = bf128, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_light() +
  geom_boxplot() +
  ggtitle("Total Payoff per SLS (bf = 128)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff") +
  geom_signif(comparisons = list(c("0", "1"), c("1", "2"), c("1", "3"), c("1", "4")),
              map_signif_level = TRUE,
              step_increase = 0.1,
              test = "t.test")

fig8 <- ggplot(data = bf128, aes(x = as.factor(SLS), y = TotalPayoff)) +
  theme_pubr() +
  geom_boxplot() +
  ggtitle("Branching Factor = 128") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Social Learning Strategy") +
  ylab("Total Payoff")

modelbf128 <- aov(TotalPayoff ~ as.factor(SLS), data = bf128)
summary(modelbf128)
TukeyHSD(modelbf128)

combinedfigure <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8,
                             nrow = 4, ncol = 2)

## Branching factor versus payoff for each SLS

random <- rbind(strategysuccess1[strategysuccess1$SLS == 0,], strategysuccess2[strategysuccess2$SLS == 0,])
randomtotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(random$TotalPayoff[random$Branching == 1]), mean(random$TotalPayoff[random$Branching == 2]),
                                                    mean(random$TotalPayoff[random$Branching == 4]), mean(random$TotalPayoff[random$Branching == 8]),
                                                    mean(random$TotalPayoff[random$Branching == 16]), mean(random$TotalPayoff[random$Branching == 32]),
                                                    mean(random$TotalPayoff[random$Branching == 64]), mean(random$TotalPayoff[random$Branching == 128])),
                     rbind(mean(random$ProportionSuccessfulTrials[random$Branching == 1]), mean(random$ProportionSuccessfulTrials[random$Branching == 2]),
                           mean(random$ProportionSuccessfulTrials[random$Branching == 4]), mean(random$ProportionSuccessfulTrials[random$Branching == 8]),
                           mean(random$ProportionSuccessfulTrials[random$Branching == 16]), mean(random$ProportionSuccessfulTrials[random$Branching == 32]),
                           mean(random$ProportionSuccessfulTrials[random$Branching == 64]), mean(random$ProportionSuccessfulTrials[random$Branching == 128])))
colnames(randomtotal) <- c("BranchingFactor", "MeanPayoff", "ProportionSuccessfulTrials")
randomtotal <- as.data.frame(randomtotal)
randomtotal$BranchingFactor <- factor(randomtotal$BranchingFactor)
randomtotal[,2] <- randomtotal[,2]/randomtotal[,2]

payoff <- rbind(strategysuccess1[strategysuccess1$SLS == 1,], strategysuccess2[strategysuccess2$SLS == 1,])
payofftotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(payoff$TotalPayoff[payoff$Branching == 1]), mean(payoff$TotalPayoff[payoff$Branching == 2]),
                                                    mean(payoff$TotalPayoff[payoff$Branching == 4]), mean(payoff$TotalPayoff[payoff$Branching == 8]),
                                                    mean(payoff$TotalPayoff[payoff$Branching == 16]), mean(payoff$TotalPayoff[payoff$Branching == 32]),
                                                    mean(payoff$TotalPayoff[payoff$Branching == 64]), mean(payoff$TotalPayoff[payoff$Branching == 128])),
                     rbind(mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 1]), mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 2]),
                           mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 4]), mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 8]),
                           mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 16]), mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 32]),
                           mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 64]), mean(payoff$ProportionSuccessfulTrials[payoff$Branching == 128])))
colnames(payofftotal) <- c("BranchingFactor", "MeanPayoff", "ProportionSuccessfulTrials")
payofftotal[,2] <- payofftotal[,2]/randomtotal[,2]
payofftotal <- as.data.frame(payofftotal)
payofftotal$BranchingFactor <- factor(payofftotal$BranchingFactor)

similarity <- rbind(strategysuccess1[strategysuccess1$SLS == 2,], strategysuccess2[strategysuccess2$SLS == 2,])
similaritytotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(similarity$TotalPayoff[similarity$Branching == 1]), mean(similarity$TotalPayoff[similarity$Branching == 2]),
                                                        mean(similarity$TotalPayoff[similarity$Branching == 4]), mean(similarity$TotalPayoff[similarity$Branching == 8]),
                                                        mean(similarity$TotalPayoff[similarity$Branching == 16]), mean(similarity$TotalPayoff[similarity$Branching == 32]),
                                                        mean(similarity$TotalPayoff[similarity$Branching == 64]), mean(similarity$TotalPayoff[similarity$Branching == 128])),
                         rbind(mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 1]), mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 2]),
                               mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 4]), mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 8]),
                               mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 16]), mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 32]),
                               mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 64]), mean(similarity$ProportionSuccessfulTrials[similarity$Branching == 128])))
colnames(similaritytotal) <- c("BranchingFactor", "MeanPayoff", "ProportionSuccessfulTrials")
similaritytotal[,2] <- similaritytotal[,2]/randomtotal[,2]
similaritytotal <- as.data.frame(similaritytotal)
similaritytotal$BranchingFactor <- factor(similaritytotal$BranchingFactor)

age <- rbind(strategysuccess1[strategysuccess1$SLS == 3,], strategysuccess2[strategysuccess2$SLS == 3,])
agetotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(age$TotalPayoff[age$Branching == 1]), mean(age$TotalPayoff[age$Branching == 2]),
                                                 mean(age$TotalPayoff[age$Branching == 4]), mean(age$TotalPayoff[age$Branching == 8]),
                                                 mean(age$TotalPayoff[age$Branching == 16]), mean(age$TotalPayoff[age$Branching == 32]),
                                                 mean(age$TotalPayoff[age$Branching == 64]), mean(age$TotalPayoff[age$Branching == 128])),
                  rbind(mean(age$ProportionSuccessfulTrials[age$Branching == 1]), mean(age$ProportionSuccessfulTrials[age$Branching == 2]),
                        mean(age$ProportionSuccessfulTrials[age$Branching == 4]), mean(age$ProportionSuccessfulTrials[age$Branching == 8]),
                        mean(age$ProportionSuccessfulTrials[age$Branching == 16]), mean(age$ProportionSuccessfulTrials[age$Branching == 32]),
                        mean(age$ProportionSuccessfulTrials[age$Branching == 64]), mean(age$ProportionSuccessfulTrials[age$Branching == 128])))
colnames(agetotal) <- c("BranchingFactor", "MeanPayoff", "ProportionSuccessfulTrials") 
agetotal[,2] <- agetotal[,2]/randomtotal[,2]
agetotal <- as.data.frame(agetotal)
agetotal$BranchingFactor <- factor(agetotal$BranchingFactor)

conformity <- rbind(strategysuccess1[strategysuccess1$SLS == 4,], strategysuccess2[strategysuccess2$SLS == 4,])
conformitytotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(conformity$TotalPayoff[conformity$Branching == 1]), mean(conformity$TotalPayoff[conformity$Branching == 2]),
                                                        mean(conformity$TotalPayoff[conformity$Branching == 4]), mean(conformity$TotalPayoff[conformity$Branching == 8]),
                                                        mean(conformity$TotalPayoff[conformity$Branching == 16]), mean(conformity$TotalPayoff[conformity$Branching == 32]),
                                                        mean(conformity$TotalPayoff[conformity$Branching == 64]), mean(conformity$TotalPayoff[conformity$Branching == 128])),
                         rbind(mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 1]), mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 2]),
                               mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 4]), mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 8]),
                               mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 16]), mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 32]),
                               mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 64]), mean(conformity$ProportionSuccessfulTrials[conformity$Branching == 128])))
colnames(conformitytotal) <- c("BranchingFactor", "MeanPayoff", "ProportionSuccessfulTrials") 
conformitytotal[,2] <- conformitytotal[,2]/randomtotal[,2]
conformitytotal <- as.data.frame(conformitytotal)
conformitytotal$BranchingFactor <- factor(conformitytotal$BranchingFactor)

combineddata <- bind_rows(
  mutate(randomtotal, Group = "Random"),
  mutate(payofftotal, Group = "Payoff"),
  mutate(similaritytotal, Group = "Similarity"),
  mutate(agetotal, Group = "Age"),
  mutate(conformitytotal, Group = "Conformity")
)

ggplot(combineddata, aes(x = BranchingFactor, y = MeanPayoff, color = Group, group = Group, linetype = Group)) +
  theme_classic() +
  geom_line(size = 0.75) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Branching Factor") +
  ylab("Relative Payoff Compared to Random Learning") +
  labs(color = "Social Learning Strategy") +
  scale_color_manual(values =c("Payoff" = "#00B0F6", "Similarity" = "#E76BF3",
                                "Age" = "#F8766D", "Conformity" = "#A3A500")) +  
  scale_linetype_manual(values = c("Random" = "dashed", "Payoff" = "solid", "Similarity" = "solid",
                                   "Age" = "solid", "Conformity" = "solid")) +
  guides(color = guide_legend(override.aes = list(linetype = "solid")),
         linetype = FALSE)

## Plotting for presentation

presdata <- combineddata
presdata$BranchingFactor <- rep(8:1, 5)
presdata$BranchingFactor <- as.factor(presdata$BranchingFactor)

ggplot(presdata, aes(x = BranchingFactor, y = MeanPayoff, color = Group, group = Group, linetype = Group)) +
  theme_classic() +
  geom_line(size = 0.75) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Trait Model") +
  ylab(" ") +
  labs(color = "Social Learning Strategy") +
  scale_color_manual(values =c("Payoff" = "#00B0F6", "Similarity" = "#E76BF3",
                               "Age" = "#F8766D", "Conformity" = "#00BF7D")) +  
  scale_linetype_manual(values = c("Random" = "dashed", "Payoff" = "solid", "Similarity" = "solid",
                                   "Age" = "solid", "Conformity" = "solid")) +
  guides(color = guide_legend(override.aes = list(linetype = "solid")),
         linetype = FALSE)

ggplot(presdata, aes(x = BranchingFactor, y = ProportionSuccessfulTrials, color = Group, group = Group)) +
  theme_classic() +
  geom_line(size = 0.75) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Trait Model") +
  ylab("Proportion of Successful Trials") +
  labs(color = "Social Learning Strategy") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))



## Proportion of successful trials per SLS

ggplot(combineddata, aes(x = BranchingFactor, y = ProportionSuccessfulTrials, color = Group, group = Group)) +
  theme_classic() +
  geom_line(size = 0.75) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Branching Factor") +
  ylab("Proportion of Successful Trials") +
  labs(color = "Social Learning Strategy") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))

## Branching factor versus payoff for each SLS (last 5000 timesteps)

sumpayofflast1 <- cbind(strategysuccess1[,c(2,4,5)], rowSums(SLSpayoff1[,15002:20001]))
colnames(sumpayofflast1) <- c("Simulation", "Branching", "SLS", "Payoff")
sumpayofflast2 <- cbind(strategysuccess2[,c(2,4,5)], rowSums(SLSpayoff2[,15002:20001]))
colnames(sumpayofflast2) <- c("Simulation", "Branching", "SLS", "Payoff")

last5000 <- rbind(sumpayofflast1, sumpayofflast2)

branching_levels <- c(1, 2, 4, 8, 16, 32, 64, 128)

random <- sapply(branching_levels, function(x) mean(last5000$Payoff[last5000$Branching == x & last5000$SLS == 0]))
randomlast <- cbind(branching_levels, random)
colnames(randomlast) <- c("BranchingFactor", "MeanPayoff")
randomlast <- as.data.frame(randomlast)
randomlast$BranchingFactor <- factor(randomlast$BranchingFactor)

payoff <- sapply(branching_levels, function(x) mean(last5000$Payoff[last5000$Branching == x & last5000$SLS == 1]))
payofflast <- cbind(branching_levels, payoff)
colnames(payofflast) <- c("BranchingFactor", "MeanPayoff")
payofflast <- as.data.frame(payofflast)
payofflast$BranchingFactor <- factor(payofflast$BranchingFactor)
payofflast[,2] <- payofflast[,2]/randomlast[,2]

similarity <- sapply(branching_levels, function(x) mean(last5000$Payoff[last5000$Branching == x & last5000$SLS == 2]))
similaritylast <- cbind(branching_levels, similarity)
colnames(similaritylast) <- c("BranchingFactor", "MeanPayoff")
similaritylast <- as.data.frame(similaritylast)
similaritylast$BranchingFactor <- factor(similaritylast$BranchingFactor)
similaritylast[,2] <- similaritylast[,2]/randomlast[,2]

age <- sapply(branching_levels, function(x) mean(last5000$Payoff[last5000$Branching == x & last5000$SLS == 3]))
agelast <- cbind(branching_levels, age)
colnames(agelast) <- c("BranchingFactor", "MeanPayoff")
agelast <- as.data.frame(agelast)
agelast$BranchingFactor <- factor(agelast$BranchingFactor)
agelast[,2] <- agelast[,2]/randomlast[,2]

conformity <- sapply(branching_levels, function(x) mean(last5000$Payoff[last5000$Branching == x & last5000$SLS == 4]))
conformitylast <- cbind(branching_levels, conformity)
colnames(conformitylast) <- c("BranchingFactor", "MeanPayoff")
conformitylast <- as.data.frame(conformitylast)
conformitylast$BranchingFactor <- factor(conformitylast$BranchingFactor)
conformitylast[,2] <- conformitylast[,2]/randomlast[,2]

combineddatalast <- bind_rows(
  mutate(randomlast, Group = "Random"),
  mutate(payofflast, Group = "Payoff"),
  mutate(similaritylast, Group = "Similarity"),
  mutate(agelast, Group = "Age"),
  mutate(conformitylast, Group = "Conformity")
)

ggplot(combineddatalast[9:40,], aes(x = BranchingFactor, y = MeanPayoff, color = Group, group = Group)) +
  theme_light() +
  geom_line(size = 0.75) +
  ggtitle("SLS Efficacy Given Different Branching Factors (last 5000 timesteps)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Branching Factor") +
  ylab("Mean Payoff") +
  labs(color = "Social Learning Strategy")

## Mean traits in the system over time 

meantraits <- rbind(cbind(strategysuccess1[,c(2,4,5)], meantraits1[,-1]), 
                    cbind(strategysuccess2[,c(2,4,5)], meantraits2[,-1]))

meantraitsbf1 <- meantraits[meantraits$Branching == 1,]
meantraitsbf2 <- meantraits[meantraits$Branching == 2,]
meantraitsbf4 <- meantraits[meantraits$Branching == 4,]
meantraitsbf8 <- meantraits[meantraits$Branching == 8,]
meantraitsbf16 <- meantraits[meantraits$Branching == 16,]
meantraitsbf32 <- meantraits[meantraits$Branching == 32,]                     
meantraitsbf64 <- meantraits[meantraits$Branching == 64,]                    
meantraitsbf128 <- meantraits[meantraits$Branching == 128,]

# bf 1
randomMeanbf1 <- colMeans(meantraitsbf1[meantraitsbf1$SLS == 0, 4:20003])
payoffMeanbf1 <- colMeans(meantraitsbf1[meantraitsbf1$SLS == 1, 4:20003])
similarityMeanbf1 <- colMeans(meantraitsbf1[meantraitsbf1$SLS == 2, 4:20003])
ageMeanbf1 <- colMeans(meantraitsbf1[meantraitsbf1$SLS == 3, 4:20003])
conformityMeanbf1 <- colMeans(meantraitsbf1[meantraitsbf1$SLS == 4, 4:20003])

plot(randomMeanbf1, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
    main = "Branching Factor = 1", ylim = c(0.1,0.5), lwd = 1.5, bty = "l")
lines(payoffMeanbf1, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf1, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf1, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf1, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

# bf 2
randomMeanbf2 <- colMeans(meantraitsbf2[meantraitsbf2$SLS == 0, 4:20003])
payoffMeanbf2 <- colMeans(meantraitsbf2[meantraitsbf2$SLS == 1, 4:20003])
similarityMeanbf2 <- colMeans(meantraitsbf2[meantraitsbf2$SLS == 2, 4:20003])
ageMeanbf2 <- colMeans(meantraitsbf2[meantraitsbf2$SLS == 3, 4:20003])
conformityMeanbf2 <- colMeans(meantraitsbf2[meantraitsbf2$SLS == 4, 4:20003])

plot(randomMeanbf2, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
     main = "Branching Factor = 2", ylim = c(0.15,0.5), lwd = 1.5, bty = "l")
lines(payoffMeanbf2, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf2, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf2, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf2, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

# bf 4

randomMeanbf4 <- colMeans(meantraitsbf4[meantraitsbf4$SLS == 0, 4:20003])
payoffMeanbf4 <- colMeans(meantraitsbf4[meantraitsbf4$SLS == 1, 4:20003])
similarityMeanbf4 <- colMeans(meantraitsbf4[meantraitsbf4$SLS == 2, 4:20003])
ageMeanbf4 <- colMeans(meantraitsbf4[meantraitsbf4$SLS == 3, 4:20003])
conformityMeanbf4 <- colMeans(meantraitsbf4[meantraitsbf4$SLS == 4, 4:20003])

plot(randomMeanbf4, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
     main = "Branching Factor = 4", ylim = c(0.15,0.5), lwd = 1.5, bty = "l")
lines(payoffMeanbf4, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf4, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf4, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf4, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

#bf 8

randomMeanbf8 <- colMeans(meantraitsbf8[meantraitsbf8$SLS == 0, 4:20003])
payoffMeanbf8 <- colMeans(meantraitsbf8[meantraitsbf8$SLS == 1, 4:20003])
similarityMeanbf8 <- colMeans(meantraitsbf8[meantraitsbf8$SLS == 2, 4:20003])
ageMeanbf8 <- colMeans(meantraitsbf8[meantraitsbf8$SLS == 3, 4:20003])
conformityMeanbf8 <- colMeans(meantraitsbf8[meantraitsbf8$SLS == 4, 4:20003])

plot(randomMeanbf8, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population", 
     main = "Branching Factor = 8", ylim = c(0.22,0.55), lwd = 1.5, bty = "l")
lines(payoffMeanbf8, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf8, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf8, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf8, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

# bf 16

randomMeanbf16 <- colMeans(meantraitsbf16[meantraitsbf16$SLS == 0, 4:20003])
payoffMeanbf16 <- colMeans(meantraitsbf16[meantraitsbf16$SLS == 1, 4:20003])
similarityMeanbf16 <- colMeans(meantraitsbf16[meantraitsbf16$SLS == 2, 4:20003])
ageMeanbf16 <- colMeans(meantraitsbf16[meantraitsbf16$SLS == 3, 4:20003])
conformityMeanbf16 <- colMeans(meantraitsbf16[meantraitsbf16$SLS == 4, 4:20003])

plot(randomMeanbf16, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
     main = "Branching Factor = 16", ylim = c(0.3,0.60), lwd = 1.5, bty = "l")
lines(payoffMeanbf16, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf16, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf16, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf16, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

# bf 32

randomMeanbf32 <- colMeans(meantraitsbf32[meantraitsbf32$SLS == 0, 4:20003])
payoffMeanbf32 <- colMeans(meantraitsbf32[meantraitsbf32$SLS == 1, 4:20003])
similarityMeanbf32 <- colMeans(meantraitsbf32[meantraitsbf32$SLS == 2, 4:20003])
ageMeanbf32 <- colMeans(meantraitsbf32[meantraitsbf32$SLS == 3, 4:20003])
conformityMeanbf32 <- colMeans(meantraitsbf32[meantraitsbf32$SLS == 4, 4:20003])

plot(randomMeanbf32, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
     main = "Branching Factor = 32", ylim = c(0.35,0.6), lwd = 1.5, bty = "l")
lines(payoffMeanbf32, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf32, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf32, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf32, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

# bf 64

randomMeanbf64 <- colMeans(meantraitsbf64[meantraitsbf64$SLS == 0, 4:20003])
payoffMeanbf64 <- colMeans(meantraitsbf64[meantraitsbf64$SLS == 1, 4:20003])
similarityMeanbf64 <- colMeans(meantraitsbf64[meantraitsbf64$SLS == 2, 4:20003])
ageMeanbf64 <- colMeans(meantraitsbf64[meantraitsbf64$SLS == 3, 4:20003])
conformityMeanbf64 <- colMeans(meantraitsbf64[meantraitsbf64$SLS == 4, 4:20003])

plot(randomMeanbf64, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
     main = "Branching Factor = 64", ylim = c(0.45,0.62), lwd = 1.5, bty = "l")
lines(payoffMeanbf64, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf64, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf64, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf64, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

# bf 128

randomMeanbf128 <- colMeans(meantraitsbf128[meantraitsbf128$SLS == 0, 4:20003])
payoffMeanbf128 <- colMeans(meantraitsbf128[meantraitsbf128$SLS == 1, 4:20003])
similarityMeanbf128 <- colMeans(meantraitsbf128[meantraitsbf128$SLS == 2, 4:20003])
ageMeanbf128 <- colMeans(meantraitsbf128[meantraitsbf128$SLS == 3, 4:20003])
conformityMeanbf128 <- colMeans(meantraitsbf128[meantraitsbf128$SLS == 4, 4:20003])

plot(randomMeanbf128, type = "l", col = "#00BF7D", xlab = "Timesteps", ylab = "Mean Traits in the Population",
     main = "Branching Factor = 128", ylim = c(0.5,0.6), lwd = 1.5, bty = "l")
lines(payoffMeanbf128, col = "#00B0F6", lwd = 1.5)
lines(similarityMeanbf128, col = "#E76BF3", lwd = 1.5)
lines(ageMeanbf128, col = "#F8766D", lwd = 1.5)
lines(conformityMeanbf128, col = "#A3A500", lwd = 1.5)
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd = 2, cex = 0.8)

legend("bottom", legend=c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col=c("#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500"), lwd=2, horiz=TRUE,
       x="center")
