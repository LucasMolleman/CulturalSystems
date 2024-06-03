## Graphing
## Hannah Armstrong

## Load data

strategysuccess1 <- read.csv("StrategySuccess")
SLSpayoff1 <- read.csv("SLSPayoff")
meantraits1 <- read.csv("MeanTraitsInSystem")

strategysuccess2 <- read.csv("StrategySuccess")
SLSpayoff2 <- read.csv("SLSPayoff")
meantraits2 <- read.csv("MeanTraitsInSystem")

## Payoff versus SLS (for each branching factor)

# bf = 1

bf1 <- strategysuccess1[strategysuccess1$Branching == 1,]

boxplot(TotalPayoff ~ SLS, data = bf1, main = "Total Payoff per SLS (bf = 1)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf1, aes(x = as.factor(SLS), y = TotalPayoff)) +
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


modelbf1 <- aov(TotalPayoff ~ as.factor(SLS), data = bf1)
summary(modelbf1)
TukeyHSD(modelbf1)

# bf = 2

bf2 <- strategysuccess2[strategysuccess2$Branching == 2,]

boxplot(TotalPayoff ~ SLS, data = bf2, main = "Total Payoff per SLS (bf = 2)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf2, aes(x = as.factor(SLS), y = TotalPayoff)) +
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


modelbf2 <- aov(TotalPayoff ~ as.factor(SLS), data = bf2)
summary(modelbf2)
TukeyHSD(modelbf2)

# bf = 4

bf4 <- strategysuccess1[strategysuccess1$Branching == 4,]

boxplot(TotalPayoff ~ SLS, data = bf4, main = "Total Payoff per SLS (bf = 4)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf4, aes(x = as.factor(SLS), y = TotalPayoff)) +
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


modelbf4 <- aov(TotalPayoff ~ as.factor(SLS), data = bf4)
summary(modelbf4)
TukeyHSD(modelbf4)

# bf = 8

bf8 <- strategysuccess2[strategysuccess2$Branching == 8,]

boxplot(TotalPayoff ~ SLS, data = bf8, main = "Total Payoff per SLS (bf = 8)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf8, aes(x = as.factor(SLS), y = TotalPayoff)) +
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


modelbf8 <- aov(TotalPayoff ~ as.factor(SLS), data = bf8)
summary(modelbf8)
TukeyHSD(modelbf8)

# bf = 16

bf16 <- strategysuccess2[strategysuccess2$Branching == 16,]

boxplot(TotalPayoff ~ SLS, data = bf16, main = "Total Payoff per SLS (bf = 16)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf16, aes(x = as.factor(SLS), y = TotalPayoff)) +
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


modelbf16 <- aov(TotalPayoff ~ as.factor(SLS), data = bf16)
summary(modelbf16)
TukeyHSD(modelbf16)

# bf = 32

bf32 <- strategysuccess1[strategysuccess1$Branching == 32,]

boxplot(TotalPayoff ~ SLS, data = bf32, main = "Total Payoff per SLS (bf = 32)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf32, aes(x = as.factor(SLS), y = TotalPayoff)) +
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

modelbf32 <- aov(TotalPayoff ~ as.factor(SLS), data = bf32)
summary(modelbf32)
TukeyHSD(modelbf32)

# bf = 64

bf64 <- strategysuccess2[strategysuccess2$Branching == 64,]

boxplot(TotalPayoff ~ SLS, data = bf64, main = "Total Payoff per SLS (bf = 64)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf64, aes(x = as.factor(SLS), y = TotalPayoff)) +
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

modelbf64 <- aov(TotalPayoff ~ as.factor(SLS), data = bf64)
summary(modelbf64)
TukeyHSD(modelbf64)

# bf = 128

bf128 <- strategysuccess1[strategysuccess1$Branching == 128,]

boxplot(TotalPayoff ~ SLS, data = bf128, main = "Total Payoff per SLS (bf = 128)", ylab = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

ggplot(data = bf128, aes(x = as.factor(SLS), y = TotalPayoff)) +
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

modelbf128 <- aov(TotalPayoff ~ as.factor(SLS), data = bf128)
summary(modelbf128)
TukeyHSD(modelbf128)
