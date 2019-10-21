library(FuzzyNumbers)
library(ggplot2)
### Figures for Paper, in order of appearance:

## Figure 1: Triangular fuzzy numbers.

f1 <- TriangularFuzzyNumber(1500,1800,2200)
FuzzyNumbers::plot(f1, xlab = "Number of Bathrooms", ylab = "Membership Value", xlim = c(1,6), ylim = c(0,1))

plot(f1)




## Figure 3: Trapazoidal Fuzzy Number: Time

fig3 <- TrapezoidalFuzzyNumber(10,10,60,80)

FuzzyNumbers::plot(fig3, xlab = "Time Requirement", ylab = "Membership Value", xlim = c(1,100), ylim = c(0,1))





f2 <- TriangularFuzzyNumber(2000,2000,2000)
e1 <- as.PiecewiseLinearFuzzyNumber(f1)
e2 <- as.PiecewiseLinearFuzzyNumber(f2)

possibilityExceedance(e1,e2)
possibilityExceedance(e2,e1)

possibilityUndervaluation(e1,e2)
possibilityUndervaluation(e2,e1)

FN1 <- as.data.frame(
  list("X" = c(2,3,5), "M" = c(0,1,0), "FN" = rep("A",3))
)
FN2 <- as.data.frame(
  list("X" = c(1,4,6), "M" = c(0,1,0), "FN" = rep("B",3))
)

fig2 <- rbind(FN1, FN2)

ggplot(fig2) +
  geom_line(aes(x=X,y=M,color = FN, size = 2)) +
  scale_x_continuous(breaks = seq(1,6,1)) +
  scale_y_continuous(breaks = c(0.0,0.5, 0.8,1.0)) +
  ggtitle("Possibility Exceedance Example") +
  theme(axis.text.x = element_text(size=14),
        axis.title.x = element_text(face="bold",size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold",size=14))