# Functions to calculate ANOVA
install.packages("lsr")
library(lsr)

# Get one-way ANOVA table
m.aov1 <- aov(price ~ small_region, data = cleaned_monohull_data)
summary(m.aov1)
c.aov1 <- aov(price ~ small_region, data = cleaned_catamaran_data)
summary(c.aov1)

# Calculate eta squared 
etaSquared(m.aov1)
etaSquared(c.aov1)

# Get two-way ANOVA table
m.aov2 <- aov(price ~ small_region + variant, data = cleaned_monohull_data)
summary(m.aov2)
c.aov2 <- aov(price ~ small_region + variant, data = cleaned_catamaran_data)
summary(c.aov2)

# Calculate eta squared 
etaSquared(m.aov2)
etaSquared(c.aov2)
