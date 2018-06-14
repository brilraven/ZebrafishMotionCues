# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#		Data Analysis: Experiment 2 :: Spatial trends :: Fig. S2
#   Updated: Wednesday, June 13, 2018
#   Citation: Lemasson, B., Tanner, C. Woodley, C., Threadgill, T., Qarqish, S., and Smith, D. 2018. Motion Cues tune social influence in shoaling fish
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ===================================  
# workspace
# ===================================  
WorkSpace = './Workspaces/exp2_CompanionEffects.RData'

# load(WorkSpace)

# save.image(WorkSpace)

# ===================================  
# library packages
# ===================================  
library(tidyverse)
library(ggplot2)
library(grid)  		      # for ggplot's 'unit' - used to scale legend's text

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(minpack.lm) # fitting nonlinear models

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Summary - 
# Initially, the best fit model here is a 2nd order polynomial. 
# This is because of outliers in Nc- and Nc+.
# Once the outliers are removed we find no differences in model
# fit between the polynomial & linear models. The linear model
# is adopted & residuals look better. 
# 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# ==================================================
# Fish interactions based on the decisions
# of leading companions :: GLMM analysis
# Note that all group sizes are retained, even N = 1
# for comparison with the group size effects tests.
# Omitting the group size of 1 data does change the results, but 
# not the overall conclusions. Exclusing the G = 1 data result in companions
# loosing their significance in the main Decision ~ Companions model.
# Speed, however, retains a significant effect on Decision
# and the results of the subsequent logistic fits do not change.
# ==================================================
Nc.df = read_tsv('./Data/Exp2_DecisiveFishData.tsv') 

# --- Leading companions (+ correct, - incorrect)
Nc.df$Companions = Nc.df$NFishCorrectArm - Nc.df$NFishIncorrectArm

# --- visualization
ggplot(data = Nc.df, aes(x = Companions, y = Decision, color = factor(Speed))) + geom_jitter()

# --- model exploration
m1 <- glmer(Decision ~ Speed + Companions + (1|Fish), data = Nc.df, family='binomial')
m2 <- glmer(Decision ~ Speed + I(poly(Companions,2)) + (1|Fish), data = Nc.df, family='binomial') # 2nd order
m3 <- glmer(Decision ~ Speed + I(poly(Companions,3)) + (1|Fish), data = Nc.df, family='binomial') # 3rd order
m4 <- glmer(Decision ~ Speed + I(poly(Companions,4)) + (1|Fish), data = Nc.df, family='binomial') # 4th order

AIC(m1,m2,m3,m4)# mC0c best
#     df      AIC
# m1  4 196.2501
# m2  5 196.9177
# m3  6 192.0091
# m4  7 193.9666

summary(m3) # --- Promising, but difficult to interpret 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)              0.07779    0.34148   0.228  0.81980   
# Speed                    0.14281    0.04736   3.016  0.00256 **
#   I(poly(Companions, 3))1  4.56901    2.82818   1.615  0.10620   
# I(poly(Companions, 3))2 -3.21347    2.67969  -1.199  0.23045   
# I(poly(Companions, 3))3 -6.82627    2.80198  -2.436  0.01484 * 
# 

# --- does Scaling companions improve model fit?
NFxZoneData$Companions.z = scale(NFxZoneData$Companions)

m3b <- glmer(Decision ~ Speed + I(poly(Companions.z,3, raw=T)) + (1|Fish), data = NFxZoneData, family='binomial') # 2nd order
# same, but reveals rank deficiencies and dropped columns / coefficients - presumably stemming from instances where a rank category has only 1 or few data points - outliers

# -----------------------------
# diagnostics
# -----------------------------
mod = m3; mdata = data.e2
# [models fit to reduced dataset] mod = mP1; mdata = df.1

p1<- plot(mod,type=c("p","smooth")) # note, idLables don't seem to work for me
mdata2 <- data.frame(mdata,resid=residuals(mod,type="pearson"), fitted = fitted(mod))
p2 <- ggplot(mdata2,aes(x=factor(Companions), y=resid)) + geom_boxplot() + coord_flip()
E <- residuals(mod,type="pearson")
p3 <- ggplot(aes(x=resid), data = mdata2) + geom_histogram(fill="grey",color="black") + xlab("Residuals") 
p4 <- ggplot(aes(sample=resid), data = mdata2) + stat_qq()

diagnostics_pL = grid.arrange(p1, p2, p3, p4,nrow=2,top = textGrob("Companion effect"))


# -----------------------------
# identify potential outliers &
# check back against simpler model
# -----------------------------


# Identify rare, high value Nc cases by hand
table(Nc.df[,c(7,14)])

                                    Companions
# Decision -12 -10 -9 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 10 11 13 14
# 0         0   1  1  0  1  4  6  5 23  6  2  1  2  0  2  1  0  1  0  0  1  1
# 1         1   0  0  2  0  2  2  2 40 12  7  7 10  4  4  1  4  3  2  3  0  0

# --- moving inward from the extremes, consider removing cases represented by only 1 event (on both -/+ sides) 
df.1 = data.e2 %>% subset(Companions > -9 & Companions < 13) 
  mL1 = glmer(Decision ~ Speed + Companions + (1|Fish), data = df.1, family='binomial')
  mP1 = glmer(Decision ~ Speed + I(poly(Companions,3)) + (1|Fish), data = df.1, family='binomial') # 3rd order
  
  AIC(mL1, mP1)
  #     df      AIC
  # mL1  4 184.8621
  # mP1  6 188.2570

# linear model now fits better with the same results
summary(mL1)     
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -0.09487    0.33807  -0.281  0.77899   
# Speed        0.13681    0.04703   2.909  0.00363 **
# Companions   0.18463    0.07721   2.391  0.01679 * 


# ==================================================
# ----- Investigating social feedback x Speed level
# ==================================================

# fit a logistic equation
Lmod <- function(x, a, b){
  1/(1 + exp(-a*(x-b)))
}

# --------------------
# --- speed = 1
# --------------------

# --- nls 
mLfit.v1 = nlsLM(Decision ~ Lmod(Companions, a, b),data = subset(df.1, Speed == 1), start=list(a=-0.5, b = 0.5))
summary(mLfit.v1)
# Parameters:
#     Estimate Std. Error t value Pr(>|t|)  
#   a  0.52048    0.22078   2.358   0.0208 *
#   b  0.06547    0.47380   0.138   0.8904 

px <- with(subset(df.1, Speed == 1), seq(from = min(Companions), to = max(Companions), length.out = 100))

pLfit.v1 = data.frame(Companions = px, Decision = Lmod(px, a = coef(mLfit.v1)[1], b = coef(mLfit.v1)[2]))
pLfit.v1$Speed = 1

# visual:
ggplot(data = subset(df.1, Speed == 1), aes(x = Companions, y = Decision)) + geom_line(data = pLfit.v1, aes(x = Companions, y = Decision)) + ylim(0,1)

# --------------------
# --- speed = 10
# --------------------

# --- nls 
mLfit.v10 = nlsLM(Decision ~ Lmod(Companions, a, b),data = subset(df.1, Speed == 10), start=list(a=-0.5, b = 0.5))
summary(mLfit.v10)
#   Estimate Std. Error t value Pr(>|t|)
# a   0.1585     0.1065   1.488    0.141
# b  -6.7263     5.1702  -1.301    0.197


px <- with(subset(df.1, Speed == 10), seq(from = min(Companions), to = max(Companions), length.out = 100))

pLfit.v10 = data.frame(Companions = px, Decision = Lmod(px, a = coef(mLfit.v10)[1], b = coef(mLfit.v10)[2]))
pLfit.v10$Speed = 10

# visual:
ggplot(data = subset(df.1, Speed == 10), aes(x = Companions, y = Decision)) + geom_line(data = pLfit.v10, aes(x = Companions, y = Decision)) + ylim(0,1)


# =======================================
# Does parameter a remain consistently 
# larger when speed = 1 vs. 10?
# =======================================

# -------------------------------
# Group 5: v1 vs v10
# -------------------------------

dfv1g5 = df.1 %>% subset(Speed == 1 & GroupSize == 5)
nrow(dfv1g5)
# 21

mLfit.dfv1g5 = nlsLM(Decision ~ Lmod(Companions, a, b),data = dfv1g5, start=list(a=-0.5, b = 0.5))

summary(mLfit.dfv1g5) 
# Formula: Decision ~ Lmod(Companions, a, b)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
# a   1.0526     0.6954   1.514    0.147
# b  -0.2229     0.4853  -0.459    0.651


dfv10g5 = df.1 %>% subset(Speed == 10 & GroupSize == 5)
nrow(dfv10g5)
# 22

mLfit.dfv10g5 = nlsLM(Decision ~ Lmod(Companions, a, b),data = dfv10g5, start=list(a=-0.5, b = 0.5))

summary(mLfit.dfv10g5)
# Formula: Decision ~ Lmod(Companions, a, b)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
# a   0.4602     0.3323   1.385    0.181
# b  -2.2568     1.7754  -1.271    0.218


# -------------------------------
# Group 10: v1 vs v10
# -------------------------------
dfv1g10 = df.1 %>% subset(Speed == 1 & GroupSize == 10)
nrow(dfv1g10)
# 21

mLfit.dfv1g10 = nlsLM(Decision ~ Lmod(Companions, a, b),data = dfv1g10, start=list(a=-0.5, b = 0.5))

summary(mLfit.dfv1g10) 
# Estimate Std. Error t value Pr(>|t|)
# a   0.2224     0.1912   1.163    0.259
# b   2.0598     2.4336   0.846    0.408


dfv10g10 = df.1 %>% subset(Speed == 10 & GroupSize == 10)
nrow(dfv10g10)
# 18

mLfit.dfv10g10 = nlsLM(Decision ~ Lmod(Companions, a, b),data = dfv10g10, start=list(a=-0.5, b = 0.5))

summary(mLfit.dfv10g10) 
# Estimate Std. Error t value Pr(>|t|)
# a   0.06957    0.14611   0.476    0.640
# b -14.95091   37.42836  -0.399    0.695
# 
# -------------------------------
# Group 15: v1 vs v10
# -------------------------------

dfv1g15 = df.1 %>% subset(Speed == 1 & GroupSize == 15)
nrow(dfv1g15)

mLfit.dfv1g15 = nlsLM(Decision ~ Lmod(Companions, a, b),data = dfv1g15, start=list(a=-0.5, b = 0.5))

summary(mLfit.dfv1g15) 
# Estimate Std. Error t value Pr(>|t|)
# a   0.1423     0.1939   0.734    0.473
# b  -2.1041     5.5570  -0.379    0.710


dfv10g15 = df.1 %>% subset(Speed == 10 & GroupSize == 15)
nrow(dfv10g15)
# 19

mLfit.dfv10g15 = nlsLM(Decision ~ Lmod(Companions, a, b),data = dfv10g15, start=list(a=-0.5, b = 0.5))

summary(mLfit.dfv10g15) 
# Estimate Std. Error t value Pr(>|t|)
# a   0.1378     0.1354   1.018    0.323
# b  -5.1398     8.0003  -0.642    0.529
