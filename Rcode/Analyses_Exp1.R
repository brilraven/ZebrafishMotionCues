# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#		Data Analysis: Experiment 1 :: Individual effects
#   Updated: Thursday June 14, 2018
#   Authors contributing to analyses: Bertrand Lemasson & Colby Tanner
#   Citation: Lemasson, B., Tanner, C. Woodley, C., Threadgill, T., Qarqish, S., and Smith, D. 2018. Motion Cues tune social influence in shoaling fish
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ===================================  
# workspace
# ===================================  
WorkSpace = './Workspaces/exp1.RData'

# load(WorkSpace)

# save.image(WorkSpace)

# ===================================  
# library packages & preferences
# ===================================  

source('./PackagesAndGlobalSettings.txt')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Data import & inspection: 
#   main factor results & tracking data
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- Primary experimental results (speed and accuracy in decisions ~ Speed & Coherency of stimuli)
data.e <- read_tsv("./Data/Exp1MainData.tsv") 
# A tibble: 149 x 8
#         Date    Fish Speed Coherency RewardArm ArmChosen Decision DecisionTime
#       <date>    <int> <int>   <dbl>     <int>     <int>    <int>        <dbl>
# 1 2015-02-24     1     1     0.330         1         1        1         2.67
# 2 2015-03-02     1     1     0.660         1         1        1         1.75



# ---------------------------------------
# --- Factors/Variables
# Fish - fish id number
# (factors): 
#   Speed (1, 10), 
#   Coherency (0,0.33, 0.66, 1)
#
# (Decision variables): 
#   ArmChosen - arm selected by subject zebrafish (1, 2)
#   Decision - correct/incorrect choice (match between Reward & Chosen arms)
#   DecisionTime - time taken by zebrafish subject to traverse the decision zone
# ---------------------------------------

# --- Tracking data
# You need to specify column types to avoid parsing issues 
colT <- cols(
  Date = col_date(format = ""),
  Fish = col_integer(),
  Speed = col_integer(),
  Coherency = col_double(),
  Decision = col_integer(),
  Frame = col_integer(),
  xpos = col_double(),
  ypos = col_double(),
  z.spd = col_double(),
  turnangle.c = col_double(),
  zone = col_character(),
  CDT.y = col_double(),
  CDT = col_double(),
  CTTxZONE = col_double()
)

data.t <- read_tsv("./Data/Exp1TracksKins.tsv", col_types = colT)

data.t
# A tibble: 8,031 x 13
#     Date        Fish Speed Coherency Decision Frame  xpos  ypos z.spd turnangle.c zone    CDT.y   CDT
#     <date>     <int> <int>     <dbl>    <int> <int> <dbl> <dbl> <dbl>       <dbl> <chr>   <dbl> <dbl>
#   1 2015-04-06     5     1         0        1     1   978   138 NA        NA      HOLDING   0     0  
#   2 2015-04-06     5     1         0        1     2   992   150  6.42     NA      HOLDING  12.0  18.4


# ---------------------------------------
# --- Factors/Variables
# Fish - fish id number
# (factors): 
#   Speed (1, 10), 
#   Coherency (0,0.33, 0.66, 1)
#
# (track data): 
#   z.spd - fish speed in body lengths/s
#   zone - fish location in maze in a given frame (decision, holding)
#   CDT - cummulative distance travelled from the holding area
#   CDT.y - CDT along the y-axis only

# ---------------------------------------

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Data analyses
#
#   1. Tests for directional biases
#   2. LMM & GLMM of main factors

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ==============================
# Movement bias? - No 
# ==============================

# --- bias in leader silhouettes?
table(data.e$RewardArm)
# 1  2 
# 78 71 

binom.test(x=78,n=78 + 71,p=0.5) # N.S. 

# --- bias in subjects across all  conditions?
table(data.e$ArmChosen)
# 1  2 
# 65 84 

binom.test(x=65,n=65 + 84,p=0.5) # N.S. 

# --- bias in subjects under null  condition (Coherency = 0)?
table(data.e[data.e$Coherency == 0,]$ArmChosen)
# 1  2 
# 16 24 

binom.test(x=16,n=16 + 24,p=0.5)  # N.S.


# ---------------------------------------
# LMMs & GLMMs
# ---------------------------------------

# --- Treat Fish id a factor 
data.e$Fish = as.factor(data.e$Fish)


# ==============================
# Decision Speed 
# ==============================

# extract time taken to travel by zone & retain only data pertaining to the decision zone
# Decision is included in the group_by() to keep it in the dataframe

data.t.s <- data.t %>%
  group_by(Date,Fish, Speed, Coherency, zone, Decision) %>%
  summarise(Tmax = max(CTTxZONE)) %>%
  mutate(Day = day(Date)) %>%
  filter(zone == "DECISION") %>%
  arrange(Date, Day, Fish, Speed, Coherency, zone, Decision, Tmax)

# ---------------
# quick plots
# ---------------
  # ggplot(data.t.s, aes(y=log(Tmax), x=Coherency,color=factor(Speed))) + geom_point()

# summarize
data.tS.s <- data.t.s %>% 
  group_by(Speed,Coherency) %>% 
  summarise(T.m = mean(Tmax), T.var = var(Tmax), T.se = sd(Tmax)/sqrt(n())) 

ggplot(data.tS.s, aes(y=T.m, x=Coherency,color=factor(Speed))) + 
  geom_errorbar(aes(ymax = T.m + T.se, ymin=T.m - T.se), width=0.02) + geom_point()

# -------------------------------
# LMM: Time to Decide, TD - N.S. No interaction between Coherency & Speed, no effect of either factor 
# -------------------------------
mTD1.0 = lme(I(log(Tmax)) ~ Speed * Coherency, random = ~1|Fish, data=data.t.s)
anova(mTD1.0)
  #                 numDF denDF  F-value p-value
  # (Intercept)         1   126 64.35714  <.0001
  # Speed               1   126  0.02896  0.8651
  # Coherency           1   126  0.80392  0.3716
  # Speed:Coherency     1   126  0.53561  0.4656

mTD2.0 = update(mTD1.0,~.-Speed:Coherency)
anova(mTD2.0)

  #               numDF denDF  F-value p-value
  # (Intercept)     1   127 64.37983  <.0001
  # Speed           1   127  0.02912  0.8648
  # Coherency       1   127  0.80665  0.3708

mTD3.0 = update(mTD2.0,~.-Speed)
anova(mTD3.0) 

  #             numDF denDF  F-value p-value
  # (Intercept)     1   128 64.44050  <.0001
  # Coherency       1   128  0.81672  0.3678

# -----------------------
# diagnostics
# -----------------------
mod = mTD1.0

p1<- plot(mod,type=c("p","smooth")) 
mdata2 <- data.frame(data.t.s,resid=residuals(mod,type="pearson"), fitted = fitted(mod))
p2 <- ggplot(mdata2,aes(x=factor(Coherency), y=resid)) + geom_boxplot() + coord_flip()
p3 <- ggplot(mdata2,aes(x=factor(Speed), y=resid)) + geom_boxplot() + coord_flip()

E <- residuals(mod,type="pearson")

p4 <- ggplot(aes(x=resid), data = mdata2) + geom_histogram(fill="grey",color="black") + xlab("Residuals") 
p5 <- ggplot(aes(sample=resid), data = mdata2) + stat_qq()

grid.arrange(p1, p2, p3, p4, p5,nrow=2,top = textGrob("Decision time: model mTD1.0"))


# ==============================
# Decision Accuracy 
# ==============================


# Summary:
# Main model - Signif. interaction between coherency & speed
# Post-Hoc (1)- Speed by itself has a significant effect
# Post-Hoc (2) - Coherency levels are not sign. diff. from 0 in the speed = 1 data
# Post-Hoc (3) - Coherency levels are all sign. diff. from 0 in the speed = 10 data
# Post-Hoc (4) - Coherency levels > 0 are not signif. diff. from one another in the speed = 20 data.
#
# -   Speed drives the process. At speed = 1, there is effectively no effect on directional accuracy
#     When Speed = 10, accuracy jumps up significantly across coherency levels equally


# ---------------
# quick plots
# ---------------
# [not very useful] ggplot(data.e, aes(y=Decision, x=Coherency,color=factor(Speed)))  + geom_jitter()

# summarize
data.eA.s <- data.e %>% 
  group_by(Speed,Coherency) %>% 
  summarise(D.m = mean(Decision), D.var = var(Decision), D.se = sd(Decision)/sqrt(n())) 

ggplot(data.eA.s, aes(y=D.m, x=Coherency,color=factor(Speed))) + 
  geom_errorbar(aes(ymax = D.m + D.se, ymin=D.m - D.se), width=0.02) + geom_point()

# -------------------------------
# Main GLMM
# -------------------------------

mDA1.0 <- glmer(Decision ~ Speed * Coherency + (1|Fish), data = data.e, family='binomial')

summary(mDA1.0)

  # Fixed effects:
  #                 Estimate Std. Error z value Pr(>|z|)   
  # (Intercept)     -0.23170    0.42761  -0.542  0.58792   
  # Speed           -0.02169    0.06207  -0.350  0.72673   
  # Coherency       -0.55163    0.70383  -0.784  0.43318   
  # Speed:Coherency  0.35161    0.11670   3.013  0.00259 **

# -------------------------
# Post-hoc analyses (1)
# -------------------------
# Did fish behave differently than expected within speed treatment levels

# --- speed = 1
table(data.e[data.e$Speed == 1 & data.e$Coherency > 0.00,]$Decision)
# 0  1 
# 31 24 

binom.test(x=24,n=24 + 31,p=0.5)
# Exact binomial test
# 
# data:  24 and 24 + 31
# number of successes = 24, number of trials = 55, p-value = 0.4188

# --- speed = 10
table(data.e[data.e$Speed == 10 & data.e$Coherency > 0.00,]$Decision)
# 0  1 
# 8 46 

binom.test(x=46,n=8 + 46,p=0.5)
# Exact binomial test
# 
# data:  46 and 8 + 46
# number of successes = 46, number of trials = 54, p-value = 1.384e-07

# -------------------------
# : Speed only model
# -------------------------

mDA2.0 <- glmer(Decision ~ Speed  + (1|Fish), data = data.e, family='binomial')
summary(mDA2.0)

# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.47760    0.26203  -1.823 0.068357 .  
# Speed        0.12739    0.03818   3.337 0.000848 ***

# -------------------------
# Post-hoc analyses (2): Coherency effects @ Speed 1
# -------------------------

mDA3.0 <- glmer(Decision ~ Coherency  + (1|Fish), data = subset(data.e, Speed == 1), family='binomial')
summary(mDA3.0)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -0.2824     0.4244  -0.666    0.506
# Coherency    -0.2397     0.6584  -0.364    0.716

# -------------------------
# Post-hoc analyses(3): Coherency effects @ Speed 10
# -------------------------

mDA4.0 <- glmer(Decision ~ Coherency  + (1|Fish), data = subset(data.e, Speed == 10), family='binomial')
summary(mDA4.0)

# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -0.4486     0.4070  -1.102 0.270323    
# Coherency     2.9645     0.8420   3.521 0.000431 ***

# -------------------------
# Bonferroni adjustment
# -------------------------
p.adjust(c(0.716,0.0004), method="BH")  # to correct for splitting the data and doing 2 tests
# 0.7160 0.0008

# -------------------------
# Post-hoc analyses(4): Tukey test on coherency levels within mDA4.0 Hot
# -------------------------

# for pairwise comparison, we need to treat Coherency as a factor
mdata = data.e %>% subset(Speed == 10)
mdata$Coherency = as.factor(mdata$Coherency)

mDA4.1 = glmer(Decision ~ Coherency + (1|Fish), data = mdata, family='binomial')

summary(glht(mDA4.1, linfct = mcp(Coherency='Tukey')), test=adjusted('BH'))

# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)   
# 0.33 - 0 == 0      2.5649     0.8228   3.118  0.00365 **
# 0.66 - 0 == 0      3.9890     1.1499   3.469  0.00313 **
# 1 - 0 == 0         2.4204     0.7638   3.169  0.00365 **
# 0.66 - 0.33 == 0   1.4240     1.2107   1.176  0.28742   
# 1 - 0.33 == 0     -0.1446     0.8526  -0.170  0.86534   
# 1 - 0.66 == 0     -1.5686     1.1714  -1.339  0.27082   

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Figurs
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# =======================
# ----- Fig. 2a
# =======================


gpFig2a_Accuracy <- ggplot(data.eA.s, aes(y=R.m, x=Coherency,color=factor(Speed))) + 
  geom_line() +             # aes(linetype = factor(Speed))
  geom_errorbar(aes(ymax = R.m + R.se, ymin=R.m - R.se), width=0.02) +
  geom_point() + 
  scale_color_manual(values = cbbPalette[6:7], 
                     guide = guide_legend(title = expression(paste("Virtual leader speeds, ",Delta~italic(v))))) +
  scale_fill_hue(l=40) +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  ylab("Decision accuracy") +
  xlab("Coherency") +
  
  theme(legend.title = element_text(face = "bold",size=9, hjust=0),
        legend.position = "top",
        legend.title.align=0.5,
        legend.box.spacing = margin(t = 0, b = 0, unit = "in"),
        
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_line(colour = "grey"),
        axis.text.x = element_text(size = 9,color="black"),
        axis.text.y = element_text(size = 9,color="black"),
        axis.title.x = element_text(face = "bold",size=9,vjust=-0.25), 
        axis.title.y = element_text(face = "bold",size=9,vjust= 1)) 


ggsave(gpFig2a_Accuracy, file = "./Figures/Fig2a_Exp1_Accuracy_SE.jpeg",units="in",width = 3,height=3.25,dpi=100)

####################################
# Figure  2, panels b-e
####################################

# ----- pixel conversion to cm (based on the average fish length measured in cm (3.5) and pixels (36.5))
# 3.5 cm/36.5 pixels ~ 0.1 cm/pixel
c1 = 0.09589041
# 350.85 - location of the decision zone (in pixels)

#[original] pd = data.t 
pd = data.t %>% subset(Coherency > 0)

pd$Speed = factor(pd$Speed)
levels(pd$Speed) <- c(expression(Delta~italic(v)==1), expression(Delta~italic(v)==10))

gpS <- ggplot(aes(x = CDT*c1, y = z.spd*3.515342,colour = factor(Decision), fill = factor(Decision), name="Decision"), data = pd) + 
  
        geom_smooth(alpha = 0.5) + # , method = "loess"
        geom_vline(xintercept = c(350.85)*c1,linetype=2, size = 0.5) +
        scale_color_manual(values=c("grey", "slategrey"), name="Decision",labels=c("Incorrect","Correct")) +
        scale_fill_manual(values=c("grey", "slategrey"), name="Decision",labels=c("Incorrect","Correct")) +
        facet_grid(.~Speed, labeller = label_parsed) + 
        #xlab("Distance traveled (cm)") +
        xlab("") +
        ylab("Fish speed (cm/s)") +
        theme(legend.title = element_text(size=10),
              legend.position = "top",
              #legend.background = element_rect(colour ="darkblue"),
              legend.box.spacing = margin(t = 0, b = 0, unit = "in"),
              axis.title.x = element_text(face="bold",size=10,vjust=-0.25), 
              axis.title.y = element_text(face="bold",size=10,vjust= 1),
              axis.text.x = element_text(colour="black",size=9),
              axis.text.y = element_text(colour="black",size=9))


# -------------------------------------
# turning arc as a function of 
# distance traveled
# -------------------------------------

gpT <- ggplot(aes(x = CDT*c1, y = sin(turnangle.c),colour = factor(Decision), fill = factor(Decision), name="Decision"), data = pd) +  
  
        geom_smooth(alpha = 0.5) +
        geom_vline(xintercept = c(350.85)*c1,linetype=2, size = 0.5) +
        scale_color_manual(values=c("grey", "slategrey"), name="Decision",labels=c("Incorrect","Correct")) +
        scale_fill_manual(values=c("grey", "slategrey"), name="Decision",labels=c("Incorrect","Correct")) +
        facet_grid(.~Speed, labeller = label_parsed) + 
        coord_cartesian(ylim=c(0, 0.3)) +
        xlab("Distance traveled (cm)") +
        ylab("Sine (Turning arc)") +
        theme(legend.title = element_text(size=10),
              legend.position = "none",
              axis.title.x = element_text(face="bold",size=10,vjust=-0.25), 
              axis.title.y = element_text(face="bold",size=10,vjust= 1),
              axis.text.x = element_text(colour="black",size=9),
              axis.text.y = element_text(colour="black",size=9))
      
ggsave(gpS, file = "./Figures/gpFig2_bd.jpeg",units="in",width = 3.5,height=2.25,dpi=100)
ggsave(gpT, file = "./Figures/gpFig2_ce.jpeg",units="in",width = 3.5,height=2,dpi=100)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Any evidence of a SAT here?
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# -------------------------------------------------------------------
# exploratory plots v1 & v10 together

df.p = data.t %>% subset(zone == 'DECISION')

df.p = data.t %>% subset(Coherency > 0 & zone == 'DECISION')
df.p$Speed = factor(df.p$Speed)
levels(df.p$Speed) <- c(expression(Delta~italic(v)==1), expression(Delta~italic(v)==10))


df.p.s =  df.p %>%
  group_by(Fish, Speed,Decision) %>%
  summarise(Tmax = max(CTTxZONE))

satBoxPlots = ggplot(df.p.s, aes(factor(Decision),Tmax, fill=Speed)) + geom_boxplot() +
  scale_fill_manual(values=c("#cccccc", "#636363"), labels=c("Incorrect","Correct")) +
  xlab("Decision score") + ylab("Decision Time (s)") + scale_x_discrete(labels=c("Incorrect", "Correct")) +
  facet_grid(.~Speed, labeller = label_parsed) + 
  theme(legend.position = 'none')

ggsave(satBoxPlots, file = "./Figures/SI_SATBoxPlts.jpeg",units = "in",width = 5,height=2.5, dpi=100)  

# -----------------------
# Speed = 1
# -----------------------

df1 = data.t %>% subset(Speed == 1  & zone == 'DECISION')

df1.s =  df1 %>%
  group_by(Fish, Coherency,Decision) %>%
  summarise(Tmax = max(CTTxZONE), spd.m = mean(z.spd), spd.sd = sd(z.spd))

# N.S.
wilcox.test(subset(df1.s, Decision == 0 & Coherency > 0)$Tmax, subset(df1.s, Decision == 1 & Coherency > 0)$Tmax) # N.S.


# -----------------------
# Speed = 10
# -----------------------

df10 = data.t %>% subset(Speed == 10 & zone == 'DECISION')


df10.s =  df10 %>%
  group_by(Fish, Coherency,Decision) %>%
  summarise(Tmax = max(CTTxZONE))


wilcox.test(subset(df10.s, Decision == 0 & Coherency > 0)$Tmax, subset(df10.s, Decision == 1 & Coherency > 0)$Tmax)
# Wilcoxon rank sum test with continuity correction
# 
# data:  subset(df10.s, Decision == 0 & Coherency > 0)$Tmax and subset(df10.s, Decision == 1 & Coherency > 0)$Tmax
# W = 79, p-value = 0.01086
# alternative hypothesis: true location shift is not equal to 0

####################################
# General summary stats for v = 1 VS. v = 10
####################################


mean(subset(df1.s, Coherency > 0 & Decision == 0)$Tmax)
# [1] 2.268817
sd(subset(df1.s, Coherency > 0 & Decision == 0)$Tmax)
# [1] 1.585774
mean(subset(df1.s, Coherency > 0 & Decision == 1)$Tmax)
# [1] 2.246528
sd(subset(df1.s, Coherency > 0 & Decision == 1)$Tmax)
# [1] 1.412393

# ------------------
mean(subset(df10.s, Coherency > 0 & Decision == 0)$Tmax)
# [1] 1.3
sd(subset(df10.s, Coherency > 0 & Decision == 0)$Tmax)
# 0.77
mean(subset(df10.s, Coherency > 0 & Decision == 1)$Tmax)
# [1] 2.550725
sd(subset(df10.s, Coherency > 0 & Decision == 1)$Tmax)
# [1] 1.962808



