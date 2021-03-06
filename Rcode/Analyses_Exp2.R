# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#		Data Analysis: Experiment 2 :: Group effects
#   Updated: Thursday June 14, 2018
#   Authors contributing to analyses: Bertrand Lemasson & Colby Tanner
#   Citation: Lemasson, B., Tanner, C. Woodley, C., Threadgill, T., Qarqish, S., and Smith, D. 2018. Motion Cues tune social influence in shoaling fish
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


sessionInfo()
# R version 3.4.1 (2017-06-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS High Sierra 10.13.5


# ===================================  
# workspace
# ===================================  
WorkSpace = './Workspaces/exp2.RData'

# load(WorkSpace)

# save.image(WorkSpace)

# ===================================  
# library packages & preferences
# ===================================  

source('./PackagesAndGlobalSettings.txt')


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Data import & inspection
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- Primary experimental results (speed and accuracy in Decisions ~ Speed & Coherency of stimuli)
data.e <- read_tsv("./Data/Exp2Data.tsv") 

data.e
# A tibble: 177 x 13
#           Date        Fish Speed GroupSize RewardArm ArmChosen Decision Time2A… Deci… NFis… NFis… NFis… NFis…
#           <date>     <int> <int>     <int> <chr>     <chr>        <int>   <dbl> <dbl> <int> <int> <int> <int>
#   1 2016-02-24     1     1         1 R         R                1    8.26 0.701     0     0     0     1



# --- Factors: Speed x GroupSize 
table(data.e[,c(3,4)])
#         GroupSize
# Speed  1  5 10 15
#     1  24 22 22 22
#     10 23 23 20 21


summary(data.e)
# --- NA's generated by fish that never made a Decision (coded as having a Decision = 999)
unique(data.e$Decision)
# [1]   1   0 999

# number of indecisive fish
nrow(data.e[data.e$Decision == 999,])
[1] 13

# --- Any repeat offenders? Yes, but not any consistent pattern
badFish <- data.e[data.e$Decision == 999,]
sort(badFish$Fish)

# [1]  2  4  9  9 13 13 16 19 20 21 21 23 23

# --- The majority of fish that failed to make a Decision were alone (8/13; 62%) and 
#     while there were some repeat offenders, none did so more than twice:

apply(table(badFish$Fish,badFish$GroupSize),2,sum)
# 1  5 10 15 
# 8  2  2  1 
# 



# --- create 2nd dataset w/out indecisive fish to test whether the presence/absence of these individuals affect the results
data.e2 <- data.e[data.e$Decision != 999,]

write_tsv(data.e2,'./Data/Exp2_DecisiveFishData.tsv')


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

(data.e2$ArmChosen) # fish that made a Decision
# L  R 
# 86 78 

binom.test(x=86,n=86+78,p=0.5) # N.S.



# --- Convert Fish to factor for linear models
data.e$Fish = factor(data.e$Fish)

# ---------------------------------------
# LMMs & GLMMs
# ---------------------------------------

# ==============================
# Time to act, TA
# ==============================

  # ---------------
  # quick plots
  # ---------------
  # ggplot(data.e, aes(y=Time2Act, x=GroupSize,color=factor(Speed))) + geom_jitter()
  #  ggplot(data.e, aes(y=Time2Act, x=factor(GroupSize),color=factor(Speed))) + geom_boxplot()
  #  ggplot(data.e, aes(y=Time2Act, x=factor(GroupSize))) + geom_boxplot()


mTA1.0 = lme(I(log(Time2Act)) ~ Speed * GroupSize,random = ~1|Fish, data=data.e)
anova(mTA1.0)
#                   numDF denDF   F-value p-value
# (Intercept)         1   150 246.11361  <.0001
# Speed               1   150   0.52815  0.4685
# GroupSize           1   150  29.00980  <.0001
# Speed:GroupSize     1   150   0.60102  0.4394

mTA2.0 = update(mTA1.0, ~. -Speed:GroupSize) 
anova(mTA2.0) 
#               numDF denDF   F-value p-value
# (Intercept)     1   151 245.56189  <.0001
# Speed           1   151   0.52982  0.4678
# GroupSize       1   151  29.09903  <.0001

mTA3.0 = update(mTA2.0, ~. -Speed) 
anova(mTA3.0)  
#             numDF denDF   F-value p-value
# (Intercept)     1   152 245.79107  <.0001
# GroupSize       1   152  29.26769  <.0001


# -----------------------
# diagnostics
# -----------------------
mod = mTA1.0

p1<- plot(mod,type=c("p","smooth")) # note, idLables don't seem to work for me
mdata2 <- data.frame(data.e,resid=residuals(mod,type="pearson"), fitted = fitted(mod))
p2 <- ggplot(mdata2,aes(x=factor(GroupSize), y=resid)) + geom_boxplot() + coord_flip()
p3 <- ggplot(mdata2,aes(x=factor(Speed), y=resid)) + geom_boxplot() + coord_flip()

E <- residuals(mod,type="pearson")

p4 <- ggplot(aes(x=resid), data = mdata2) + geom_histogram(fill="grey",color="black") + xlab("Residuals") 
p5 <- ggplot(aes(sample=resid), data = mdata2) + stat_qq()

grid.arrange(p1, p2, p3, p4, p5,nrow=2,top = textGrob("Time 2 Act"))



# --------------------------------------------------------------------------------
# Do the above results change if we only include fish that made a Decision? -No
# --------------------------------------------------------------------------------

mTA4.0 = lme(I(log(Time2Act)) ~ Speed * GroupSize,random = ~1|Fish, data=data.e2)
anova(mTA4.0)
#                 numDF denDF   F-value p-value
# (Intercept)         1   137 295.60106  <.0001
# Speed               1   137   0.04416  0.8339
# GroupSize           1   137  18.35646  <.0001
# Speed:GroupSize     1   137   0.72461  0.3961


mTA5.0 = update(mTA4.0, ~. -Speed:GroupSize) 
anova(mTA5.0) 
#               numDF denDF   F-value p-value
# (Intercept)     1   138 292.52151  <.0001
# Speed           1   138   0.04389  0.8344
# GroupSize       1   138  18.42781  <.0001

mTA6.0 = update(mTA5.0, ~. -Speed) 
anova(mTA6.0)  
#               numDF denDF   F-value p-value
# (Intercept)     1   139 292.29566  <.0001
# GroupSize       1   139  18.54369  <.0001


# ------------------------
# Power fit [medians]
# ------------------------
# need to undo factor assignment for log transformations
data.eL <- read_tsv("./Data/Exp2Data.tsv") 

# -- on means

TAmean.s <- data.eL %>%
  group_by(GroupSize) %>% 
  summarize(Time.mean = mean(Time2Act), Time.sd = sd(Time2Act)) %>%
  mutate(LTime.mean = log(Time.mean),LGroupSize = log(GroupSize))

# -- on medians
TAmed.s <- data.eL %>%
  group_by(GroupSize) %>% 
  summarize(Time.med = median(Time2Act), Time.mad = mad(Time2Act)) %>%
  mutate(LTime.med = log(Time.med),LGroupSize = log(GroupSize))

pfitMeans.lm <- lm(TAmean.s$LTime.mean ~ TAmean.s$LGroupSize)
summary(pfitMeans.lm)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)   
#   (Intercept)           5.8443     0.1889  30.945  0.00104 **
#   TAmean.s$LGroupSize  -0.5257     0.0968  -5.431  0.03227 * 
  

pfitMedians.lm <- lm(TAmed.s$LTime.med ~ TAmed.s$LGroupSize)
summary(pfitMedians.lm)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)         4.51715    0.10164   44.44 0.000506 ***
#   TAmed.s$LGroupSize -0.92315    0.05209  -17.72 0.003169 ** 

# --- remove extra data set
rm(data.eL)
      
# ==================================================
# ----- Plot: Fig. 3a
# ==================================================
  TA.s <- data.e %>%
          group_by(GroupSize) %>% 
          summarize(Time.mean = mean(Time2Act),Time.sd = sd(Time2Act), Time.cv = sd(Time2Act)/mean(Time2Act),
                    Time.med = median(Time2Act), Time.mad = mad(Time2Act)) %>%
          mutate(LTime.mean = log(Time.mean), LTime.med = log(Time.med),LGroupSize = log(GroupSize),
                 LNTime.mean = log(Time.mean/max(Time.mean)), LTime.med = log(Time.med/max(Time.med)))
      
  # ---- main figure
  Fig3a_TotalTimeXShoalSizeBoxplots <-  ggplot(data.e, aes(x = factor(GroupSize), y = Time2Act)) +
          #  gpTotalTimeXShoalSizeBoxplots_NoGrid <-  ggplot(data.e, aes(x = factor(GroupSize), y = Time2Act)) +
          
          geom_boxplot(fill = "slategrey", colour = "black", outlier.shape = NA) + 
          geom_jitter(size=0.25) +
          ylab("Time to act (s)") +
          ylab(expression(paste("Time to act, ", italic(T[A])))) +
          
          xlab("Shoal size") +
          theme_bw() + 
          #theme_classic() +
          theme(legend.title = element_text(size=10),
                #legend.key.size = unit(4, "cm"),
                panel.border = element_rect(fill = NA, colour = "black"),
                axis.title.x = element_text(size=10,vjust=-0.25),   # face="bold",
                axis.title.y = element_text(size=10,vjust= 1),
                axis.text.x = element_text(colour="black",size=9),
                axis.text.y = element_text(colour="black",size=9))

# ---- inset
  gpPwr <-    ggplot(TA.s, aes(x=LGroupSize,y=LTime.med)) +  
          geom_point(color=cbbPalette[6], size = 1.5) + geom_smooth(method="lm",alpha=0.3,size=0.5) +
          xlab(expression(paste(italic(L[10]),"(Shoal Size)"))) +
          ylab(expression(paste(italic(L[10]),"(", tilde(italic(T[A])),")"))) +
          xlim(0,3) +
          theme(panel.border = element_rect(fill = NA, colour = "black"),
                axis.title.x = element_text(size=7,vjust=-0.25), # face="bold",
                axis.title.y = element_text(size=7,vjust= 1),
                axis.text.x = element_text(colour="black",size=7),
                axis.text.y = element_text(colour="black",size=7))
        
  pinset <- viewport(width = 0.41, height = 0.41, x = 0.75, y = 0.71) #plot area for the main map
      
  # ---- combine
  tiff("./Figures/Fig3a.jpeg",w = 3,h = 3,units="in",res=100)
  print(Fig3a_TotalTimeXShoalSizeBoxplots)
  print(gpPwr,vp = pinset)
  dev.off()
      
      
      # Note - inset obsures an outlier for group size of 15:
      # A tibble: 2 x 7
      # G15Outliers = subset(data.e, GroupSize == 15 & Time2Act > 750,)
      # G15Outliers
      #     Trial TestDay Date                Fish  GroupSize Speed Time2Act
      #     <dbl>   <dbl> <dttm>              <fct>     <int> <int>        <dbl>
      #   1  8.00    15.0 2016-03-13 00:00:00 20           15    10          881
      #   2  8.00    24.0 2016-03-25 00:00:00 21           15    10         1200

# ==================================================
# ----- Decision time, TD
# ==================================================

# ---------------
# quick plots
# ---------------
# raw - outliers to remove, otherwise not very informative
ggplot(data.e2, aes(GroupSize,DecisionTime,color=factor(Speed))) + geom_point() + geom_jitter()
  ggplot(subset(data.e2,DecisionTime < 25), aes(GroupSize,DecisionTime,color=factor(Speed))) + geom_point() + geom_jitter()
        
  DTxR.s <- data.e2 %>%
            group_by(GroupSize,Decision) %>% 
            summarize(DT.m = mean(DecisionTime), DT.se = sd(DecisionTime)/sqrt(n())) 
       
  ggplot(data = DTxR.s, aes(x = GroupSize, y = DT.m, fill = factor(Decision))) +
    geom_errorbar(aes(ymin=DT.m - DT.se, ymax = DT.m + DT.se), width = 0.2,size=0.5) +
    geom_point(size=2,shape=21)
        
  # removing obvious ouliers cleans up existing trends but doesn't appear to change them 
  DTxR2.s <- data.e2 %>% subset(DecisionTime <=25) %>%
              group_by(GroupSize,Decision) %>% 
              summarize(DT.m = mean(DecisionTime), DT.se = sd(DecisionTime)/sqrt(n())) 
      
  ggplot(data = DTxR2.s, aes(x = GroupSize, y = DT.m, fill = factor(Decision))) +
    geom_errorbar(aes(ymin=DT.m - DT.se, ymax = DT.m + DT.se), width = 0.2,size=0.5) +
    geom_point(size=2,shape=21)
      
      
# Two rounds of model fitting: 2nd is with influential outliers removed
# mdata = data.e2 [1st round excludes those fish that never committed to a Decision]
mdata = data.e3 # [data.e2 with ouliers removed after model diagnostics for: ]
      

# mTD1.0 = lme(I(log(DecisionTime)) ~ Speed * GroupSize,random = ~1|Fish, data=mdata)
mTD1.0e3 = lme(I(log(DecisionTime)) ~ Speed * GroupSize,random = ~1|Fish, data=mdata)

# anova(mTD1.0)
anova(mTD1.0e3)
#                 numDF denDF  F-value p-value
# (Intercept)         1   135 34.71696  <.0001
# Speed               1   135  0.35466  0.5525
# GroupSize           1   135  0.32079  0.5721
# Speed:GroupSize     1   135  0.88763  0.3478

# mTD2.0 = update(mTD1.0,~.-Speed:GroupSize)
mTD2.0e3 = update(mTD1.0e3,~.-Speed:GroupSize)

#anova(mTD2.0)
anova(mTD2.0e3)
#             numDF denDF  F-value p-value
# (Intercept)     1   136 34.29542  <.0001
# Speed           1   136  0.35411  0.5528
# GroupSize       1   136  0.32139  0.5717


# mTD3.0 = update(mTD2.0,~.-Speed)
mTD3.0e3 = update(mTD2.0e3,~.-Speed)

#anova(mTD3.0)
anova(mTD3.0e3)
#             numDF denDF  F-value p-value
# (Intercept)     1   137 34.16334  <.0001
# GroupSize       1   137  0.33284  0.5649

  # -----------------------
  # diagnostics: contrast residuals for data.e (all decisive fish) vs. data.e3 (data.e2 w/ 3 outliers removed)
  # -----------------------
  # Steps: 
  # 1. diagnostics on data.e2 model for DecisionTimes
  # 2. idendify outliers based on Cook's D, remove, refit - residuals look better, but changes nothing - ignoring this step
  
  # [original: data.e2] mod = mTD1.0; 
  mod = mTD1.0e3
  
  p1<- plot(mod,type=c("p","smooth")) # note, idLables don't seem to work for me
  mdata2 <- data.frame(mdata,resid=residuals(mod,type="pearson"), fitted = fitted(mod))
  p2 <- ggplot(mdata2,aes(x=factor(GroupSize), y=resid)) + geom_boxplot() + coord_flip()
  p3 <- ggplot(mdata2,aes(x=factor(Speed), y=resid)) + geom_boxplot() + coord_flip()
  
  E <- residuals(mod,type="pearson")
  
  p4 <- ggplot(aes(x=resid), data = mdata2) + geom_histogram(fill="grey",color="black") + xlab("Residuals") 
  p5 <- ggplot(aes(sample=resid), data = mdata2) + stat_qq()
  
  # grid.arrange(p1, p2, p3, p4, p5,nrow=2,top = textGrob("Decision time: data.e2"))
  
  grid.arrange(p1, p2, p3, p4, p5,nrow=2,top = textGrob("Decision time: data.e3"))

  # -----------------------
  # oulier check/removal
  # -----------------------
  
  # outlier identification:
  CookD(mod,newwd = FALSE)
  
  # --- pluck out two, rather than 3, refit
  data.e2$index = seq(1,nrow(data.e2))
  
  data.e3 <- data.e2[!(data.e2$index %in% c(86, 157)),]
  
  # ==================================================
  # ----- Any evidence of an overall SAT?
  # ==================================================
  
  ggplot(data.e2, aes(DecisionTime,Decision)) + geom_point()
  
  mDAxS = glmer(Decision ~ DecisionTime + (1|Fish), data = data.e2, family='binomial', nAGQ = 15)
  
  summary(mDAxS)
  # Fixed effects:
  #                   Estimate Std. Error z value Pr(>|z|)  
  # (Intercept)      0.004134   0.393238   0.010   0.9916  
  # DecisionTime 0.384473   0.185226   2.076   0.0379 *
  
  # -----------------------
  # Decision Speed ~ Decision correlations x Shoal size 
  # -----------------------
  
  
  c1 <- subset(data.e3, GroupSize == 1)
  c5 <- subset(data.e3, GroupSize == 5)
  c10 <- subset(data.e3, GroupSize == 10)
  c15 <- subset(data.e3, GroupSize == 15)
  
  cor.test(c1$DecisionTime, c1$Decision) # N.S, P = 0.07, r = 0.30 
  cor.test(c5$DecisionTime, c5$Decision) # N.S, P = 0.07, r = 0.28
  cor.test(c10$DecisionTime, c10$Decision) # N.S, P = 0.60, r = 0.09
  cor.test(c15$DecisionTime, c15$Decision) # N.S, P = 0.71, r = 0.06
  
      
  # ==================================================
  # ----- Fig. 3b
  # ==================================================
      
# Decision accuracy x speed summary for plotting
  pDTxR.s <- DTxR.s
  names(pDTxR.s)[2] = "Decision"
  pDTxR.s[pDTxR.s$Decision == 0,]$Decision = "Incorrect"
  pDTxR.s[pDTxR.s$Decision == 1,]$Decision = "Correct"
      
      
  Fig3b_TDxDecision <- ggplot(data = pDTxR.s, aes(x = GroupSize, y = DT.m, fill = factor(Decision))) +
    geom_errorbar(aes(ymin=DT.m - DT.se, ymax = DT.m + DT.se), width = 0.2,size=0.5) +
    geom_point(size=2,shape=21) +
    scale_fill_manual(values = c("gray","slategrey"),"Decision") +
    scale_x_discrete("Shoal size", limits = c(1,5,10,15)) +
    ylab(expression(paste("Decision time, ", italic(T[D])))) +
    theme(legend.title = element_text(face="bold",size=9),
      legend.position = "top",
      legend.title.align = 0.5,
      axis.title.x = element_text(size=10,vjust=-0.25), # face="bold",
      axis.title.y = element_text(size=10,vjust= 1), # face="bold",
      axis.text.x = element_text(colour="black",size=9), # face="bold",
      axis.text.y = element_text(colour="black",size=9)) # face="bold",
      
  ggsave("./Figures/Fig3b_TDxDecision.jpeg", Fig3b_TDxDecision, width = 3, height = 3.25, dpi = 100)
      
# ==================================================
# ----- Decision accuracy
# ==================================================
   
# ---------------
# quick plots
# ---------------
      
  ggplot(data.e2, aes(GroupSize,Decision,color=factor(Speed))) + geom_point() + geom_jitter()
      
  # Decision accuracy x speed summary for plotting
  DA.s <- data.e2 %>%
            group_by(GroupSize, Speed) %>% 
            summarize(R.m = mean(Decision), R.se = sd(Decision)/sqrt(n())) 
              
  ggplot(DA.s, aes(GroupSize, R.m,color = factor(Speed))) + geom_point(size=1) + geom_errorbar(aes(ymin=R.m - R.se, ymax = R.m + R.se)) 
      
  DA2.s <- data.e2 %>%
            group_by(GroupSize) %>% 
            summarize(R.m = mean(Decision), R.se = sd(Decision)/sqrt(n())) 
      
  ggplot(DA2.s, aes(GroupSize, R.m)) + geom_point(size=1) + geom_errorbar(aes(ymin=R.m - R.se, ymax = R.m + R.se)) 
  
# -------------------------------
# Main GLMM
# -------------------------------
      
mDA1 <- glmer(Decision ~ Speed * GroupSize + (1|Fish), data = data.e2, family='binomial', nAGQ = 15)
summary(mDA1) 
# Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)     -0.051525   0.537003  -0.096   0.9236  
# Speed            0.179959   0.079512   2.263   0.0236 *
# GroupSize       -0.002398   0.051742  -0.046   0.9630  
# Speed:GroupSize -0.001950   0.008203  -0.238   0.8121        
      
mDA2 <- update(mDA1,~.-Speed:GroupSize)
summary(mDA2)
# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.01788    0.45126   0.040 0.968399    
# Speed        0.16457    0.04550   3.617 0.000298 ***
# GroupSize   -0.01119    0.03621  -0.309 0.757271  
      

mDA3 <- update(mDA2,~.-GroupSize)
summary(mDA3)
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.06969    0.35063  -0.199   0.8425    
# Speed        0.16426    0.04543   3.616   0.0003 ***


  
# how many fish choose correctly despite factor influence?
table(data.e2$Decision)
# 0   1 
# 58 106

binom.test(x=106,n=106+58,p=0.5)
# Exact binomial test
# 
# data:  106 and 106 + 58
# number of successes = 106, number of trials = 164, p-value = 0.0002207
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.5679863 0.7193012
# sample estimates:
#   probability of success 
# 0.6463415 
#  
# check on distribution of speed treatments
table(data.e2$Speed)
# 1 10 
# 86 78 

binom.test(86, 86+78, p=0.5) # - N.S.
     