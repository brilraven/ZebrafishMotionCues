# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#		Data Analysis: Experiment 2 :: Spatial trends :: Fig. S2
#   Updated: Friday, June 13, 2018
#   Citation: Lemasson, B., Tanner, C. Woodley, C., Threadgill, T., Qarqish, S., and Smith, D. 2018. Motion Cues tune social influence in shoaling fish
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ===================================  
# workspace
# ===================================  
WorkSpace = './Workspaces/exp2_Spatial.RData'

# load(WorkSpace)

# save.image(WorkSpace)

# ===================================  
# library packages
# ===================================  
library(tidyverse)
library(ggplot2)
library(grid)  		      # for ggplot's 'unit' - used to scale legend's text

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ==================================================
# ----- Fish by zone at decision time
# ==================================================
NFxZoneData = read_tsv('./Data/Exp2_DecisiveFishData.tsv')

# --- Proportion of Neighbors by Zone x Decision
pXzXd.s <- NFxZoneData %>%
  subset(GroupSize > 1) %>%
  group_by(GroupSize, Decision) %>%
  summarise(NcorrectArm.m = mean(NFishCorrectArm),
            NincorrectArm.m = mean(NFishIncorrectArm),
            NholdingArm.m = mean(NFishHoldingArm),
            NdecisionZone.m = mean(NFishDecisionZone),
            mDecisionZoneTime = mean(DecisionTime)) %>%
  mutate(pCorrect = NcorrectArm.m/GroupSize,
         pIncorrect = NincorrectArm.m/GroupSize,
         pHolding = NholdingArm.m/GroupSize,
         pDecision = NdecisionZone.m/GroupSize) 

# ---- check that proportions x zone sum to 1
apply(pXzXd.s[,8:11], 1, sum) # yes

pXzXd.s[,8:11]
# A tibble: 6 x 4
#     pCorrect pIncorrect  pHolding pDecision
#         <dbl>      <dbl>     <dbl>     <dbl>
# 1 0.35333333 0.04666667 0.1600000 0.4400000
# 2 0.03076923 0.16923077 0.2461538 0.5538462
# 3 0.37391304 0.08260870 0.2913043 0.2521739
# 4 0.21764706 0.20588235 0.1941176 0.3823529
# 5 0.33571429 0.09523810 0.2904762 0.2785714
# 6 0.27142857 0.12380952 0.2714286 0.3333333

# --- assign basic spatial quadrants & recombine
pXzXd.s2 <- pXzXd.s[,c(1,2,8:11)]

d1 <- pXzXd.s2[,c(1:3)];   d1$Y = 2; d1$X = 0         # Correct arm (upper left)
d2 <- pXzXd.s2[,c(1:2,4)]; d2$Y = 2; d2$X = 2       # Incorrect arm (upper right)
d3 <- pXzXd.s2[,c(1:2,6)]; d3$Y = 1; d3$X = 1       # Decision zone (mid-center)
d4 <- pXzXd.s2[,c(1:2,5)]; d4$Y = 0; d4$X = 1       # Holding zone (bottom-center)

pXzXd.s3 <- bind_rows(d1, d2, d3, d4)

# --- checks [binding these categories (GroupSize x Decision) to space will create a 
#     bunch of 'dead' combinations, or NA's]. Make sure the remaining data are not corrupted
#     by comparing each p_Column between pXzXd.s2 & pXzXd.s3.
subset(pXzXd.s2,GroupSize == 5)
subset(pXzXd.s3,GroupSize == 5)

# --- Reformat from wide to long for plotting
pXzXd.s4 <- pXzXd.s3 %>% gather(key='zone', value='proportion',-GroupSize, -Decision, -X, -Y)

# ----- Remove NA (do NOT replace w/ zeros)
# 25% of this contains NA's 
# nrow(pXzXr.s4[!(is.na(pXzXr.s4$proportion)),])/nrow(pXzXr.s4)

pXzXd.s5 <- pXzXd.s4[!(is.na(pXzXd.s4$proportion)),]

# ---- check again
subset(pXzXd.s2,GroupSize == 5)
subset(pXzXd.s5,GroupSize == 5)


# ==================================
# Fig. SI-2 Neighbor distribution at decision time
# ==================================
# # --- rename Decision for plot
pXzXd.s5[pXzXd.s5$Decision == 0,]$Decision = "Incorrect"
pXzXd.s5[pXzXd.s5$Decision == 1,]$Decision = "Correct"

FigSI2_gpSpatialZoneXGrpSizeXDecision_SansText  <- ggplot(pXzXd.s5,aes(X,Y,fill=proportion)) +
  geom_tile(alpha=1) + 
  #geom_text(data = pdata,aes(x=X, y=Y,group = Decision, label = proportion))
  
  scale_fill_gradient("Proportion \n of \n shoal", low="white",high=cbbPalette[6],limits=c(0, 0.8),breaks=c(0.0,.2,0.4,0.6,0.8)) +                
  theme_classic() +
  facet_grid(Decision~GroupSize) +
  theme(legend.title = element_text(face = "bold",size=10),
        legend.title.align=0.5,
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_line(colour = "grey"),
        strip.text.x = element_text(size = 10), # face="bold", 
        strip.text.y = element_text(size = 10,angle=0), # face="bold",
        axis.title.x = element_text(size=9,vjust=-0.25), 
        axis.title.y = element_text(size=9,vjust= 1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave("./Figures/FigSI2_gpSpatialZoneXGrpSizeXDecision_SansText.png",FigSI2_gpSpatialZoneXGrpSizeXDecision_SansText,width=5, height=3, dpi=300)


# ---------------------------------
# values to juxtapose onto figure
# proportion present x group size x decision accuracy
# ---------------------------------

# --- Correct
subset(pXzXd.s5,Decision == "Correct" ) %>% arrange(GroupSize)
# A tibble: 12 x 6
# Groups: GroupSize [3]
#   GroupSize Decision     Y     X zone       proportion
#       <int> <chr>    <dbl> <dbl> <chr>           <dbl>
# 1         5 Correct   2.00  0    pCorrect       0.353 
# 2         5 Correct   2.00  2.00 pIncorrect     0.0467
# 3         5 Correct   1.00  1.00 pDecision      0.440 
# 4         5 Correct   0     1.00 pHolding       0.160 
# 5        10 Correct   2.00  0    pCorrect       0.374 
# 6        10 Correct   2.00  2.00 pIncorrect     0.0826
# 7        10 Correct   1.00  1.00 pDecision      0.252 
# 8        10 Correct   0     1.00 pHolding       0.291 
# 9        15 Correct   2.00  0    pCorrect       0.336 
# 10        15 Correct   2.00  2.00 pIncorrect     0.0952
# 11        15 Correct   1.00  1.00 pDecision      0.279 
# 12        15 Correct   0     1.00 pHolding       0.290 


# --- Incorrect
subset(pXzXd.s5,Decision == "Incorrect" ) %>% arrange(GroupSize)
# A tibble: 12 x 6
# Groups: GroupSize [3]
#   GroupSize Decision      Y     X zone       proportion
#       <int> <chr>     <dbl> <dbl> <chr>           <dbl>
# 1         5 Incorrect  2.00  0    pCorrect       0.0308
# 2         5 Incorrect  2.00  2.00 pIncorrect     0.169 
# 3         5 Incorrect  1.00  1.00 pDecision      0.554 
# 4         5 Incorrect  0     1.00 pHolding       0.246 
# 5        10 Incorrect  2.00  0    pCorrect       0.218 
# 6        10 Incorrect  2.00  2.00 pIncorrect     0.206 
# 7        10 Incorrect  1.00  1.00 pDecision      0.382 
# 8        10 Incorrect  0     1.00 pHolding       0.194 
# 9        15 Incorrect  2.00  0    pCorrect       0.271 
# 10        15 Incorrect  2.00  2.00 pIncorrect     0.124 
# 11        15 Incorrect  1.00  1.00 pDecision      0.333 
# 12        15 Incorrect  0     1.00 pHolding       0.271 




