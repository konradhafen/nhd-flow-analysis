
# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)
library(tidyverse)

setwd("E:/konrad/Projects/usgs/nhd-flow-estimates/wrk_data/obs/csv")
fn = "obs_flow.csv"
fn2 = "miller2018_disagree_strord.csv"

indat <- as.data.frame(read_csv(fn))


# functions ---------------------------------------------------------------

nhdclass <- function(fcode)
{
  if (fcode == 46006 | fcode == 55800)
  {
    return('Permanent')
  }
  else
  {
    return('Nonpermanent')
  }
}

misclass <- function(fcode, class)
{
  if ((fcode == 46006 | fcode == 55800) & class == 0)
  {
    return(1)
  }
  else if ((fcode == 46007 | fcode == 46003) & class == 1)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

disagree <- function(nhdclass, perm)
{
  if (nhdclass == 'Wet' & perm == 1)
  {
    return(0)
  }
  else if (nhdclass == 'Dry' & perm == 0)
  {
    return(0)
  }
  else
  {
    return(1)
  }
}


# Apply functions ---------------------------------------------------------

indat$nhdclass <- mapply(nhdclass, indat$FCODE)
indat$nhdmc <- mapply(misclass, indat$FCODE, indat$nhd_perm)
indat$obsmc <- mapply(misclass, indat$FCODE, indat$obs_perm)
indat$disnhd <- mapply(disagree, indat$Category, indat$nhd_perm)
indat$disobs <- mapply(disagree, indat$Category, indat$obs_perm)


# Subset data -------------------------------------------------------------

moddat <- indat[!(indat$Category=="Wet" & (indat$Month<8 | indat$Month>9)),]

# Disagreement ------------------------------------------------------------

#with nhd
1 - sum(moddat$disnhd)/nrow(moddat)

#with field obs
1 - sum(moddat$disobs)/nrow(moddat)

# Save csv ----------------------------------------------------------------

writeobs <- moddat[,c("FID", "COMID", "disnhd", "disobs")]
write_csv(writeobs, "miller2018_disagree.csv")


# Histogram of disagreement by stream order -------------------------------

plotdat <- as.data.frame(read_csv(fn2))
plotdat <- plotdat[!(plotdat$Category=="Wet" & (plotdat$Month<8 | plotdat$Month>9)),]
plotdat$barcolor <- as.character(plotdat$StreamOrde)
plotdat_sub <- plotdat[plotdat$disobs == 1 & !is.na(plotdat$StreamOrde),]
plotdat <- plotdat[!is.na(plotdat$StreamOrde)]
colors <- c('#00d1ec', '#00399d', '#00734d', '#02ac00', '#58c800', '#aee500', '#fffa00')
ggplot(NULL) +
  geom_histogram(data=plotdat_sub, aes(x=StreamOrde, y=..density..), binwidth=1, fill=colors, color='black') +
  geom_histogram(data=plotdat, aes(x=StreamOrde, y=..density..), binwidth=1, color='red', alpha=0.0) +
  labs(x='Stream order', y='Proportion') +
  scale_x_continuous(breaks=seq(1, 9, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=16, face='bold'), axis.text=element_text(size=12), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0)))
ggsave("C:/Users/khafe/Downloads/strord_miller2018.png", plot = last_plot(), width = 8, height = 6, units = "in", bg = "transparent")

library(reshape2)
plotdat <- as.data.frame(read_csv(fn2))
plotdat <- plotdat[!(plotdat$Category=="Wet" & (plotdat$Month<8 | plotdat$Month>9)), c("StreamOrde", "disobs")]
#plotdat <- indat[!(indat$Category=="Wet" & (indat$Month<8 | indat$Month>9)), c("StreamOrde", "mc")]
plotdat <- plotdat[!is.na(plotdat$StreamOrde),]
#plotdat <- indat[, c("StreamOrde", "mc")]
freq <- as.matrix(table(plotdat))
StreamOrder <- seq(1:7)
plotdf <- data.frame(cbind(freq), StreamOrder)
colnames(plotdf) <- c("Agree", "Disagree", "StreamOrder")
plotdf.melt <- melt(plotdf, id.vars="StreamOrder")

ggplot(plotdf.melt, aes(as.factor(StreamOrder), value)) + 
  geom_bar(aes(fill=variable), position="dodge", stat="identity") + 
  scale_fill_manual(values=c("#0c51fd", "#fb0026")) +
  labs(x="Stream Order", y="Count") + 
  ggtitle("Disagreement by Stream Order (NHDPlus-MR V2)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14), axis.text=element_text(size=12),
        plot.title=element_text(size=16, face="bold", margin=margin(t=0, r=0, b=20, l=0)),
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0))) +
  theme(legend.position =c(0.98,0.98), legend.justification = c(1,1), legend.title = element_blank())

ggsave("C:/Users/khafe/Downloads/disagreement_so_mr.png", plot = last_plot(), width = 10, height = 6, units = "in", bg = "transparent")

