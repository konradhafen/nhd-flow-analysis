
# Do setup ----------------------------------------------------------------

rm(list=ls())
setwd("E:/konrad/Projects/usgs/nhd-flow-estimates/wrk_data/analysis/validation")
fn <- "min_cfs_threshold_5070_streamcat.csv"

indat <- read.csv(fn)
indat <- na.omit(indat)


# Functions ---------------------------------------------------------------

library(verification)
class.sum <- function(truth, predicted)
{
  xt<-table(truth,round(predicted+0.000001))
  if (ncol(xt) < 2)
  {
    if (colnames(xt) == "0")
    {
      xt <- cbind(xt, "1"=c(0,0))
    }
    if (colnames(xt) == "1")
    {
      xt <- cbind(xt, "0"=c(0,0))
      xt <- xt[,c("0", "1")]
    }
  }
  pcc<-round(100*sum(diag(xt))/sum(xt),2)
  spec<-round(100*xt[1,1]/sum(xt[1,]),2)
  sens<-round(100*xt[2,2]/sum(xt[2,]),2)
  kap<-round(kappa(xt)[1],4)
  au<-round(roc.area(truth,predicted)$A,4)
  return(cbind(c("Percent Correctly Classified = ","Specificity = ","Sensitivity = ","Kappa =","AUC= "),c(pcc,spec,sens,kap,au)))
}


# Table of all univariate logistic regression models ----------------------

lrdat.all <- indat
out.dat <- data.frame(var=character(), coef=double(), p=double(), aic=double(), pr2=double(),
                      pcc=double(), spec=double(), sens=double(), auc=double())
for (i in 6:ncol(lrdat.all))
{
  #print(i)
  lr1 <- glm(lrdat.all[,5] ~ lrdat.all[,i], family=binomial(link = "logit"))
  #print("lr done")
  lr.sum <- summary(lr1)
  #print("summary done")
  pr2 <- pR2(lr1)
  #print("pr2 done")
  predicted <- predict(lr1, indat, type="response")
  #print("predicted done")
  sumtab <- class.sum(indat$disagree, predicted)
  #print("class sum done")
  row <- data.frame(var=colnames(lrdat.all)[i], coef=lr.sum$coefficients[2], p=round(lr.sum$coefficients[8], digits=5), 
                    pr2=pr2[4], pcc=sumtab[1,2], spec=sumtab[2,2], sens=sumtab[3,2], auc=sumtab[5,2])
  #print(row)
  out.dat <- rbind(out.dat, row)
}
out.dat


# Correlations ------------------------------------------------------------

#Elevation, precipitation
cor(indat$ElevCat, indat$Precip8110Ws)
#Elevation, BFI
cor(indat$ElevCat, indat$BFIWs)
#Elevation, watershed area
cor(indat$ElevCat, indat$WsAreaSqKm)



# Exploratory Logistic Regression -----------------------------------------

library(bbmle)

lr.wd <- glm(disagree ~ Category, data = indat, family=binomial)
lr.bfi <- glm(disagree ~ BFIWs, data = indat, family=binomial)
lr.bfityp <- glm(disagree ~ BFIWs + Category, data = indat, family=binomial)
lr.bficat <- glm(disagree ~ BFICat, data = indat, family=binomial)
lr.area <- glm(disagree ~ WsAreaSqKm, data = indat, family=binomial)
lr.areatyp <- glm(disagree ~ WsAreaSqKm + Category, data = indat, family=binomial)
lr.areacat <- glm(disagree ~ CatAreaSqKm, data = indat, family=binomial)
lr.wdbfi <- glm(disagree ~ Category + BFICat, data = indat, family=binomial)
lr.bfiarea <- glm(disagree ~ BFICat + WsAreaSqKm, data = indat, family=binomial)
lr.wtdcat <- glm(disagree ~ WtDepCat, data = indat, family=binomial)
lr.wtdws <- glm(disagree ~ WtDepWs, data = indat, family=binomial)
lr.rdcat <- glm(disagree ~ RckDepCat, data = indat, family=binomial)
lr.rdws <- glm(disagree ~ RckDepWs, data = indat, family=binomial)
lr.pptcat <- glm(disagree ~ Precip8110Cat, data = indat, family=binomial)
lr.pptws  <- glm(disagree ~ Precip8110Ws, data = indat, family=binomial)
lr.pptwstyp  <- glm(disagree ~ Precip8110Ws + Category, data = indat, family=binomial)
lr.pptbfi <- glm(disagree ~ Precip8110Ws + BFICat, data = indat, family=binomial)
lr.pptbfiarea <- glm(disagree ~ Precip8110Ws + BFICat + WsAreaSqKm, data = indat, family=binomial)
lr.elev <- glm(disagree ~ ElevCat, data = indat, family=binomial)
lr.elevtyp <- glm(disagree ~ ElevCat + Category, data = indat, family=binomial)
lr.k <- glm(disagree ~ HydrlCondWs, data = indat, family=binomial)
lr.ktyp <- glm(disagree ~ HydrlCondWs + Category, data = indat, family=binomial)

AICctab(lr.wd, lr.bfi, lr.bficat, lr.area, lr.areacat, lr.wdbfi, lr.bfiarea, lr.wtdcat, lr.wtdws,
        lr.rdcat, lr.rdws, lr.pptcat, lr.pptws, lr.pptbfi, lr.pptbfiarea, lr.bfityp, lr.pptwstyp,
        lr.areatyp, lr.elev, lr.elevtyp, lr.k, lr.ktyp)

predicted <- predict(lr.pptbfiarea, indat, type="response")
cs <- class.sum(indat$disagree, predicted)

# Logistic Regression Plots -----------------------------------------------

library(broom)
model.data <- augment(lr.bfityp) %>% mutate(index = 1:n())
ggplot(model.data, aes(BFIWs, plogis(.fitted))) +
  geom_line(aes(color=Category)) + 
  geom_point(aes(BFIWs, disagree, colour=Category), alpha=0.1) +
  scale_x_continuous(breaks=seq(0,100,5)) +
  scale_color_manual(values=c("#fb0026", "#0c51fd")) +
  #facet_wrap(~panelname, ncol=3) + 
  labs(x = "BFI", y = "Probability of disagreement", color = "Observation type")

model.data <- augment(lr.pptwstyp) %>% mutate(index = 1:n())
ggplot(model.data, aes(Precip8110Ws, plogis(.fitted))) +
  geom_line(aes(color=Category)) + 
  geom_point(aes(Precip8110Ws, disagree, colour=Category), alpha=0.1) +
  #scale_x_continuous(breaks=seq(0,100,5)) +
  scale_color_manual(values=c("#fb0026", "#0c51fd")) +
  #facet_wrap(~panelname, ncol=3) + 
  labs(x = "Mean annual precip (mm)", y = "Probability of disagreement", color = "Observation type")

model.data <- augment(lr.areatyp) %>% mutate(index = 1:n())
ggplot(model.data, aes(WsAreaSqKm, plogis(.fitted))) +
  geom_line(aes(color=Category)) + 
  geom_point(aes(WsAreaSqKm, disagree, colour=Category), alpha=0.1) +
  #scale_x_continuous(breaks=seq(0,100,5)) +
  scale_color_manual(values=c("#fb0026", "#0c51fd")) +
  #facet_wrap(~panelname, ncol=3) + 
  labs(x = "Watershed area (sq km)", y = "Probability of disagreement", color = "Observation type")

model.data <- augment(lr.elevtyp) %>% mutate(index = 1:n())
ggplot(model.data, aes(ElevCat, plogis(.fitted))) +
  geom_line(aes(color=Category)) + 
  geom_point(aes(ElevCat, disagree, colour=Category), alpha=0.1) +
  #scale_x_continuous(breaks=seq(0,100,5)) +
  scale_color_manual(values=c("#fb0026", "#0c51fd")) +
  #facet_wrap(~panelname, ncol=3) + 
  labs(x = "Mean catchment elevation (m)", y = "Probability of disagreement", color = "Observation type")


