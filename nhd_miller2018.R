
# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)
library(tidyverse)

setwd("E:/konrad/Projects/usgs/nhd-flow-estimates/wrk_data/obs/csv")
fn = "obs_flow.csv"

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
