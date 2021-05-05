#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

  

  ## ---calculations----

  # 32 https://pubmed.ncbi.nlm.nih.gov/29962895/
  ## get the epa beta
  ## using the different parametric distributions in the EPA documentation
  set.seed(5)
  expa <- rtruncnorm(1000, a = 0, mean = 1.42, sd = 0.89)
  expc <- rtruncnorm(1000, a = 0, mean = 1.2, sd = 0.49)
  expd <- triangle::rtriangle(1000, 0.1, 1.6, 0.95) 
  expe <- rtruncnorm(1000, a = 0, mean = 2, sd = 0.61)
  expg <- rtruncnorm(1000, a = 0, mean = 1, sd = 0.19)
  expi <- rtruncnorm(1000, a = 0, b = 2.273, mean = 1.25, sd = 0.53)
  expj <- rweibull(1000, 2.21, 1.41)
  epa <- c(expa, expc, expd, expe, expg, expi, expj)
  beta <- mean(epa / 100)
  
  pm_summ <- pm_summ %>% 
    mutate(paf = (exp(beta*pm)-1)) %>%
    group_by(Year, Race, Hispanic.Origin) %>%
    summarise(paf =  weighted.mean(paf, pop_size))
  
  attrBurden <- inner_join(total_burden, pm_summ, by = c("Year", "Race", "Hispanic.Origin")) %>%
    mutate(value = value * paf, value = NULL, paf = NULL)
  
  print(attrBurden)