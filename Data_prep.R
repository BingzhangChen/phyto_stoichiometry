library(tidyverse)
Semi <- read.csv('Semi_temp.csv') %>% 
  mutate(POP = POP*1e6) %>%   #unit: pmol P/cell
  mutate(POC = POC*1e6) %>%   #unit: pmol C/cell
  mutate(PON = PON*1e6) %>%   #unit: pmol N/cell
  mutate(Chl = Chl*1e3) %>%   #unit: pg Chl/cell
  mutate(C2P = POC/POP) %>%   #P:C molar ratio
  mutate(C2N = POC/PON) %>%   #N:C molar ratio
  mutate(N2P = PON/POP) %>%  #N:P molar ratio
  mutate(Chl2C = Chl/POC) %>%  #Chl:C ratio (gChl:molC)
  mutate(Temp= factor(Temp)) 

# Define outliers
Semi$flag <- F  #Define problematic measurements
x <- which(Semi$Temp==10 & Semi$Growth < 0.1)
Semi[x,]$flag <- TRUE
x <- which(Semi$Temp==30 & Semi$Growth < 0.3)
Semi[x,]$flag <- TRUE
rm(x)
