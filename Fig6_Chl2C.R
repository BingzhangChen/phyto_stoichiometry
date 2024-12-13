#Fig. 6: cellular Chl:C ratio against temperature and growth rate
#Run a linear model to predict Chl:C ratio using temperature, 
#growth rate, and the type of limiting nutrient
dat <- Semi %>% 
  filter(flag == F) %>% 
  filter(!is.na(Growth))

Chl2C_lm <- step(lm(Chl2C ~ Growth + Temp + Lim +
                            Growth:Temp + Growth:Lim +
                            Growth:Temp:Lim,
                    data = dat))
summary(Chl2C_lm)
anova(Chl2C_lm)

#Check residuals
par(mfrow=c(2,2))
plot(Chl2C_lm, ask = F)
car::vif(Chl2C_lm)

#Check boxcox transformation
par(mfrow=c(1,1))
MASS::boxcox(Chl2C_lm)

pdf('Fig6_ChlC_Plim.pdf',width=6,height=6)
op     <- par(font.lab=1,
              family ="serif",
              mar    = c(3.2,3.5,.1,.1),
              mgp    = c(2.2,1,0),
              mfrow  = c(1,1),
              oma    = c(2,2,1,1))
ylab    <- expression(paste('Chl:C (gChl '*molC^-1*' )'))
Temps   <- unique(Semi$Temp)
limtype <- unique(Semi$Lim)
xlim    <- c(0, max(Semi$Growth, na.rm=T))
ylim    <- c(0.1*min(Semi$Chl2C,na.rm=T),
             1.3*max(Semi$Chl2C,na.rm=T))

#Add regression lines
newr <- seq(xlim[1], xlim[2],0.01)
newx <- expand.grid(Growth = newr,
                    Temp   = Temps,
                    Lim    = limtype)

newx$Chl2C <- predict(Chl2C_lm,newx)

plot(Semi$Growth,Semi$Chl2C,
 xlab = expression(paste('Growth rate (',d^-1*')')),
 ylab = ylab,
 xlim = xlim, 
 ylim = ylim,
 type = 'n')
 
for (i in 1:length(limtype)){
  for (j in 1:length(Temps)){
    tmp <- Semi %>% 
      filter(Lim  == limtype[i]) %>% 
      filter(Temp == Temps[j])
    points(tmp$Growth, tmp$Chl2C,
           col = j,
           pch = i,
           cex = 0.8)   
    
    #Plot regression lines only at N limitation
    if (i == 2){
      z <- newx %>% 
       filter(Lim  == limtype[i]) %>% 
       filter(Temp == Temps[j])
      points(z$Growth, z$Chl2C, type='l', col=j, lty=j)     
    }
  }
}

legend('topright',
       c('Exponential','P-limited','N-limited'),
       pch=1:3)
legend('topleft', paste(Temps,' ºC',sep=''),
       lty=1:length(Temps),
       col=1:length(Temps))
par(op)
dev.off()