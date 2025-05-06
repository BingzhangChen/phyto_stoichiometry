#Plot N:P with growth rate and temperature within one plot:
#Run a multiple regression to assess the effects of temperature,
#growth rate and the type of limiting nutrient on N:P ratio
Semi1 <- Semi %>% 
  filter(flag == F)
options(contrasts = c("contr.sum", "contr.poly"))
NP_lm <- step(lm(N2P ~ Growth * Lim * Temp,
                 data = Semi1))
summary(NP_lm)
anova(NP_lm)


#Check residuals
par(mfrow=c(2,2))
plot(NP_lm, ask = F)
car::vif(NP_lm)

#Check boxcox transformation
par(mfrow=c(1,1))
MASS::boxcox(NP_lm)

NP_lm <- step(lm(sqrt(N2P) ~ Growth * Lim * Temp,
                 data = Semi1))

summary(NP_lm) #Temperature insignificant!!
anova(NP_lm)

Temps  <- unique(Semi$Temp)
limtype<- unique(Semi$Lim)

#Add regression lines
xlim <- c(0, max(Semi$Growth, na.rm=T))
newr <- seq(xlim[1], xlim[2],0.01)
newx <- expand.grid(Growth = newr,
                    Lim    = limtype)


newx$N2P <- predict(NP_lm, newx)**2

pdf('Fig6_NPratio.pdf',width=5,height=8)
op <- par(font.lab=1,
          family ="serif",
          mar    = c(3.5,3,1.5,.5),
          mgp    = c(2.2,1,0),
          mfrow  = c(2,1))


plot(Semi$Growth,Semi$N2P,
     xlab = expression(paste('Growth rate ('*d^-1*')')),
     ylab = 'N:P',
     type = 'n')

for (i in 1:length(limtype)){
  for (j in 1:length(Temps)){
    tmp <- Semi %>% 
      filter(Lim  == limtype[i]) %>% 
      filter(Temp == Temps[j])
    points(tmp$Growth, tmp$N2P,
           col = i,
           pch = j,
           cex = 0.8)   
  }
  #Plot regression lines
  z <- newx %>% 
   filter(Lim  == limtype[i]) 
  points(z$Growth, z$N2P, type='l', col=i, lty=i)     
}

legend('topleft',
       c('Exponential','P-limited','N-limited'),
       lty=1:3,
       col=1:3)
legend('topright', paste(Temps,' ºC',sep=''),
       pch=1:length(Temps))
mtext('A', adj=0)

ExG <- Semi %>% 
  filter(Lim == 'E')

plot(ExG$Temp, ExG$N2P, 
     xlab = 'Temperature (ºC)',
     ylab = 'N:P')

mtext('B', adj=0)

par(op)
dev.off()