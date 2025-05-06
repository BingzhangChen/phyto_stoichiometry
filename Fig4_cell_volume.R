#Fig. 4 Check what affects cell size (volume)
options(contrasts = c("contr.sum", "contr.poly"))
Vol_lm2 <- lm(log(CellVol) ~ Growth + Temp + Lim + Growth:Temp + Growth:Lim,
                   data = Semi[!is.na(Semi$Growth),])
summary(Vol_lm2)
anova(Vol_lm2)
alias(Vol_lm2)
par(mfrow=c(2,2))
plot(Vol_lm2, ask = F)

Temps  <- unique(Semi$Temp)
pdf('Fig4_Vol.pdf', height = 8, width = 5)
op   <- par(font.lab=1,
            family  ="serif",
            mar     = c(3.5,3.5,.1,.1),
            mgp     = c(2.2,1.2,0),
            mfrow   = c(2,1),
            oma     = c(1,1,1,1))

#Fig. 3A Relationships between cell volume against growth rate and temperature
plot(Semi$Growth, Semi$CellVol,
     log = 'y',
     cex.lab=1.2,
     cex.axis=1.2,
     xlab=expression(paste('Growth rate (',d^-1*')')),
     ylab=expression(paste('Cell volume (',mu*m^3,')')),
     type='n')

limtype <- unique(Semi$Lim)

for (i in 1:length(limtype)){
  for (j in 1:length(Temps)){
    tmp <- Semi %>% 
      filter(Lim == limtype[i]) %>% 
      filter(Temp== Temps[j])
    points(tmp$Growth, tmp$CellVol,
           col = j,
           pch = i,
           cex = 0.8)   
  }
}
legend('topleft',c('Exponential','P-limited','N-limited'),
       pch = 1:3)
legend('bottomright', paste(Temps,' ºC',sep=''),
       lty=1,
       col=1:length(Temps))

#Add regression lines
xmin <- min(Semi$Growth,na.rm=T)
xmax <- max(Semi$Growth,na.rm=T)
newr <- seq(xmin,xmax,0.01)
newx <- expand.grid(Growth = newr,
                    Temp   = Temps,
                    Lim    = c('N', 'P'))
newx$Vol <- exp(predict(Vol_lm2, newx))

for (i in 1:length(unique(newx$Lim))){
  for (j in 1:length(Temps)){
    z <- newx %>% 
      filter(Temp == Temps[j]) %>% 
      filter(Lim  == unique(newx$Lim)[i])
    points(z$Growth, z$Vol, type='l', col=j, lty=i)
  }
}
mtext('A', adj=0)

#Fig. 3B Relationships between cell volume against temperature in exponentially growing cultures
Exp <- Semi %>% 
  filter(Lim == 'E') %>% 
  mutate(Temp = as.numeric(as.character(Temp)))


#Run a piecewise regression of logVol ~ Temp
library(segmented)
base_lm <- lm(log(CellVol) ~ Temp, Exp)

#Add breakpoint with segmented()
# Fit piecewise linear regression
seg_mod <- segmented(base_lm, 
                     seg.Z = ~Temp, 
                     psi = list(Temp = 25))  # 25 is initial guess

beta <- round(coef(summary(seg_mod))[2,1], 3)
newx <- data.frame(Temp = seq(10, 30, length.out=100))
newy <- predict(seg_mod, newdata = newx, se.fit = T)
lwr  <- exp(newy$fit - 2*newy$se.fit)
upr  <- exp(newy$fit + 2*newy$se.fit)
Ymin <- min(lwr)
Ymax <- max(upr)

plot(Exp$Temp, Exp$CellVol,
     ylim= c(Ymin, Ymax),
     pch = 16,
     cex = 0.8,
     log = 'y',
     cex.lab = 1.2,
     cex.axis= 1.2,
     xlab= 'Temperature (ºC)',
     ylab=expression(paste('Cell volume (', mu*m^3,')')))

lines(newx$Temp, exp(newy$fit))


lines(newx$Temp, lwr, lty = 2)
lines(newx$Temp, upr, lty = 2)

text(15, 745, labels=paste('Slope =', beta), pos = 4)
mtext('B', adj=0)
par(op)
dev.off()
