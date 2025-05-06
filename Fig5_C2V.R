##########################Fig. 4############
#Impose sum-to-zero constraint
options(contrasts = c("contr.sum", "contr.poly"))
CVol_lm <- lm(log(POC) ~ log(CellVol) + Temp + Lim +
                         log(CellVol):Temp,
                         data=Semi)
summary(CVol_lm)
anova(CVol_lm)

par(mfrow=c(2,2))
plot(CVol_lm, ask = F)


Temps  <- unique(Semi$Temp)
pdf('Fig5_Vol_PC.pdf', height = 5, width = 6)
op   <- par(font.lab=1,
            family  ="serif",
            mar     = c(3.5,3.5,.1,.1),
            mgp     = c(2.2,1.2,0),
            mfrow   = c(1,1),
            oma     = c(2,2,1,1))

plot(Semi$CellVol, Semi$POC,
     log = 'xy',
     cex.lab=1.2,
     cex.axis=1.2,
     xlab=expression(paste('Cell volume (',mu*m^3,')')),
     ylab=expression(paste('Cell carbon (',pgC*' '*cell^-1*')')),
     type='n')

limtype <- unique(Semi$Lim)

for (i in 1:length(limtype)){
  for (j in 1:length(Temps)){
    tmp <- Semi %>% 
      filter(Lim == limtype[i]) %>% 
      filter(Temp== Temps[j])
    points(tmp$CellVol, tmp$POC,
           col = j,
           pch = i,
           cex = 0.8)   
  }
}
legend('topleft',c('Exponential','P-limited','N-limited'),
       pch=1:3)
legend('bottomright', paste(Temps,' ºC',sep=''), lty=1,
       col=1:length(Temps))

#Plot regression lines for different temperatures under N limitation
xmin <- min(Semi$CellVol,na.rm=T)
xmax <- max(Semi$CellVol,na.rm=T)
newV <- seq(xmin,xmax,0.1)

ct <- 1
for (j in c('N', 'P')){
  newx <- expand.grid(CellVol=newV,
                      Temp   =Temps,
                      Lim    =j)
  newx$PC <- exp(predict(CVol_lm,newx))
  
  for (i in 1:length(Temps)){
    z <- newx %>% 
      filter(Temp == Temps[i])
    points(z$CellVol, z$PC, type='l', col=i, lty=ct)
  } 
  ct <- ct + 1
}

par(op)
dev.off()
