#Plot umax and Q0 with temperature:
pdf('Fig2_Q_mu_Temp.pdf')
op     <- par(font.lab=1,
              family ="serif",
              mar    = c(3.5,3.3,.2,.1),
              mgp    = c(2.2,1,0),
              mfrow  = c(2,2),
              oma    = c(4,4,3,3))
x <- as.numeric(as.character(result$Temp))
count = 0
for (i in c('N','P')){
   count  = count + 1
   Q_min  = paste('Q',  i,'0', sep = '')
  SD_min  = paste('SDQ',i,'0', sep = '') 
  mu_max  = paste('mu',i,      sep = '')
  SD_mu   = paste('SDmu',i,    sep = '')
  Q_min   = result[,Q_min ]
  SD_min  = result[,SD_min]
  mu_max  = result[,mu_max]
  SD_mu   = result[,SD_mu ]
  
  yLIM <- if (i == 'N'){
            c(0, max(result$QN0+2*result$SDQN0))
         } else{
            c(0, max(result$QP0+2*result$SDQP0))
         }
  
  #Plot Q0
  plot(x, Q_min,
       ylim = yLIM, 
       pch  = 16,
       xlab = expression(paste("Temperature (ºC)")),
       ylab = paste('Minimal ',i,':C',sep = '')  )
  
  #Add error bars:
  lwr = Q_min - 2*SD_min
  upr = Q_min + 2*SD_min
  segments(x,lwr,x,upr)
  text(10,yLIM[2],LETTERS[count])
  
  #Plot umax
  yLIM <- c(0, 3.5)
  plot(x,  mu_max,
       ylim = yLIM, 
       pch  = 16,
       xlab = expression(paste("Temperature (",degree,"C)")),
       ylab = expression(paste('Maximal growth rate ( '*d^-1*' )',sep = ''))  )
  #Add error bars:
  lwr = mu_max - 2*SD_mu
  upr = mu_max + 2*SD_mu
  segments(x,lwr,x,upr)
  count = count + 1
  text(10,yLIM[2],LETTERS[count])
}

par(op)
dev.off()
