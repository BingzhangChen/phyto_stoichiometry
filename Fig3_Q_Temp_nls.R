#Plot umax and Q0 with temperature:
pdf('Fig3_Q_mu_Temp_nls.pdf')
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
  Q_min_raw = paste0('Q',  i,'0_raw')
  SD_min_raw= paste0('SDQ',i,'0_raw') 
  mu_max  = paste('mu',i,      sep = '')
  SD_mu   = paste('SDmu',i,    sep = '')
  Q_min   = result[,Q_min ]
  SD_min  = result[,SD_min]
  mu_max  = result[,mu_max]
  SD_mu   = result[,SD_mu ]
  Q_min_raw = result[, Q_min_raw]
  SD_min_raw= result[,SD_min_raw]
  
  yLIM <- if (i == 'N'){
            c(0, max(result$QN0+3*result$SDQN0))
         } else{
            c(0, max(result$QP0+3*result$SDQP0))
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
  
  #Add raw data of Q0N and Q0P
  points(x, Q_min_raw, pch = 1, col = 'green')
  lwr = Q_min_raw - 2*SD_min_raw
  upr = Q_min_raw + 2*SD_min_raw
  segments(x,lwr,x,upr, col = 'green') 
  
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
  
  #Add umax from exponential growth phase
  points(x, result$mu_exp, pch = 1, col = 'green')
  lwr = result$mu_exp - 2*result$sd_mu_exp
  upr = result$mu_exp + 2*result$sd_mu_exp
  segments(x,lwr,x,upr, col = 'green') 
}

#Add legend
legend('topright',
       c('Model estimates','Raw data'),
       col=c(1, 'green'),
       pch=c(16,1))

par(op)
dev.off()
