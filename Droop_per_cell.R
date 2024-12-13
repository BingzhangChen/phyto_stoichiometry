# Dataset of N limitation
Nlim <- Semi %>% 
  mutate(inv_PON = 1/PON) %>% 
  filter(flag == F) %>% 
  filter(Lim != 'P')

lm_Nlim  <- lm(Growth ~ inv_PON * Temp, Nlim)

# Dataset of P limitation
Plim <- Semi %>% 
  mutate(inv_POP = 1/POP) %>% 
  filter(flag == F) %>% 
  filter(Lim != 'N')

lm_Plim <- lm(Growth ~ inv_POP * Temp, Plim)

#Summarize the result:
Temps  <- unique(Semi$Temp)
result_cell <- data.frame(Temp = Temps,
                          QN0  = NA, SDQN0 = NA,
                          QP0  = NA, SDQP0 = NA,
                          muN  = NA, SDmuN = NA,
                          muP  = NA, SDmuP = NA )
N_coef <- coef(summary(lm_Nlim))
N_VCOV <- vcov(lm_Nlim)

P_coef <- coef(summary(lm_Plim))
P_VCOV <- vcov(lm_Plim)

sd2 <- function(dsd, sd1) {
  dvar <- dsd**2
  var1 <- sd1**2
  var2 <- dvar - var1
  return(sqrt(var2))
}
result_cell[1, 'muN']   <- N_coef[1,1]
result_cell[1, 'muP']   <- P_coef[1,1]
result_cell[1, 'SDmuN'] <- N_coef[1,2]
result_cell[1, 'SDmuP'] <- P_coef[1,2]
result_cell[1, 'QN0']   <- -N_coef[2,1]/N_coef[1,1]
result_cell[1, 'QP0']   <- -P_coef[2,1]/P_coef[1,1]


#1000 replicates
N <- 1000L

library(MASS)
set.seed(25112024)

#Estimate SE of QN0
#Use Monte-Carlo to estimate SE of Q0
VCOV <- N_VCOV[1:2,1:2]
MEAN <- N_coef[1:2,1]
z <- mvrnorm(N, mu=MEAN, Sigma = VCOV) %>% 
  as.data.frame(.) %>% 
  filter(inv_PON < 0)
QN0 <- -z[,2]/z[,1]
result_cell[1, 'SDQN0'] <- sd(QN0) 

#Estimate SE of QP0
VCOV <- P_VCOV[1:2,1:2]
MEAN <- P_coef[1:2,1]
z <- mvrnorm(N, mu=MEAN, Sigma = VCOV) %>% 
  as.data.frame(.) %>% 
  filter(inv_POP < 0)
QP0 <- -z[,2]/z[,1]
result_cell[1, 'SDQP0'] <- sd(QP0) 

for (i in 2:nrow(result_cell)){
  result_cell[i, 'muN'] <- N_coef[i+1,1] + N_coef[1,1]
  result_cell[i, 'muP'] <- P_coef[i+1,1] + P_coef[1,1]
  
  #Estimate SE of mumax
  dSE <- N_coef[i+1, 2]
  result_cell[i, 'SDmuN'] <- sd2(dSE, N_coef[1,2]) 
  
  dSE <- P_coef[i+1, 2]
  result_cell[i, 'SDmuP'] <- sd2(dSE, P_coef[1,2]) 
  result_cell[i, 'QN0'] <- -(N_coef[i+5,1] + N_coef[2,1])/result_cell[i, 'muN'] 
  result_cell[i, 'QP0'] <- -(P_coef[i+5,1] + P_coef[2,1])/result_cell[i, 'muP']  
  
  #Estimate SE of QN0
  l    <- c(1,2,i+1,i+5)
  VCOV <- N_VCOV[l,l]
  MEAN <- N_coef[l,1]
  z <- mvrnorm(N, mu=MEAN, Sigma = VCOV) %>% 
    as.data.frame
  
  new_int   <- z[,1] + z[,3]
  new_slope <- z[,2] + z[,4]
  QN0       <- -new_slope/new_int
  result_cell[i, 'SDQN0'] <- sd(QN0[QN0 > 0]) 
  
  #Estimate SE of QP0
  VCOV <- P_VCOV[l,l]
  MEAN <- P_coef[l,1]
  z <- mvrnorm(N, mu=MEAN, Sigma = VCOV) %>% 
    as.data.frame
  
  new_int   <- z[,1] + z[,3]
  new_slope <- z[,2] + z[,4]
  QP0       <- -new_slope/new_int
  result_cell[i, 'SDQP0'] <- sd(QP0[QP0>0]) 
}

# Plotting
fname <- 'Fig1_Droop.pdf'
pdf(fname,width=6,height=15)
op <-par(font.lab=1,
        family ="serif",
        mar    = c(3.5,3.5,.1,.1),
        mgp    = c(2.2,1,0),
        mfrow  = c(5,2),
        oma    = c(3,3,2,2))
Nlim = c(0,0.4)
Plim = c(0,1.*max(1/Semi$C2P,na.rm=T))
ylim = c(0,1.*max(Semi$Growth, na.rm = T))
count= 0
for (i in 1:length(Temps)){
  for (j in c('N','P')){
    count = count + 1
    var = paste('PO',j,sep='')
    Semi$Q = Semi[,var]/Semi[,'POC'] 
    LIM = subset(Semi,(Lim == j) & (Temp == Temps[i]))
    Exp = subset(Semi,(Lim == 'E') & (Temp == Temps[i]))
    NLIM= subset(Semi,((Lim != 'E')&(Lim!=j)) & (Temp == Temps[i]))
    if (j == 'N'){
      xlim = Nlim
    } else{
      xlim = Plim
    }

    plot(LIM$Q, LIM$Growth,pch=1,cex=0.7,type='n',
         cex.axis = 1.2,
         cex.lab  = 1.2,
         xlim = xlim,
         ylim = ylim,
         xlab = paste(j,':C (mol:mol)',sep=''),
         ylab = expression(paste('Growth rate ('*d^-1*')', sep='')))
    points(LIM$Q,LIM$Growth,pch=16,col='blue',cex=0.7)
    points(Exp$Q,Exp$Growth,pch=16,col='green',cex=0.7)
    points(NLIM$Q,NLIM$Growth,pch=16,col=2,cex=0.7)
    
    if (j == 'N'){
      newx <- data.frame(Temp=Temps[i], C2N=1./(seq(0.01,xlim[2],.01)))
      newy <- predict(lm_Nlim, newx)
      lines(1/newx$C2N, newy,lty=2,lwd=1.2)
    } else{
      newx <- data.frame(Temp=Temps[i], C2P=1./(seq(0.01/16,xlim[2],.01/16)))
      newy <- predict(lm_Plim, newx)
      lines(1/newx$C2P, newy,lty=2,lwd=1.2)
    }
    text(0,y=ylim[2]*0.97,
         labels=paste(LETTERS[count],') ',
                      Temps[i],' ºC, ',j,' limitation',
                      sep=''), 
         pos = 4)
   #Add legend
   if (count == 1){
      legend('bottomright',
      c('Exponential','Limiting','Nonlimiting'),
      col=c('green',  'blue',    'red'),
      pch=16,cex=1.0)
   } 
  }
}
par(op)
dev.off()
