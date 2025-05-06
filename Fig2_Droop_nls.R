#Use nonlinear regression
library(minpack.lm)
library(keras3)

Temps  <- levels(Semi$Temp)

#Summarize the result:
result <- data.frame(Temp = Temps,
                     QN0  = NA, SDQN0 = NA,
                     QN0_raw= NA, SDQN0_raw = NA,
                     QP0  = NA, SDQP0 = NA,
                     QP0_raw= NA, SDQP0_raw = NA,
                     muN  = NA, SDmuN = NA,
                     muP  = NA, SDmuP = NA,
                     R2N  = NA, R2P   = NA)

# Dataset of N limitation
Nlim <- Semi %>% 
  filter(Lim != 'P') %>% 
  filter(flag == F) %>% 
  mutate(N2C = 1/C2N)


for (i in 1:length(Temps)){
 df <- Nlim %>% 
   drop_na(Growth, N2C) %>% 
   filter(Temp == Temps[i]) 
 
 NLS_mod <- nlsLM(Growth ~ r0*(1-Q0/N2C), 
                  data = df, 
                  start = list(r0 = max(df$Growth, na.rm = T), 
                               Q0 = min(df$N2C,    na.rm = T)))
 
 #Calculate model predictions
 df %<>%
   mutate(pred= NLS_mod %>% predict(df))
 
 #Calculate mean of observations
 Mean_obs_growth <- mean(df$Growth)
 
 #Calculate the sum of sqaured differences between growth and mean
 SSq <- sum((df$Growth - Mean_obs_growth)**2)
 
 #Calculate R2 = 1- sum((M - O)^2)/sum((O-avg(O))^2)
 Rsq <- 1 - sum((df$Growth - df$pred)**2)/SSq
 
 result[i, 'QN0']   <- coef(NLS_mod)[2][[1]]
 result[i, 'muN']   <- coef(NLS_mod)[1][[1]]
 result[i, 'SDmuN'] <- coef(summary(NLS_mod))[1,2] 
 result[i, 'SDQN0'] <- coef(summary(NLS_mod))[2,2] 
 result[i, 'R2N']   <- Rsq
 
 #Add Q0 of raw data
 df <- Semi %>% 
   filter(Temp == Temps[i]) 
 
 #Find the three data points with largest C:N
 z <- sort(1/df$C2N, decreasing = F)[1:3]
 result[i, 'QN0_raw']   = mean(z)
 result[i, 'SDQN0_raw'] = sd(z)
   
}

# Dataset of P limitation
Plim <- Semi %>% 
  filter(flag == F) %>% 
  filter(Lim != 'N') %>% 
  mutate(P2C = 1/C2P)

for (i in 1:length(Temps)){
 df <- Plim %>% 
   drop_na(Growth, P2C) %>% 
   filter(Temp == Temps[i])
 
 NLS_mod <- nlsLM(Growth ~ r0*(1-Q0/P2C), 
                  data = df, 
                  start = list(r0 = max(df$Growth, na.rm = T), 
                               Q0 = min(df$P2C,    na.rm = T)))
 

 result[i, 'QP0']   <- coef(NLS_mod)[2][[1]]
 result[i, 'muP']   <- coef(NLS_mod)[1][[1]]
 result[i, 'SDmuP'] <- coef(summary(NLS_mod))[1,2] 
 result[i, 'SDQP0'] <- coef(summary(NLS_mod))[2,2] 
 
 #Calculate model predictions
 df %<>%
   mutate(pred = NLS_mod %>% predict(df))
 
 #Calculate mean of observations
 Mean_obs_growth <- mean(df$Growth, na.rm = T)
 
 #Calculate the sum of sqaured differences between growth and mean
 SSq <- sum((df$Growth - Mean_obs_growth)**2)
 Rsq <- 1 - sum((df$Growth - df$pred)**2)/SSq
 result[i, 'R2P']   <- Rsq
 
 #Find the three data points with largest C:P
 z <- sort(1/df$C2P, decreasing = F)[1:3]
 result[i, 'QP0_raw']   = mean(z)
 result[i, 'SDQP0_raw'] = sd(z)
 
}

#Compute maximal growth rate from exponential phase
Exp_dat <- 
  Semi %>% 
  filter(Lim == 'E') %>% 
  drop_na(Growth) %>% 
  group_by(Temp) %>% 
  summarise(mu_exp = mean(Growth), 
            sd_mu_exp = sd(Growth))

result %<>%
  left_join(Exp_dat, by = join_by(Temp))


# Plotting
fname <- 'Fig2_Droop_NLS.pdf'
pdf(fname,width=6,height=15)
op <-par(font.lab=1,
        family ="serif",
        mar    = c(3.5,3.5,.1,.1),
        mgp    = c(2.2,1,0),
        mfrow  = c(5,2),
        oma    = c(3,3,2,2))
Nlim = c(0,0.4)
Plim = c(0,1.*max(1/Semi$C2P,  na.rm=T))
ylim = c(0,1.*max(Semi$Growth, na.rm = T))
count= 0
for (i in 1:length(Temps)){
  for (j in c('N','P')){
    count = count + 1
    var = paste0('PO',j)
    Semi$Q = Semi[,var]/Semi[,'POC'] 
    
    dat <- Semi %>% 
      filter(Temp==Temps[i])
    
    LIM <- dat %>% 
      filter(Lim == j) %>% 
      filter(flag== F)
    
    Exp <- dat %>% 
      filter(Lim == 'E') %>% 
      filter(flag== F)
    
    NLIM <- dat %>% 
      filter((Lim != 'E')&(Lim!=j)) 
    
    Outliers <- dat %>% 
      filter(flag==T) 
    
    if (j == 'N'){
      xlim = Nlim
    } else{
      xlim = Plim
    }

    if (i == length(Temps)){
      XLAB <- paste(j,':C (mol:mol)',sep='')
    }else{
      XLAB <- ''
    }
    plot(LIM$Q, LIM$Growth,pch=1,cex=0.7,type='n',
         cex.axis = 1.2,
         cex.lab  = 1.2,
         xlim = xlim,
         ylim = ylim,
         xlab = XLAB,
         ylab = expression(paste('Growth rate ('*d^-1*')', sep='')))
    points(Outliers$Q,Outliers$Growth,cex=0.7)
    points(LIM$Q,LIM$Growth,pch=16,col='blue',cex=0.7)
    points(Exp$Q,Exp$Growth,pch=16,col='green',cex=0.7)
    points(NLIM$Q,NLIM$Growth,pch=16,col=2,cex=0.7)
    
    #Add R2 onto the plot
    lab <- paste0('R2', j)
    rsq <- round(result[i, lab], 2)
    R2label <- bquote(R^2 ==  .(rsq))
    text(0, 1.6, R2label, cex = 1.2, pos = 4)  
    
    
    if (j == 'N'){
      newx <- seq(0.01,xlim[2],.01)
      mu0  <- result[i, 'muN']
      Q0   <- result[i, 'QN0']
    } else{
      newx <- seq(0.01/16,xlim[2],.01/16)
      mu0  <- result[i, 'muP']
      Q0   <- result[i, 'QP0']
    }
    newy <- mu0 * (1 - Q0/newx)
    lines(newx, newy,lty=2,lwd=1.2)
    mtext(text=paste(LETTERS[count],') ',
                      Temps[i],' ºC, ',j,' limitation',
                      sep=''), 
          adj = 0)
    
    #Add Droop equation to the plot
    mu0 %<>% round(2)
    
    if (j == 'N'){
      Q0  %<>% round(3)
    }else{
      Q0  %<>% round(4)
    }
    droop_eqn <- bquote(mu ==  .(mu0) *(1 -  frac(.(Q0), Q))) 
      
    text(xlim[2]*.4, mu0*.55, droop_eqn, cex = 1.1, pos = 4)  
    
    
    #Add legend
    if (count == 1){
       legend('bottomright',
       c('Exponential','Limiting','Nonlimiting', 'Outliers'),
       col=c('green',  'blue',    'red', 1),
       pch=c(rep(16,3),1))
    } 
    
  }
}
par(op)
dev.off()
