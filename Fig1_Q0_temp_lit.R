#To plot literature reports on relationships between Q0 and temp
library(tidyverse)
fname <- 'Q0_temp_summary.csv'
df <- read_csv(fname) %>% 
  mutate(Species = as.factor(Species))  %>% 
  mutate(Nutrient= as.factor(Nutrient)) %>% 
  mutate(ID = as.factor(ID)) 

N_ID <- length(levels(df$ID))

pdf('Fig1_lit_summary.pdf',width=6,height=9)
op     <- par(font.lab = 1,
              family ="serif",
              mar    = c(2.8,3.5,.5,3.5),
              mgp    = c(2.2,1,0),
              mfrow  = c(5,2),
              oma    = c(2,2,1,1))

for (i in 1:N_ID){
  z <- df %>% 
    filter(ID == i)
  

  if (n_distinct(z$Nutrient) > 1){
    for (j in 1:n_distinct(z$Nutrient)){
      z1 <- z %>% 
          filter(Nutrient == unique(z$Nutrient)[j])
      YLAB <- z1$unit[1]
      if (i >= 9) YLAB <- expression(paste(mu*'g P '*mm^-3))
      if (j == 1){
        plot(z1$Temp, z1$Q0,
           xlim = c(0, 30),
           xlab = '',
           ylab = YLAB,
           type = 'b')
      }else{
        par(new=T)
        plot(z1$Temp, z1$Q0,
           xlim = c(0, 30),
           xlab = '',
           ylab = '',
           axes = F,
           lty  = j,
           type = 'b')
        mtext(z1$unit[1], side=4, line=2.5, cex = .7)
        Ymax <- max(z1$Q0)
        Ymin <- min(z1$Q0)
        axis(side = 4, at = round(c(Ymin, Ymax), 3), 
             labels = T, las=1)
      }
    }
    legend('topright', legend=unique(z$Nutrient), lty=1:n_distinct(z$Nutrient))
  }else{
    plot(z$Temp, z$Q0,
         xlim = c(0, 30),
         xlab = '',
         ylab = YLAB,
         type = 'b')
  }
  mtext(paste0(LETTERS[i],') ', z$Species[1]), adj=0)
}
mtext(side=1, 'Temperature (ºC)', adj=0.5, outer=T)
dev.off()