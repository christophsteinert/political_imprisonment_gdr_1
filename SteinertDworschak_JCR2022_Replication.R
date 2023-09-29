
### Christoph Steinert and Christoph Dworschak (read as: the two Christophs)
### Analysis file, JCR 2022: Political imprisonments and protest mobilization

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
dir()
rm(list=ls())
set.seed(10213)

df <- read.csv("SteinertDworschak_JCR2022_Replication.csv")
head(df)


#### Analysis preparation ####
model.variables <- c("parrests.sum", "protests53",
                     "theater_guests.1984", "pop_county.1984.log", "city_county", "cities")
df2 <- df[ !apply(is.na(df[,colnames(df)%in%model.variables]),1,any), ]

# Main models: OVs as control
model.variables2 <- c("parrests.sum",
                      "ovs_clean.1987", "ovs_clean.1986", "ovs_clean.1985", "ovs_clean.1984",
                      "theater_guests.1984", "pop_county.1984.log", "city_county", "cities")
df3 <- df2[ !apply(is.na(df2[,colnames(df2)%in%model.variables2]),1,any), ]



#### Descriptive DV graph ####
# pdf("DV_hist.pdf", width = 8, height = 6)
hist(df3$time.of.first.protest, freq=TRUE, main="", xlab="Day of first protest outbreak", 
     xlim=c(0,79), ylim=c(0,15), breaks = 40, xaxt="n", col = FALSE)
axis(1, at=seq(1, 65, 4), labels = seq(1, 65, 4), cex.axis=0.7)
for(i in 1:1){
  text(x=df3$time.of.first.protest[order(df3$time.of.first.protest)][i]+2, y=1,
       labels=gsub(" \\(Stadt\\)$", "", df3$county[order(df3$time.of.first.protest)][i]), pos=4, cex=0.75, offset=0.05)
}
dev.off()



#### Survival Analysis ####
library(survival)

summary(cox_ovs4 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1988  + ovs_clean.1987 +
                            theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(cox_ovs3 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1987 + ovs_clean.1986 +
                            theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(cox_ovs2 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1986 + ovs_clean.1985 +
                            theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(cox_ovs1 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1985 + ovs_clean.1984 +
                            theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(cox_ovs_naive <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum,
                               data=df3) )
summary(cox_ovs_null <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ ovs_clean.1984 +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(cox_ovs <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum + ovs_clean.1984 +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )

library(stargazer)
stargazer(cox_ovs_naive, cox_ovs, cox_ovs_null, cox_ovs1, cox_ovs2, cox_ovs3, cox_ovs4,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "parrests.1985" = "Pol.\\ arrests, 1985",
            "ovs_clean.1984" = "OVs in 1984",
            "parrests.1986" = "Pol.\\ arrests, 1986",
            "ovs_clean.1985" = "OVs in 1985",
            "parrests.1987" = "Pol.\\ arrests, 1987",
            "ovs_clean.1986" = "OVs in 1986",
            "parrests.1988" = "Pol.\\ arrests, 1988",
            "ovs_clean.1987" = "OVs in 1987",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)


# Survival plot
newdata1 <- data.frame(
  parrests.sum = round(quantile(na.omit(df3$parrests.sum), c(0.1, 0.5, 0.9)),0),
  ovs_clean.1984 = mean(df3$ovs_clean.1984, na.rm=T),
  theater_guests.1984 = mean(df3$theater_guests.1984, na.rm=T),
  pop_county.1984.log = mean(df3$pop_county.1984.log, na.rm=T),
  city_county = median(df3$city_county, na.rm=T),
  cities = median(df3$cities, na.rm=T)
) # 1989-09-04 1989-11-08
# pdf("survivalplot_ovs.pdf", width = 7, height = 6)
plot(survfit(cox_ovs, newdata=newdata1 ), col=c("gray70", "gray50", "black"), lty=c(3:1), lwd=2, conf.int = F, fun="event",
     xlab = "\nDays from the first protest \nto the fall of the Berlin wall", ylab = "Cumumlative probability of mobilizing",
     main="", # F(t): Protest initiation across the former GDR, 1989
     las=1, xaxt="n", bty="n")
axis(side=1, at=seq(1,65,1), labels = FALSE, tck=-0.02); axis(side=1, at=c(0,33,66), labels = FALSE, tck=-0.04)
text(x=c(0, 66), y=rep(-0.07, 2), cex=0.9,  par("usr")[3],
     labels = c("\n04/09/1989:\nProtests start\nin Leipzig", "\n09/11/1989:\n Fall of \nBerlin wall"), srt = 0, pos = 1, xpd = TRUE)
legend(0, 1, c("Counties in 1985-1988 with: \n\nlow number of arrests (9) \n\n",
               "median number of arrests (24)",
               "high number of arrests (60)"),
       lty = c(3:1), col=c("gray70", "gray40", "black"), y.intersp=-2.5, bty = "n")
# dev.off()


# pdf("survivalplot_ovs_CIs.pdf", width = 7, height = 6)
plot(survfit(cox_ovs, newdata=newdata1[-2,] ), col=c("gray70", "black"), lty=c(3,1), lwd=2, conf.int = F, fun="event",
     xlab = "\nDays from the first protest \nto the fall of the Berlin wall", ylab = "Cumumlative probability of mobilizing",
     main="", # F(t): Protest initiation across the former GDR, 1989
     las=1, xaxt="n", bty="n")
axis(side=1, at=seq(1,65,1), labels = FALSE, tck=-0.02); axis(side=1, at=c(0,33,66), labels = FALSE, tck=-0.04)
text(x=c(0, 66), y=rep(-0.07, 2), cex=0.9,  par("usr")[3],
     labels = c("\n04/09/1989:\nProtests start\nin Leipzig", "\n09/11/1989:\n Fall of \nBerlin wall"), srt = 0, pos = 1, xpd = TRUE)
legend(0, 1, c("Counties in 1985-1988 with: \n\nlow number of arrests (9) \n\n",
               "high number of arrests (60)"),
       lty = c(3,1), col=c("gray70", "black"), y.intersp=-2.5, bty = "n")
model1 <- summary( survfit(cox_ovs, newdata=newdata1[-2,]) )
polygon(c(model1$time, rev(model1$time)), c(1-model1$lower[,1], 1-rev(model1$upper[,1])), col=rgb(0.8,0.8,0.8, alpha = 0.5), border = FALSE)
polygon(c(model1$time, rev(model1$time)), c(1-model1$lower[,2], 1-rev(model1$upper[,2])), col=rgb(0.6,0.6,0.6, alpha = 0.5), border = FALSE)
# dev.off()
model1$time[model1$lower[,1] > model1$upper[,2]]


#### Prediction ####

relerror <- function(observed.y, predicted.y) {
  re <- sum((observed.y - predicted.y)^2)/sum((observed.y - mean(observed.y))^2)
  return(re)
  } # relative error SSR/TSS

X <- cbind(df3$parrests.sum, df3$ovs_clean.1984, df3$theater_guests.1984,
           df3$pop_county.1984.log, df3$city_county, df3$cities)
colnames(X) <- c("Arrests", "OVs", "Theatre", "Pop", "Urban", "Cities")

# Checking deviance b/w poisson fit and linear regression
summary(bla1 <- glm(time.of.first.protest ~ parrests.sum + ovs_clean.1984 + theater_guests.1984 + pop_county.1984.log +
          city_county + cities, data=df3, family = "poisson") )
exp(predict(bla1))
predict(bla1, type = "response")
summary(bla2 <- lm(time.of.first.protest ~ parrests.sum + ovs_clean.1984 + theater_guests.1984 + pop_county.1984.log +
                      city_county + cities, data=df3) )
predict(bla2)
cbind(1, X) %*% summary(bla2)$coef[,1]
summary(bla2 <- lm(log(time.of.first.protest) ~ parrests.sum + ovs_clean.1984 + theater_guests.1984 + pop_county.1984.log +
                     city_county + cities, data=df3) )
exp(predict(bla2))

library(randomForest); set.seed(56768)
rf.model <- randomForest(y = as.numeric(df3$time.of.first.protest), x = X,
                         ntree = 500, localImp = TRUE, nodesize = 5, do.trace = TRUE, keep.forest = TRUE, keep.inbag=TRUE)

rf.model

# Extract error measures for oob test data
rf.model$mse
rf.model$forest
rf.model$rsq

# Extract predicted values for validation set (oob test data)
pred.rf <- rf.model$predicted
rf.model$oob.times

# calculated oob mse:
mse.rf <- mean((pred.rf - df3$time.of.first.protest)^2)
mse.rf

# calculated oob mae:
mae.rf <- mean(abs(pred.rf - df3$time.of.first.protest))
mae.rf

# calculate oob R-squared:
r2.rf <- 1 - relerror(observed.y = df3$time.of.first.protest, predicted.y = pred.rf)
r2.rf


## Evaluate importance of predictors
rf.model$importance
rf.model$importanceSD

# pdf("rf_eval.pdf", width = 8, height = 8)
varImpPlot(rf.model, main = "", cex = 0.75, labels = "")
axis(2, at=seq(0.9,5.9,0.96),
     labels=c("# cities", "Theatre g.", "Urban", "OVs", "Pop. size", "Pol. arrests"),
     pos=-480, lwd=0, cex.axis=0.7)
axis(2, at=seq(0.9,5.9,0.96),
     labels=c("Urban", "Theatre g.", "# cities", "OVs", "Pop.", "Pol. arrests"),
     pos=1250, lwd=0, cex.axis=0.7)
dev.off()


rf.model.naive <- randomForest(y = as.numeric(df3$time.of.first.protest), x = as.data.frame(X[,"Arrests"]),
                               ntree = 500, localImp = TRUE, nodesize = 5)
rf.model.null <- randomForest(y = as.numeric(df3$time.of.first.protest), x = X[,-1],
                              ntree = 500, localImp = TRUE, nodesize = 5)

# PDP:
# pdf("rf_pdp.pdf", width = 8, height = 8)
par(mfrow = c(1,2))
partialPlot(rf.model, pred.data = X, x.var = "Arrests", rug = FALSE, # but: covairates are of course correlated
            xlab = "Pol. arrests (full model)", ylab = "Predicted time until mobilization", main = "")
partialPlot(rf.model.naive, pred.data = as.data.frame(X[,"Arrests"]), x.var = 'X[, "Arrests"]', rug = FALSE,
            xlab = "Pol. arrests (bivariate)", ylab = "Predicted time until mobilization", main = "")
dev.off(); par(mfrow = c(1,1))



# Calculate oob MAE when arrests not included (careful, different par dimension!)
mse.rf
mean((rf.model.naive$predicted - df3$time.of.first.protest)^2)
mean((rf.model.null$predicted - df3$time.of.first.protest)^2)

mae.rf
mean(abs(rf.model.naive$predicted - df3$time.of.first.protest))
mean(abs(rf.model.null$predicted - df3$time.of.first.protest))



#### Robustness: various checks ####

# Proportional hazards assumption
cox.zph(cox_ovs)
par(mfrow=c(2,3))
plot(cox.zph(cox_ovs))
dev.off()
par(mfrow=c(1,1))

# Excluding outliers and leverage points
table(df3$time.of.first.protest)
summary(coxph(Surv(
  rep(0, nrow(df3[df3$time.of.first.protest>1,])),
  time.of.first.protest,
  rep(1, nrow(df3[df3$time.of.first.protest>1,])) ) ~ parrests.sum + ovs_clean.1984 +
    theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3[df3$time.of.first.protest>1,]) )
table(df3$parrests.sum)
summary(coxph(Surv(
  rep(0, nrow(df3[df3$parrests.sum<300,])),
  time.of.first.protest,
  rep(1, nrow(df3[df3$parrests.sum<300,])) ) ~ parrests.sum + ovs_clean.1984 +
    theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3[df3$parrests.sum<300,]) )


#### Robustness: protest53 as control ####
summary(cox_p53_naive <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum,
                               data=df2) )
summary(cox_p53 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum + protests53 +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_p53_null <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ protests53 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_p53_88 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1988 + protests53 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_p53_87 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1987 + protests53 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_p53_86 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1986 + protests53 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_p53_85 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1985 + protests53 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )

library(stargazer)
stargazer(cox_p53_naive, cox_p53, cox_p53_null, cox_p53_85, cox_p53_86, cox_p53_87, cox_p53_88,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "parrests.1985" = "Pol.\\ arrests, 1985",
            "parrests.1986" = "Pol.\\ arrests, 1986",
            "parrests.1987" = "Pol.\\ arrests, 1987",
            "parrests.1988" = "Pol.\\ arrests, 1988",
            "protests53" = "Protests in 1953",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)

# Survival plot
newdata2 <- data.frame(
  parrests.sum = round(quantile(na.omit(df2$parrests.sum), c(0.1, 0.5, 0.9)),0),
  protests53 = mean(df2$protests53, na.rm=T),
  theater_guests.1984 = mean(df2$theater_guests.1984, na.rm=T),
  pop_county.1984.log = mean(df2$pop_county.1984.log, na.rm=T),
  city_county = median(df2$city_county, na.rm=T),
  cities = median(df2$cities, na.rm=T)
) # 1989-09-04 1989-11-08
# pdf("survivalplot_p53.pdf", width = 7, height = 6)
plot(survfit(cox_p53, newdata=newdata2 ), col=c("gray70", "gray50", "black"), lty=c(3:1), lwd=2, conf.int = F, fun="event",
     xlab = "\nDays from the first protest \nto the fall of the Berlin wall", ylab = "Cumumlative probability of mobilizing",
     main="", # F(t): Protest initiation across the former GDR, 1989
     las=1, xaxt="n", bty="n")
axis(side=1, at=seq(1,65,1), labels = FALSE, tck=-0.02); axis(side=1, at=c(0,33,66), labels = FALSE, tck=-0.04)
text(x=c(0, 66), y=rep(-0.07, 2), cex=0.9,  par("usr")[3],
     labels = c("\n04/09/1989:\nProtests start\nin Leipzig", "\n09/11/1989:\n Fall of \nBerlin wall"), srt = 0, pos = 1, xpd = TRUE)
legend(0, 1, c("Counties in 1985-1988 with: \n\nlow number of arrests (8) \n\n",
               "median number of arrests (23)",
               "high number of arrests (64)"),
       lty = c(3:1), col=c("gray70", "gray40", "black"), y.intersp=-2.5, bty = "n")
dev.off()


#### Robustness: OLS ####
summary(lm_naive <- lm(time.of.first.protest ~ parrests.sum, data=df3) )
summary(lm <- lm(time.of.first.protest ~ parrests.sum + ovs_clean.1984 +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(lm_null <- lm(time.of.first.protest ~ ovs_clean.1984 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(lm_88 <- lm(time.of.first.protest ~ parrests.1988 + ovs_clean.1987 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(lm_87 <- lm(time.of.first.protest ~ parrests.1987 + ovs_clean.1986 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(lm_86 <- lm(time.of.first.protest ~ parrests.1986 + ovs_clean.1985 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
summary(lm_85 <- lm(time.of.first.protest ~ parrests.1985 + ovs_clean.1984 +
                              theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )


library(stargazer)
stargazer(lm_naive, lm, lm_null, lm_85, lm_86, lm_87, lm_88,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "parrests.1985" = "Pol.\\ arrests, 1985",
            "ovs_clean.1984" = "OVs in 1984",
            "parrests.1986" = "Pol.\\ arrests, 1986",
            "ovs_clean.1985" = "OVs in 1985",
            "parrests.1987" = "Pol.\\ arrests, 1987",
            "ovs_clean.1986" = "OVs in 1986",
            "parrests.1988" = "Pol.\\ arrests, 1988",
            "ovs_clean.1987" = "OVs in 1987",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\ variable: time until mobilization in 1989")



#### Robustness: sub OVs with petition_sum & OPKs as controls ####
summary(cox_pet <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum + petition_sum.1984 +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_opk1 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum + opks_clean.1984 +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_opk2 <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum + I(opks_clean.1984 + ovs_clean.1984) +
                           theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )

library(stargazer)
stargazer(cox_opk1, cox_opk2, cox_pet,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "opks_clean.1984" = "OPKs in 1984",
            "I(opks_clean.1984 + ovs_clean.1984)" = "OPKs + OVs in 1984",
            "petition_sum.1984" = "Petitions in 1984",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)


#### Robustness: zersetz_and_prevent as alt. explanatory variable ####
model.variables3 <- c("zersetz_and_prevent.1988", "zersetz_and_prevent.1987", "zersetz_and_prevent.1986", "zersetz_and_prevent.1985")
df3_zers <- df3[ !apply(is.na(df3[,colnames(df3)%in%model.variables3]),1,any), ]
summary(cox_ovs_zers1 <- coxph(Surv( rep(0, nrow(df3_zers)), time.of.first.protest, rep(1, nrow(df3_zers)) ) ~ zersetz_and_prevent.sum + ovs_clean.1984 +
                                 theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3_zers) )
summary(cox_ovs_zers1_arrests <- coxph(Surv( rep(0, nrow(df3_zers)), time.of.first.protest, rep(1, nrow(df3_zers)) ) ~ parrests.sum + ovs_clean.1984 +
                                         theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3_zers) )
# Using protests53 instead of OVs to gain more observations:
df2_zers <- df2[ !apply(is.na(df2[,colnames(df2)%in%model.variables3]),1,any), ]
summary(cox_ovs_zers2 <- coxph(Surv( rep(0, nrow(df2_zers)), time.of.first.protest, rep(1, nrow(df2_zers)) ) ~ zersetz_and_prevent.sum + protests53 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2_zers) )
summary(cox_ovs_zers2_arrests <- coxph(Surv( rep(0, nrow(df2_zers)), time.of.first.protest, rep(1, nrow(df2_zers)) ) ~ parrests.sum + protests53 +
                                         theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2_zers) )


library(stargazer)
stargazer(cox_ovs_zers1_arrests, cox_ovs_zers1, cox_ovs_zers2_arrests, cox_ovs_zers2,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "zersetz_and_prevent.sum" = "Sum decomposition measures, 1985-1988",
            "ovs_clean.1984" = "OVs in 1984",
            "protests53" = "Protests in 1953",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)

library(cobalt); library(CBPS); library(ggplot2); library(gridExtra)
cobalt1 <- bal.plot(
  CBPS(parrests.sum ~ ovs_clean.1984, data = df3_zers, method = "exact"),
  which = "unadjusted", var.name = "ovs_clean.1984", colors = c("black", "gray"))
cobalt1 <- cobalt1 + labs(title = "Cov. balance of OVs across pol. imprisonments") + 
  xlab("OVs in 1984") + ylab("Sum pol. arrests, 1985-1988")
cobalt2 <- bal.plot(
  CBPS(zersetz_and_prevent.sum ~ ovs_clean.1984, data = df3_zers, method = "exact"),
  which = "unadjusted", var.name = "ovs_clean.1984")
cobalt2 <- cobalt2 + labs(title = "Cov. balance of OVs across decomp. measures") + 
  xlab("OVs in 1984") + ylab("Sum decomp. measures, 1985-1988")
cobalt3 <- bal.plot(
  CBPS(parrests.sum ~ protests53, data = df2_zers, method = "exact"),
  which = "unadjusted", var.name = "protests53")
cobalt3 <- cobalt3 + labs(title = "Cov. balance of protests in 1953 across pol. imprisonments") + 
  xlab("Protests in 1953") + ylab("Sum pol. arrests, 1985-1988")
cobalt4 <- bal.plot(
  CBPS(zersetz_and_prevent.sum ~ protests53, data = df2_zers, method = "exact"),
  which = "unadjusted", var.name = "protests53")
cobalt4 <- cobalt4 + labs(title = "Cov. balance of protests in 1953 across decomp. measures") + 
  xlab("Protests in 1953") + ylab("Sum decomp. measures, 1985-1988")
blank <- rectGrob(gp=gpar(col="white"))

# Survival plot
newdata3 <- data.frame(
  zersetz_and_prevent.sum = round(quantile(df2_zers$zersetz_and_prevent.sum, c(0.1, 0.5, 0.9)),0),
  protests53 = mean(df2_zers$protests53, na.rm=T),
  theater_guests.1984 = mean(df2_zers$theater_guests.1984, na.rm=T),
  pop_county.1984.log = mean(df2_zers$pop_county.1984.log, na.rm=T),
  city_county = median(df2_zers$city_county, na.rm=T),
  cities = median(df2_zers$cities, na.rm=T)
)
# pdf("survivalplot_decomposition.pdf", width = 7, height = 6)
plot(survfit(cox_ovs_zers2, newdata=newdata3 ), col=c("gray70", "gray50", "black"), lty=c(3:1), lwd=2, conf.int = F, fun="event",
     xlab = "\nDays from the first protest \nto the fall of the Berlin wall", ylab = "Cumumlative probability of mobilizing",
     main="", # F(t): Protest initiation across the former GDR, 1989
     las=1, xaxt="n", bty="n")
axis(side=1, at=seq(1,65,1), labels = FALSE, tck=-0.02); axis(side=1, at=c(0,33,66), labels = FALSE, tck=-0.04)
text(x=c(0, 66), y=rep(-0.07, 2), cex=0.9,  par("usr")[3],
     labels = c("\n04/09/1989:\nProtests start\nin Leipzig", "\n09/11/1989:\n Fall of \nBerlin wall"), srt = 0, pos = 1, xpd = TRUE)
legend(0, 1, c("Counties in 1985-1988 with: \n\nlow number of decomposition measures (0) \n\n",
               "median number of decomposition measures (4)",
               "high number of decomposition measures (17)"),
       lty = c(3:1), col=c("gray70", "gray40", "black"), y.intersp=-2.5, bty = "n")
dev.off()

# pdf("cov_bal.pdf", width = 11, height = 6)
grid.arrange(cobalt1, cobalt2, blank, blank, cobalt3, cobalt4, 
                        ncol = 2, nrow = 3, heights=c(0.45, 0.1, 0.45))
dev.off()


#### Robustness: more controls ####
summary(cox_ovs_rob1 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum + ovs_clean.1984 + 
                                theater_guests.1984 + pop_county.1984.log + city_county + cities + 
                                WGTV + poland + dist_border, data=df3) ) # attitude spillover
summary(cox_ovs_rob2 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum + ovs_clean.1984 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities + 
                                industrial_productivity.1984 + percentage_employed_persons.1984, data=df3) ) # economic
summary(cox_ovs_rob3 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum + ovs_clean.1984 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities + 
                                available_flats.1984 + hospital_beds_per_ht.1984, data=df3) ) # infrastructure
summary(cox_ovs_rob4 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum + ovs_clean.1984 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities +
                                percentage_farmers.1984 + percentage_construction_workers.1984 + percentage_female.1984, data=df3) ) # education/ideology/urban
# Interacting West German TV with imprisonments:
summary(cox_WGTV_int <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum +
                                WGTV + I(parrests.sum*WGTV) + ovs_clean.1984 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3) )
# Fixed effects:
summary(cox_FE <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum +
                          ovs_clean.1984 + theater_guests.1984 + pop_county.1984.log + city_county + cities + as.factor(district), data=df3) )
# Cluster robust SEs:
summary(cox_clusterSE <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.sum +
                                 ovs_clean.1984 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df3, cluster = df3$district) )

library(stargazer)
stargazer(cox_ovs_rob1, cox_ovs_rob2, cox_ovs_rob3, cox_ovs_rob4, cox_WGTV_int, cox_FE, cox_clusterSE,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "opks_clean.1984" = "OPKs in 1984",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns",
            "WGTV" = "WGTV",
            "poland" = "Border Poland",
            "dist_border" = "Distance to border",
            "industrial_productivity.1984" = "Indust.\\ productivity",
            "percentage_employed_persons.1984" = "Employement",
            "available_flats.1984" = "Available flats",
            "hospital_beds_per_ht.1984" = "Hospital beds",
            "percentage_farmers.1984" = "Perct.\\ farmers",
            "percentage_construction_workers.1984" = "Perct.\\ work force in construction",
            "percentage_female.1984" = "Perct.\\ female",
            "I(parrests.sum * WGTV)" = "Sum pol.\\ arrests * WGTV"),
          dep.var.caption  = "Dep.\\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)

summary(cox_ovs_pop1 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1988 + ovs_clean.1987 +
                                theater_guests.1984 + city_county + cities +
                                pop_county.1987.log + pop_change_yearly.1987, data=df3) )
summary(cox_ovs_pop2 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1987 + ovs_clean.1986 +
                                theater_guests.1984 + city_county + cities +
                                pop_county.1986.log + pop_change_yearly.1986, data=df3) )
summary(cox_ovs_pop3 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1986 + ovs_clean.1985 +
                                theater_guests.1984 + city_county + cities +
                                pop_county.1985.log + pop_change_yearly.1985, data=df3) )
summary(cox_ovs_pop4 <- coxph(Surv( rep(0, nrow(df3)), time.of.first.protest, rep(1, nrow(df3)) ) ~ parrests.1985 + ovs_clean.1984 +
                                theater_guests.1984 + city_county + cities +
                                pop_county.1984.log + pop_change_yearly.1984, data=df3) )

library(stargazer)
stargazer(cox_ovs_pop4, cox_ovs_pop3, cox_ovs_pop2, cox_ovs_pop1,
          type="latex",
          covariate.labels = c(
            "parrests.1985" = "Pol.\\ arrests, 1985",
            "ovs_clean.1984" = "OVs in 1984",
            "parrests.1986" = "Pol.\\ arrests, 1986",
            "ovs_clean.1985" = "OVs in 1985",
            "parrests.1987" = "Pol.\\ arrests, 1987",
            "ovs_clean.1986" = "OVs in 1986",
            "parrests.1988" = "Pol.\\ arrests, 1988",
            "ovs_clean.1987" = "OVs in 1987",
            "theater_guests.1984" = "Theatre guests 1984",
            "city_county" = "Urban county",
            "cities" = "Towns",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "pop_change_yearly.1984" = "Perct.\\ change pop.\\ size to 1984",
            "pop_county.1985.log" = "Pop.\\ size 1985 (log)",
            "pop_change_yearly.1985" = "Perct.\\ change pop.\\ size to 1985",
            "pop_county.1986.log" = "Pop.\\ size 1986 (log)",
            "pop_change_yearly.1986" = "Perct.\\ change pop.\\ size to 1986",
            "pop_county.1987.log" = "Pop.\\ size 1987 (log)",
            "pop_change_yearly.1987" = "Perct.\\ change pop.\\ size to 1987"),
          dep.var.caption  = "Dep.\\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)





#### Robustness: IV regression ####
iv.data <- df3[is.na(df3$ims_clean.1986)==FALSE & is.na(df3$ims_clean.1987)==FALSE,]
iv.data$parrests.sum.87_88 <- apply(iv.data[,which(colnames(iv.data)%in%c("parrests.1987", "parrests.1988"))], 1, sum, na.rm=TRUE)
iv.data$ims.per.1000.1986 <- iv.data$ims_clean.1986*1000/iv.data$pop_county.1986
iv.data$ims.per.1000.1987 <- iv.data$ims_clean.1987*1000/iv.data$pop_county.1987

summary(iv_1 <- lm(parrests.sum.87_88 ~ OPKs_DVP_86 + ims.per.1000.1986 + 
                     ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )
summary(iv_1_1 <- lm(parrests.1987 ~ OPKs_DVP_86 + ims.per.1000.1986 + 
                       ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )
summary(iv_1_2 <- lm(parrests.1988 ~ OPKs_DVP_87 + ims.per.1000.1987 + 
                       ovs_clean.1986 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )


summary(iv_2 <- coxph(Surv( rep(0, nrow(iv.data)), time.of.first.protest, rep(1, nrow(iv.data)) ) ~ iv_1$fitted.values + 
                        ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )
summary(iv_2_1 <- coxph(Surv( rep(0, nrow(iv.data)), time.of.first.protest, rep(1, nrow(iv.data)) ) ~ iv_1_1$fitted.values + 
                          ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )
summary(iv_2_2 <- coxph(Surv( rep(0, nrow(iv.data)), time.of.first.protest, rep(1, nrow(iv.data)) ) ~ iv_1_2$fitted.values + 
                          ovs_clean.1986 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )


library(stargazer)
stargazer(iv_1, iv_1_1, iv_1_2, 
          type="latex",
          covariate.labels = c(
            "OPKs_DVP_86" = "DVP OPKs in 1986",
            "ims.per.1000.1986" = "IMs/cap in 1986",
            "ovs_clean.1985" = "OVs in 1985",
            "OPKs_DVP_87" = "DVP OPKs in 1987",
            "ims.per.1000.1987" = "IMs/cap in 1987",
            "ovs_clean.1986" = "OVs in 1986",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns") )

stargazer(iv_2, iv_2_1, iv_2_2,
          type="latex",
          covariate.labels = c(
            "fitted.values" = "Pred. sum arrests, 1987-1988",
            "fitted.values" = "Pred. arrests 1987",
            "ovs_clean.1985" = "OVs in 1985",
            "fitted.values" = "Pred. arrests 1988",
            "ovs_clean.1986" = "OVs in 1986",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)

rsq::rsq.partial(lm(parrests.sum.87_88 ~ OPKs_DVP_86 + ims.per.1000.1986 + ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data),
                 lm(parrests.sum.87_88 ~ ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data), type = "n" )
lmtest::lrtest(lm(parrests.sum.87_88 ~ OPKs_DVP_86 + ims.per.1000.1986 + ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data),
               lm(parrests.sum.87_88 ~ ovs_clean.1985 + theater_guests.1984 + pop_county.1984.log + city_county + cities, data=iv.data) )




#### Robustness: dealing with missingness ####
summary(cox_ovs4_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1988  + ovs_clean.1987 +
                                 theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_ovs3_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1987 + ovs_clean.1986 +
                                 theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_ovs2_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1986 + ovs_clean.1985 +
                                 theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_ovs1_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.1985 + ovs_clean.1984 +
                                 theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_ovs_naive_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum,
                                    data=df2) )
summary(cox_ovs_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ parrests.sum + ovs_clean.1984 +
                                theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )
summary(cox_ovs_null_miss <- coxph(Surv( rep(0, nrow(df2)), time.of.first.protest, rep(1, nrow(df2)) ) ~ ovs_clean.1984 +
                                     theater_guests.1984 + pop_county.1984.log + city_county + cities, data=df2) )

library(stargazer)
stargazer(cox_ovs_naive_miss, cox_ovs_miss, cox_ovs_null_miss, cox_ovs1_miss, cox_ovs2_miss, cox_ovs3_miss, cox_ovs4_miss,
          type="latex",
          covariate.labels = c(
            "parrests.sum" = "Sum pol.\\ arrests, 1985-1988",
            "parrests.1985" = "Pol.\\ arrests, 1985",
            "ovs_clean.1984" = "OVs in 1984",
            "parrests.1986" = "Pol.\\ arrests, 1986",
            "ovs_clean.1985" = "OVs in 1985",
            "parrests.1987" = "Pol.\\ arrests, 1987",
            "ovs_clean.1986" = "OVs in 1986",
            "parrests.1988" = "Pol.\\ arrests, 1988",
            "ovs_clean.1987" = "OVs in 1987",
            "theater_guests.1984" = "Theatre guests 1984",
            "pop_county.1984.log" = "Pop.\\ size 1984 (log)",
            "city_county" = "Urban county",
            "cities" = "Towns"),
          dep.var.caption  = "Dep.\ variable: time until mobilization in 1989",
          apply.coef = exp, p.auto = FALSE)

library(Amelia)
set.seed(1111)
a.out <- amelia(df2[,which(colnames(df2)%in%c(model.variables, 
                                              "time.of.first.protest", "district",
                                              "parrests.1988", "parrests.1987", "parrests.1986", "parrests.1985",
                                              "ovs_clean.1984", "ovs_clean.1985", "ovs_clean.1986", "ovs_clean.1987",
                                              "WGTV", "poland", "dist_border", "industrial_productivity.1984", "percentage_employed_persons.1984",
                                              "available_flats.1984", "percentage_farmers.1984", "percentage_construction_workers.1984")) ], 
                m = 5, p2s=1, idvars = c("parrests.1988", "parrests.1987", "parrests.1986", "parrests.1985"), ords=c("city_county", "WGTV", "poland"), cs = c("district"))

amelia.results.88 <- list(NULL)
amelia.results.87 <- list(NULL)
amelia.results.86 <- list(NULL)
amelia.results.85 <- list(NULL)
amelia.results.sum <- list(NULL)

for(i in 1:a.out$m){
  cox.a.out <- coxph(Surv( rep(0, nrow(a.out$imputations[[i]])), time.of.first.protest, rep(1, nrow(a.out$imputations[[i]])) ) ~ parrests.1988  + ovs_clean.1987 +
                       theater_guests.1984 + pop_county.1984.log + city_county + cities, data=a.out$imputations[[i]])
  amelia.results.88[[i]] <- summary(cox.a.out)$coefficients}
for(i in 1:a.out$m){
  cox.a.out <- coxph(Surv( rep(0, nrow(a.out$imputations[[i]])), time.of.first.protest, rep(1, nrow(a.out$imputations[[i]])) ) ~ parrests.1987 + ovs_clean.1986 +
                       theater_guests.1984 + pop_county.1984.log + city_county + cities, data=a.out$imputations[[i]])
  amelia.results.87[[i]] <- summary(cox.a.out)$coefficients}
for(i in 1:a.out$m){
  cox.a.out <- coxph(Surv( rep(0, nrow(a.out$imputations[[i]])), time.of.first.protest, rep(1, nrow(a.out$imputations[[i]])) ) ~ parrests.1986 + ovs_clean.1985 +
                       theater_guests.1984 + pop_county.1984.log + city_county + cities, data=a.out$imputations[[i]])
  amelia.results.86[[i]] <- summary(cox.a.out)$coefficients}
for(i in 1:a.out$m){
  cox.a.out <- coxph(Surv( rep(0, nrow(a.out$imputations[[i]])), time.of.first.protest, rep(1, nrow(a.out$imputations[[i]])) ) ~ parrests.1985 + ovs_clean.1984 +
                       theater_guests.1984 + pop_county.1984.log + city_county + cities, data=a.out$imputations[[i]])
  amelia.results.85[[i]] <- summary(cox.a.out)$coefficients}
for(i in 1:a.out$m){
  cox.a.out <- coxph(Surv( rep(0, nrow(a.out$imputations[[i]])), time.of.first.protest, rep(1, nrow(a.out$imputations[[i]])) ) ~ parrests.sum + ovs_clean.1984 +
                       theater_guests.1984 + pop_county.1984.log + city_county + cities, data=a.out$imputations[[i]])
  amelia.results.sum[[i]] <- summary(cox.a.out)$coefficients}

max( unlist(lapply(amelia.results.88, `[`,1,5)))<0.05
max( unlist(lapply(amelia.results.87, `[`,1,5)))<0.05
max( unlist(lapply(amelia.results.86, `[`,1,5)))<0.05
max( unlist(lapply(amelia.results.85, `[`,1,5)))<0.05
max( unlist(lapply(amelia.results.sum, `[`,1,5)))<0.05


