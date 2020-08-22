require(vegan)
source('RScripts//getMasterDF.R')


df <- getMasterDF(row.names(R.km2))

df.scale <- data.frame('RP'=df$RP,
                       'R.km2'=scale(log10(df$R.km2)),
                       'P.km2'=scale(log10(df$P.km2)),
                       'R.shape'=scale(df$R.shape),
                       'P.shape'=scale(df$P.shape),
                       'R.lat'=scale(df$R.lat),
                       'R.lon'=scale(df$R.lon))

m1multi <- glm(formula=(RP)~R.km2+P.km2+R.shape+P.shape+R.lat+R.lon, data=df.scale, family='gaussian')



t.RP <- t.test(df$RP, df$Rand.RP, paired=T)
######
m1.g <- glm(formula=(RP)~log10(R.km2), data=df, family = 'gaussian')
residual.df <- data.frame(m1.g$residuals, df[row.names(df)%in%names(m1.g$residuals),])
colnames(residual.df)[1] <- 'Residuals'

intdf <- data.frame(Range=c(rep('Realized', 447), rep('Randomized', 447)),
                    RP=c(df$RP, df$Rand.RP), R.km2=rep(df$R.km2, 2))
inan <- aov(formula=RP~log(R.km2)*Range, data=intdf)
m1.gf <- glm(formula=(RP)~log10(R.km2)*Range, data=intdf)
m1.anova <- anova(m1.gf)


m1a1 <- glm(formula=(RP)~log10(R.km2), data=df, family = 'gaussian')
m1a2 <- glm(formula=(RP)~log10(P.km2), data=df, family = 'gaussian')
m1a3 <- glm(formula=(RP)~R.shape, data=df, family = 'gaussian')
m1a4 <- glm(formula=(RP)~P.shape, data=df, family = 'gaussian')
m1a5 <- glm(formula=(RP)~R.lat, data=df, family = 'gaussian')
m1a6 <- glm(formula=(RP)~R.lon, data=df, family = 'gaussian')

#m2a1 <- lm(formula=Residuals~log10(R.km2), data=residual.df)
m2a2 <- lm(formula=Residuals~log10(P.km2), data=residual.df)
m2a3 <- lm(formula=Residuals~R.shape, data=residual.df)
m2a4 <- lm(formula=Residuals~P.shape, data=residual.df)
m2a5 <- lm(formula=Residuals~R.lat, data=residual.df)
m2a6 <- lm(formula=Residuals~R.lon, data=residual.df)

m3a1 <- glm(formula=(Rand.RP)~log10(R.km2), data=df, family = 'gaussian')
m3a2 <- glm(formula=(Rand.RP)~log10(P.km2), data=df, family = 'gaussian')
m3a3 <- glm(formula=(Rand.RP)~R.shape, data=df, family = 'gaussian')
m3a4 <- glm(formula=(Rand.RP)~P.shape, data=df, family = 'gaussian')
m3a5 <- glm(formula=(Rand.RP)~R.lat, data=df, family = 'gaussian')
m3a6 <- glm(formula=(Rand.RP)~R.lon, data=df, family = 'gaussian')
  

Sq.df <- data.frame(R.km2=c(RsquareAdj(m1a1)[[2]], NA, RsquareAdj(m3a1)[[2]]),
                     P.km2=c(RsquareAdj(m1a2)[[2]], RsquareAdj(m2a2)[[2]], RsquareAdj(m3a2)[[2]]), 
                     R.shape=c(RsquareAdj(m1a3)[[2]], RsquareAdj(m2a3)[[2]], RsquareAdj(m3a3)[[2]]),
                     P.shape=c(RsquareAdj(m1a4)[[2]], RsquareAdj(m2a4)[[2]], RsquareAdj(m3a4)[[2]]),
                     Lat=c(RsquareAdj(m1a5)[[2]], RsquareAdj(m2a5)[[2]], RsquareAdj(m3a5)[[2]]),
                     Long=c(RsquareAdj(m1a6)[[2]], RsquareAdj(m2a6)[[2]], RsquareAdj(m3a6)[[2]]))
row.names(Sq.df) <- c('Range Filling', 'Residuals', 'Randomized')


# Sig.df <- data.frame(R.km2 = c(summary(m1a1)$coefficients[2,4], NA, summary(m3a1)$coefficients[2,4]),
#                     P.km2 = c(summary(m1a2)$coefficients[2,4], summary(m2a2)$coefficients[2,4], summary(m3a2)$coefficients[2,4]),
#                     R.shape=c(summary(m1a3)$coefficients[2,4], summary(m2a3)$coefficients[2,4], summary(m3a3)$coefficients[2,4]),
#                     P.shape=c(summary(m1a4)$coefficients[2,4], summary(m2a4)$coefficients[2,4], summary(m3a4)$coefficients[2,4]),
#                     Lat = c(summary(m1a5)$coefficients[2,4], summary(m2a5)$coefficients[2,4], summary(m3a5)$coefficients[2,4]),
#                     Long = c(summary(m1a6)$coefficients[2,4],  summary(m2a6)$coefficients[2,4], summary(m3a6)$coefficients[2,4]))
# row.names(Sig.df) <- c('Range Filling', 'Residuals', 'Randomized')
# coef.df <- data.frame(R.km2=c(m1a1$coefficients[2], NA, m1a3$coefficients[2]),
#                       P.km2=c(m1a2$coefficients[2], m2a2$coefficients[2], m3a2$coefficients[2]),
#                       R.shape=c(m1a3$coefficients[2], m2a3$coefficients[2], m3a3$coefficients[2]),
#                       P.shape=c(m1a4$coefficients[2], m2a4$coefficients[2], m3a4$coefficients[2]),
#                       Lat=c(m1a5$coefficients[2], m2a5$coefficients[2], m3a5$coefficients[2]),
#                       Long=c(m1a6$coefficients[2], m2a6$coefficients[2], m3a6$coefficients[2]))
# row.names(coef.df) <- c('Range Filling', 'Residuals', 'Randomized')
# serr.df <- data.frame(R.km2=c(m1a1$coefficients[2], NA, m1a3$coefficients[2]),
#                       P.km2=c(m1a2$coefficients[2], m2a2$coefficients[2], m3a2$coefficients[2]),
#                       R.shape=c(m1a3$coefficients[2], m2a3$coefficients[2], m3a3$coefficients[2]),
#                       P.shape=c(m1a4$coefficients[2], m2a4$coefficients[2], m3a4$coefficients[2]),
#                       Lat=c(m1a5$coefficients[2], m2a5$coefficients[2], m3a5$coefficients[2]),
#                       Long=c(m1a6$coefficients[2], m2a6$coefficients[2], m3a6$coefficients[2]))

realized.table <- data.frame(Coefficient=c(summary(m1a1)$coef[2,1], summary(m1a2)$coef[2,1],
                                  summary(m1a3)$coef[2,1], summary(m1a4)$coef[2,1],
                                  summary(m1a5)$coef[2,1], summary(m1a6)$coef[2,1]),
                          Std.Error=c(summary(m1a1)$coef[2,2], summary(m1a2)$coef[2,2],
                                  summary(m1a3)$coef[2,2], summary(m1a4)$coef[2,2],
                                  summary(m1a5)$coef[2,2], summary(m1a6)$coef[2,2]),
                          P.Value =c(summary(m1a1)$coef[2,4], summary(m1a2)$coef[2,4],
                                  summary(m1a3)$coef[2,4], summary(m1a4)$coef[2,4],
                                  summary(m1a5)$coef[2,4], summary(m1a6)$coef[2,4]),
                          AIC     =c(summary(m1a1)$aic, summary(m1a2)$aic, summary(m1a3)$aic,
                                  summary(m1a4)$aic, summary(m1a5)$aic, summary(m1a6)$aic),
                          Adj.Rsq =c(RsquareAdj(m1a1)[[2]], RsquareAdj(m1a2)[[2]], RsquareAdj(m1a3)[[2]],
                                  RsquareAdj(m1a4)[[2]], RsquareAdj(m1a5)[[2]], RsquareAdj(m1a6)[[2]]))
randomed.table <- data.frame(Coefficient=c(summary(m3a1)$coef[2,1], summary(m3a2)$coef[2,1],
                                  summary(m3a3)$coef[2,1], summary(m3a4)$coef[2,1],
                                  summary(m3a5)$coef[2,1], summary(m3a6)$coef[2,1]),
                          Std.Error=c(summary(m3a1)$coef[2,2], summary(m3a2)$coef[2,2],
                                  summary(m3a3)$coef[2,2], summary(m3a4)$coef[2,2],
                                  summary(m3a5)$coef[2,2], summary(m3a6)$coef[2,2]),
                          P.Value =c(summary(m3a1)$coef[2,4], summary(m3a2)$coef[2,4],
                                  summary(m3a3)$coef[2,4], summary(m3a4)$coef[2,4],
                                  summary(m3a5)$coef[2,4], summary(m3a6)$coef[2,4]),
                          AIC     =c(summary(m3a1)$aic, summary(m3a2)$aic, summary(m3a3)$aic,
                                  summary(m3a4)$aic, summary(m3a5)$aic, summary(m3a6)$aic),
                          Adj.Rsq =c(RsquareAdj(m3a1)[[2]], RsquareAdj(m3a2)[[2]], RsquareAdj(m3a3)[[2]],
                                  RsquareAdj(m3a4)[[2]], RsquareAdj(m3a5)[[2]], RsquareAdj(m3a6)[[2]]))
residual.table <- data.frame(Coefficient=c(0, summary(m2a2)$coef[2,1],
                                  summary(m2a3)$coef[2,1], summary(m2a4)$coef[2,1],
                                  summary(m2a5)$coef[2,1], summary(m2a6)$coef[2,1]),
                          Std.Error=c(0, summary(m2a2)$coef[2,2],
                                  summary(m2a3)$coef[2,2], summary(m2a4)$coef[2,2],
                                  summary(m2a5)$coef[2,2], summary(m2a6)$coef[2,2]),
                          P.Value =c(0, summary(m2a2)$coef[2,4],
                                  summary(m2a3)$coef[2,4], summary(m2a4)$coef[2,4],
                                  summary(m2a5)$coef[2,4], summary(m2a6)$coef[2,4]),
                          AIC     =c(0, AIC(m2a2), AIC(m2a3), AIC(m2a4), AIC(m2a5), AIC(m2a6)),
                          Adj.Rsq =c(0, RsquareAdj(m2a2)[[2]], RsquareAdj(m2a3)[[2]],
                                  RsquareAdj(m2a4)[[2]], RsquareAdj(m2a5)[[2]], RsquareAdj(m2a6)[[2]]))
row.names(realized.table) <- c('Range Size ', 'Potential Range Size', 'Shape Ratio',
                            'Potential Shape Ratio', 'Latitude', 'Longitude')
row.names(randomed.table) <- c('Range Size ', 'Potential Range Size', 'Shape Ratio',
                            'Potential Shape Ratio', 'Latitude', 'Longitude')
row.names(residual.table) <- c('Range Size ', 'Potential Range Size', 'Shape Ratio',
                            'Potential Shape Ratio', 'Latitude', 'Longitude')

write.csv(t(Sq.df), 'Output\\results.table.csv')
write.csv(signif(realized.table, 3), 'Output\\realized.table.csv')
write.csv(signif(randomed.table, 3), 'Output\\random.table.csv')
write.csv(signif(residual.table, 3), 'Output\\residual.table.csv')

m.shapeR <- lm(log10(Rylength)~log10(Rxlength), data=df) 
m.shapeP <- lm(log10(Pylength)~log10(Pxlength), data=df)
m.lshapeR <- lm(log10(Rylength)~log10(Rxlength), data=df) 
m.lshapeP <- lm(log10(Pylength)~log10(Pxlength), data=df)
m.shapesizeR <- lm(log10(R.shape)~log10(R.km2), data=df)
m.shapesizeP <- lm(log10(P.shape)~log10(R.km2), data=df)
m.shapeRatio <- lm(log10(R.shape/P.shape)~log10(R.km2), data=df)
m.lon.km <- lm(log10(R.km2)~R.lon, df)
m.lon.re <- lm(Residuals~R.lon, residual.df)
m.lat.km <- lm(log10(R.km2)~R.lat, df)
m.lat.re <- lm(Residuals~R.lat, residual.df)
mPvres <- lm(Residuals~log10(P.km2), residual.df)
mRand <- glm(Rand.RP~log10(R.km2), df, family = 'gaussian')
rand.residuals <- mRand$residuals

df.shape <- rbind(data.frame(type='Realized', shape=df$R.shape, size=df$R.km2), data.frame(type='Potential', shape=df$P.shape, size=df$P.km2))
m.shapetype <- lm(shape~size*type, df.shape)
an.shapetype <- anova(m.shapetype, test='Chisq')

rpdf <- data.frame(RP=c(df$RP, df$Rand.RP), R.km2=c(df$R.km2, df$R.km2), 
                   Range=c(rep('Realized', nrow(df)), rep('Randomized', nrow(df))))

mRand.int <- glm(RP~log10(R.km2)*Range, data=rpdf, family='gaussian')


#m1.2 <- glm(formula=log10(R.km2)~(RP), data=df, family = 'gaussian')
#m2.2 <- glm(formula=na.omit(log10(P.km2))~(m1.2$residuals), data=df)
#m2.res <- data.frame(m2.2$residuals)
#row.names(m2.res) <- row.names(df)[which(is.na(log10(df$P.km2))==F)]
# m11 <- lm(formula=log10(R.km2)~Tmin, data=df)
# m11.resid <- lm(formula=Residuals~Tmin, data=residual.df)
# m12 <- lm(formula=log10(R.km2)~Ffree, data=df)
# m12.resid <- lm(formula=Residuals~Ffree, data=residual.df)
# m13 <-  lm(formula=log10(R.km2)~Precip.min, data=df)
# m13.resid <- lm(formula=Residuals~Precip.min, data=residual.df)
# m14 <-  lm(formula=log10(R.km2)~log10(Precip.max), data=df)
# m14.resid <- lm(formula=Residuals~log10(Precip.max), data=residual.df)
# m15 <-  lm(formula=log10(R.km2)~pH.min, data=df)
# m15.resid <- lm(formula=Residuals~pH.min, data=residual.df)
# m16 <-  lm(formula=log10(R.km2)~pH.max, data=df)
# m16.resid <- lm(formula=Residuals~pH.max, data=residual.df)
# df[,length(df)+1] <- df$pH.max-df$pH.min
# colnames(df)[length(df)] <- 'Phrange'
# m17 <- lm(log10(R.km2)~Phrange, df)
# m17.resid <- lm(Residuals~Phrange, residual.df)
# df[, length(df)+1] <- df$Precip.max-df$Precip.min
# colnames(df)[length(df)] <- 'Preciprange'
# m18 <-  lm(log10(R.km2)~(Preciprange), df)
# m18.resid <- lm(Residuals~Preciprange, residual.df)
# mSoil <- lm(log10(R.km2)~soil, df)
# mSoil.resid <- lm(Residuals~soil, residual.df)
# #m19 <- lm(log10(R.km2)~log10(Acc.min), data=df)
# #m19.resid <- lm(Residuals~log10(Acc.max), residual.df)