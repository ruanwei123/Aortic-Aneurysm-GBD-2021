# Install APC software
library(Matrix)

apc2fit <- function(R, ...)
{
  # Fit age-period-cohort model that includes quadratic terms.
  #
  # Args:
  #   OverDispersion: 1/TRUE (default) or 0/FALSE
  #            RVals: 3-element vector of age, period, and cohort referent values;
  #                   defaults are mid-point values
  #      offset_tick: standard offset unit, default is 10^5
  #        zero_fill: scalar, value to replace 0 counts, default is 0.1
  #              HiC: include higher-order cohort deviations, 1/TRUE (default) or 0/FALSE
  #              HiP: include higher-order period deviations, 1/TRUE (default) or 0/FALSE
  #              HiA: include higher-order age deviations, 1/TRUE (default) or 0/FALSE
  #
  # Returns:
  #  Age-Period-Cohort model outputs
  
  PVP <- checkPVPPAIRS(R, ...)
  A <- length(PVP$D$a)
  P <- length(PVP$D$p)
  C <- length(PVP$D$c)
  
  if (A<3 || P<3) {stop("This functions requires three or more age groups and calendar periods.")}
  if ( (A==3 || P==3) && (PVP$HiA == FALSE || PVP$HiP == FALSE || PVP$HiC == FALSE) ) {
    stop("You must accept default HiA, HiP, and HiC values when there are only three age groups or calendar periods.")}
  
  if (A==3) {A3NULL <- cbind(matrix(PVP$D$a), matrix(0, nrow=A), matrix(-1E-6, nrow=A), matrix(+1E-6, nrow=A))}
  if (P==3) {P3NULL <- cbind(matrix(PVP$D$p), matrix(0, nrow=P), matrix(-1E-6, nrow=P), matrix(+1E-6, nrow=P))}
  
  D <- designmatrix(PVP)
  Pt <- D$Pt
  
  MOD <- 1 + PVP$HiA*4 + PVP$HiP*2 + PVP$HiC*1
  
  if (MOD == 1) {
    # No HiA, No HiP, No HiC
    apcM <- APCFIT(PVP, D$X[,1:6])
    mapc <- length(Pt[[7]]) + length(Pt[[8]]) + length(Pt[[9]])
    apcM$B <- rbind(apcM$B, matrix(0, nrow = mapc)) 
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR, matrix(1E-6, nrow = mapc, ncol= mapc))))
    
  } else if (MOD == 2) {
    # No HiA, No HiP, With HiC
    apcM <- APCFIT(PVP, D$X[,c(1:6, Pt[[9]])])
    ma <- length(Pt[[7]])
    mp <- length(Pt[[8]])
    apcM$B <- rbind(as.matrix(apcM$B[1:6]),
                    matrix(0, nrow = ma + mp), 
                    as.matrix(apcM$B[Pt[[9]] - ma - mp]))
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR[1:6,1:6], 
                                       matrix(1E-6, nrow = ma + mp, ncol = ma + mp), 
                                       apcM$s2VAR[Pt[[9]] - ma - mp, Pt[[9]] - ma - mp])))
  }
  else if (MOD == 3) {
    # No HiA, With HiP, No HiC
    apcM <- APCFIT(PVP, D$X[,c(1:6, Pt[[8]])])
    ma <- length(Pt[[7]])
    mc <- length(Pt[[9]])
    apcM$B <- rbind(as.matrix(apcM$B[1:6]),
                    matrix(0, nrow = ma), 
                    as.matrix(apcM$B[Pt[[8]] - ma]),
                    matrix(0, nrow = mc))
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR[1:6,1:6], 
                                       matrix(1E-6, nrow = ma, ncol = ma),
                                       apcM$s2VAR[Pt[[8]] - ma, Pt[[8]] - ma],
                                       matrix(1E-6, nrow = mc, ncol = mc)))) 
  }
  else if (MOD == 4) {
    # No HiA, With HiP, With HiC
    apcM <- APCFIT(PVP, D$X[,c(1:6, Pt[[8]], Pt[[9]])])
    ma <- length(Pt[[7]])
    apcM$B <- rbind(as.matrix(apcM$B[1:6]),
                    matrix(0, nrow = ma), 
                    as.matrix(apcM$B[c(Pt[[8]], Pt[[9]]) - ma]))
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR[1:6,1:6], 
                                       matrix(1E-6, nrow = ma, ncol = ma),
                                       apcM$s2VAR[c(Pt[[8]], Pt[[9]]) - ma, c(Pt[[8]], Pt[[9]]) - ma]))) 
  }
  else if (MOD == 5) {
    # With HiA, No HiP, No HiC
    apcM <- APCFIT(PVP, D$X[,c(1:6, Pt[[7]])])
    mpc <- length(Pt[[8]]) + length(Pt[[9]])
    apcM$B <- rbind(as.matrix(apcM$B),
                    matrix(0, nrow = mpc))
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR, 
                                       matrix(1E-6, nrow = mpc, ncol = mpc)))) 
  }
  else if (MOD == 6) {
    # With HiA, No HiP, With HiC
    INC0 <- c(1:6, Pt[[7]])
    INC1 <- Pt[[9]]
    apcM <- APCFIT(PVP, D$X[,c(INC0, INC1)])
    mp <- length(Pt[[8]]) 
    apcM$B <- rbind(as.matrix(apcM$B[INC0]),
                    matrix(0, nrow = mp),
                    as.matrix(apcM$B[INC1 - mp]))
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR[INC0,INC0],
                                       matrix(0, nrow = mp, ncol = mp),
                                       apcM$s2VAR[ INC1 - mp, INC1 - mp ])))  
  }
  else if (MOD == 7) {
    # With HiA, With HiP, No HiC
    apcM <- APCFIT(PVP, D$X[,c(1:6, Pt[[7]], Pt[[8]])])
    mc <- length(Pt[[9]]) 
    INC <- c(1:6, Pt[[7]], Pt[[8]])
    apcM$B <- rbind(apcM$B, matrix(0, nrow = mc))
    apcM$s2VAR <- as.matrix(bdiag(list(apcM$s2VAR, matrix(1E-6, nrow = mc, ncol = mc))))
  }
  else if (MOD == 8) {
    # With HiA, With HiP, With HiC
    apcM <- APCFIT(PVP, D$X)
    
  }
  else
  { stop("Invalid value or values for HiA, HiP, or HiC.") }
  
  
  B <- apcM$B
  s2VAR <-apcM$s2VAR
  
  
  #
  # (0) Fitted rates
  #
  ETA <- D$X%*%B
  # Formula to compute only the variances of fitted rates, not any covariances.
  v <- rowSums((D$X%*%s2VAR)*D$X)
  # Scale the values such that naive formulae yield correct variances.
  v[v<0] <- NaN
  EFit <- matrix(1/v, nrow=A, ncol=P)
  OFit <- matrix((1/v)*exp(-ETA), nrow=A, ncol=P)
  FittedRates <- list(name = paste('Fitted', PVP$name),
                      events = EFit, 
                      offset = OFit, 
                      offset_tick = PVP$offset_tick, 
                      ages = R$ages, 
                      periods = R$periods);
  
  #
  # (1) Coefficents
  # Intercept, LAT, NetDrift, CAT, THETAa, THETAp, THETAc
  #
  XCO <- matrix(c(1, 0,  0, 0, 0, 0,
                  0, 1,  0, 0, 0, 0,
                  0, 0,  1, 0, 0, 0,
                  0, 1, -1, 0, 0, 0,
                  0, 0,  0, 1, 0, 0,
                  0, 0,  0, 0, 1, 0,
                  0, 0,  0, 0, 0, 1),
                nrow = 7, byrow = T)
  b7 <- XCO%*%B[1:6]
  v7 <- XCO%*%s2VAR[1:6,1:6]%*%t(XCO)
  s7 <- matrix(sqrt(diag(v7)))
  c7 <- cbind(b7 - 1.96*s7, b7 + 1.96*s7)
  Coefficients <- cbind(b7, s7, c7)
  dimnames(Coefficients) <- list(c("Intercept","LAT","NetDrift", "CAT", "THETAa", "THETAp", "THETAc"), c("Parameter","SD","CILo", "CIHi"))
  
  #
  # Wald test - NetDrift different from 0?
  #
  X21 <- (b7[3]/s7[3])^2
  df1 <- 1
  PVAL1 <- pchisq(X21, df1,lower.tail = FALSE)
  #
  # Net Drift - as estimated annual percentage change (EAPC)
  #
  b3 <- b7[3];
  v3 <- v7[3, 3]
  s3 <- sqrt(v3)
  c3 <- cbind(b3 - 1.96*s3, b3 + 1.96*s3)
  NetDrift <- cbind(b3, c3)
  NetDrift <- 100*(exp(NetDrift) - 1)
  dimnames(NetDrift) <- list( c(), c("Net Drift (%/year)", "CILo", "CIHi") )
  
  #
  # Global Curvature
  #
  bq <- b7[5:7]
  sq <- matrix(sqrt(diag(v7[5:7,5:7])))
  cq <- cbind(bq - 1.96*sq, bq + 1.96*sq)
  eq <- 100*(exp(bq)-1)
  ecq <- 100*(exp(cq)-1)
  GlobalCurvature <- cbind(eq, ecq)
  dimnames(GlobalCurvature) <- list(c("Age", "Period", "Cohort"), c("%/yr^2", "CILo", "CIHi"))
  
  #
  # (2a) Quadratic Age Deviations
  #
  astar <- matrix(PVP$D$a)
  abar <- mean(astar)
  aref <- PVP$RVals[1]
  arefLOC <- match(aref, astar)
  XAQ <- matrix(D$X[D$INCa, 4])
  ad2 <- XAQ%*%B[4]
  ad2v <- XAQ%*%s2VAR[4,4]%*%t(XAQ)
  sd <- matrix(sqrt(diag(ad2v)))
  ci <- cbind(ad2 - 1.96*sd, ad2 + 1.96*sd)
  QuadAgeDeviations <- cbind(astar, ad2, ci)
  dimnames(QuadAgeDeviations) <- list(c(), c("Age", "Deviation", "CILo", "CIHi"))
  # Wald test - Quadratic Age Effect different from 0?
  X22 <- (b7[5]/s7[5])^2
  df2 <- 1
  PVAL2 <- pchisq(X22, df2, lower.tail = FALSE)
  
  #
  # (2b) Higher-Order Age Deviations
  #  
  XAD <- matrix(D$X[D$INCa, Pt[[7]]], nrow = A, byrow = F)
  adh <- XAD%*%B[Pt[[7]]]
  adhv <- XAD%*%s2VAR[Pt[[7]],Pt[[7]]]%*%t(XAD)
  sd <- matrix(sqrt(diag(adhv)))
  ci <- cbind(adh - 1.96*sd, adh + 1.96*sd)
  HiOrdAgeDeviations <- cbind(astar, adh, ci)
  if (A==3) {HiOrdAgeDeviations <- A3NULL}
  dimnames(HiOrdAgeDeviations) <- list(c(), c("Age", "Deviation", "CILo", "CIHi"))
  # Wald test - any Higher-Order age deviations different from 0?
  if (PVP$HiA==1 && A>3){
    X23 <- t(matrix(adh[2:(A-2)]))%*%solve(adhv[2:(A-2),2:(A-2)],matrix(adh[2:(A-2)]))
    df3 <- A - 3
    PVAL3 <- pchisq(X23, df3, lower.tail = FALSE)
  }
  else{
    X23 <- NaN
    df3 <- NaN
    PVAL3 <- 1
  }
  
  #
  # (2c) Age Deviations, Complete
  #
  XA <- cbind(XAQ, XAD)
  ad <- XA%*%B[c(4, Pt[[7]])]
  adv <- XA%*%s2VAR[c(4, Pt[[7]]),c(4, Pt[[7]])]%*%t(XA)
  sd <- matrix(sqrt(diag(adv)))
  ci <- cbind(ad - 1.96*sd, ad + 1.96*sd)
  AgeDeviations <- cbind(astar, ad, ci)
  dimnames(AgeDeviations) <- list(c(), c("Age", "Deviation", "CILo", "CIHi"))
  if (A==3) {AgeDeviations <- QuadAgeDeviations; adv <- ad2v}
  # Wald test - any complete age deviations different from 0?
  if (PVP$HiA==1 && A>3){
    X24 <- t(matrix(ad[2:(A-1)]))%*%solve(adv[2:(A-1),2:(A-1)],matrix(ad[2:(A-1)]))
    df4 <- A - 2
    PVAL4 <- pchisq(X24, df4, lower.tail = FALSE)
  }
  else{
    X24 <- (b7[5]/s7[5])^2
    df4 <- 1
    PVAL4 <- pchisq(X24, df4, lower.tail = FALSE)
  }
  
  #
  # (3a) Quadratic Period Deviations
  #
  pstar <- matrix(PVP$D$p)
  pbar <- mean(pstar)
  pref <- PVP$RVals[2]
  prefLOC <- match(pref, pstar)
  XPQ <- matrix(D$X[D$INCp, 5])
  pd2 <- XPQ%*%B[5]
  pd2v <- XPQ%*%s2VAR[5,5]%*%t(XPQ)
  sd <- matrix(sqrt(diag(pd2v)))
  ci <- cbind(pd2 - 1.96*sd, pd2 + 1.96*sd)
  QuadPerDeviations <- cbind(pstar, pd2, ci)
  dimnames(QuadPerDeviations) <- list(c(), c("Period", "Deviation", "CILo", "CIHi"))
  # Wald test - Quadratic Period Effect different from 0?
  X25 <- (b7[6]/s7[6])^2
  df5 <- 1
  PVAL5 <- pchisq(X25, df5,lower.tail = FALSE)
  
  #
  # (3b) Higher-Order Period Deviations
  #
  XPD <- matrix(D$X[D$INCp, Pt[[8]]], nrow = P, byrow = F)
  pdh <- XPD%*%B[Pt[[8]]]
  pdhv <- XPD%*%s2VAR[Pt[[8]],Pt[[8]]]%*%t(XPD)
  sd <- matrix(sqrt(diag(pdhv)))
  ci <- cbind(pdh - 1.96*sd, pdh + 1.96*sd)
  HiOrdPerDeviations <- cbind(pstar, pdh, ci)
  if (P==3) {HiOrdPerDeviations <- P3NULL}
  dimnames(HiOrdPerDeviations) <- list(c(), c("Period", "Deviation", "CILo", "CIHi"))
  # Wald test - any Higher-Order per deviations different from 0?
  if (PVP$HiP==1 && P>3){
    X26 <- t(matrix(pdh[2:(P-2)]))%*%solve(pdhv[2:(P-2),2:(P-2)],matrix(pdh[2:(P-2)]))
    df6 <- P - 3
    PVAL6 <- pchisq(X26, df6, lower.tail = FALSE)
  }
  else {
    X26 <- NaN
    df6 <- NaN
    PVAL6 <- 1
  }
  
  #
  # (3c) Period Deviations, Complete
  #
  XP <- cbind(XPQ, XPD)
  pd <- XP%*%B[c(5, Pt[[8]])]
  pdv <- XP%*%s2VAR[c(5, Pt[[8]]),c(5, Pt[[8]])]%*%t(XP)
  sd <- matrix(sqrt(diag(pdv)))
  ci <- cbind(pd - 1.96*sd, pd + 1.96*sd)
  PerDeviations <- cbind(pstar, pd, ci)
  if (P==3) {PerDeviations <- QuadPerDeviations; pdv <- pd2v}
  dimnames(PerDeviations) <- list(c(), c("Period", "Deviation", "CILo", "CIHi"))
  # Wald test - any complete period deviations different from 0?
  if (PVP$HiP==1 && P>3){
    X27 <- t(matrix(pd[2:(P-1)]))%*%solve(pdv[2:(P-1),2:(P-1)],matrix(pd[2:(P-1)]))
    df7 <- P - 2
    PVAL7 <- pchisq(X27, df7, lower.tail = FALSE)
  }
  else{
    X27 <- X25
    df7 <- df5
    PVAL7 <- PVAL5
  }
  
  #
  # (4a) Quadratic Cohort Deviations
  #
  cstar <- matrix(PVP$D$c)
  cbar <- mean(cstar)
  cref <- PVP$RVals[3]
  crefLOC <- match(cref, cstar)
  XCQ <- matrix(D$X[D$INCc, 6])
  cd2 <- XCQ%*%B[6]
  cd2v <- XCQ%*%s2VAR[6,6]%*%t(XCQ)
  sd <- matrix(sqrt(diag(cd2v)))
  ci <- cbind(cd2 - 1.96*sd, cd2 + 1.96*sd)
  QuadCohDeviations <- cbind(cstar, cd2, ci)
  dimnames(QuadCohDeviations) <- list(c(), c("Cohort", "Deviation", "CILo", "CIHi"))
  # Wald test - Quadratic Cohort Effect different from 0?
  X28 <- (b7[7]/s7[7])^2
  df8 <- 1
  PVAL8 <- pchisq(X28, df8, lower.tail = FALSE)
  
  #
  # (4b) Higher-Order Cohort Deviations
  #
  XCD <- matrix(D$X[D$INCc, Pt[[9]]], nrow = C, byrow = F)
  cdh <- XCD%*%B[Pt[[9]]]
  cdhv <- XCD%*%s2VAR[Pt[[9]],Pt[[9]]]%*%t(XCD)
  sd <- matrix(sqrt(diag(cdhv)))
  ci <- cbind(cdh - 1.96*sd, cdh + 1.96*sd)
  HiOrdCohDeviations <- cbind(cstar, cdh, ci)
  dimnames(HiOrdCohDeviations) <- list(c(), c("Cohort", "Deviation", "CILo", "CIHi"))
  # Wald test - any Higher-Order coh deviations different from 0?
  if (PVP$HiC==1){
    X29 <- t(matrix(cdh[3:(C-1)]))%*%solve(cdhv[3:(C-1),3:(C-1)],matrix(cdh[3:(C-1)]))
    df9 <- C - 3
    PVAL9 <- pchisq(X29, df9, lower.tail = FALSE)
  }
  else{
    X29 <- NaN
    df9 <- NaN
    PVAL9 <- 1
  }
  #
  # (4c) Cohort Deviations, Complete
  #
  XC <- cbind(XCQ, XCD)
  cd <- XC%*%B[c(6, Pt[[9]])]
  cdv <- XC%*%s2VAR[c(6, Pt[[9]]),c(6, Pt[[9]])]%*%t(XC)
  sd <- matrix(sqrt(diag(cdv)))
  ci <- cbind(cd - 1.96*sd, cd + 1.96*sd)
  CohDeviations <- cbind(cstar, cd, ci)
  dimnames(CohDeviations) <- list(c(), c("Cohort", "Deviation", "CILo", "CIHi"))
  # Wald test - any complete coh deviations different from 0?
  if (PVP$HiC==1){
    X210 <- t(matrix(cd[2:(C-1)]))%*%solve(cdv[2:(C-1),2:(C-1)],matrix(cd[2:(C-1)]))
    df10 <- C - 2
    PVAL10 <- pchisq(X210, df10, lower.tail = FALSE)
  }
  else{
    X210 <- X28
    df10 <- df8
    PVAL10 <- PVAL8
  }
  
  #
  # (5a) Quadratic Longitudinal Age Curve - offset by complete cohort
  # deviation, plus net drift times offset of reference value of cohort
  # versus mean value of cohort
  #
  lot <- log(PVP$offset_tick)
  XLAQ <- cbind(matrix(1, A), astar-abar, matrix(1, A)%*%(cref - cbar), XAQ, matrix(1, A)%*%XC[crefLOC,])
  lac2 <- lot + XLAQ%*%B[c(1, 2, 3, 4, 6, Pt[[9]])]
  lac2v <- XLAQ%*%s2VAR[c(1, 2, 3, 4, 6, Pt[[9]]), c(1, 2, 3, 4, 6, Pt[[9]])]%*%t(XLAQ)
  sd <- matrix(sqrt(diag(lac2v)))
  ci <- cbind(lac2 - 1.96*sd, lac2 + 1.96*sd)
  QuadLongAge <- cbind(astar, exp(lac2), exp(ci))
  dimnames(QuadLongAge) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  
  #
  # (5b) Quadratic Longitudinal Age Rate Ratios
  #
  TMP <- diag(0, nrow = A)
  TMP[,arefLOC] <- 1
  ARR <- diag(1, nrow = A) - TMP
  LARQ <- ARR%*%XLAQ
  lac2rr <- LARQ%*%B[c(1, 2, 3, 4, 6, Pt[[9]])]
  lac2rrv <- LARQ%*%s2VAR[c(1, 2, 3, 4, 6, Pt[[9]]), c(1, 2, 3, 4, 6, Pt[[9]])]%*%t(LARQ)
  sd <- matrix(sqrt(diag(lac2rrv)))
  ci <- cbind(lac2rr - 1.96*sd, lac2rr + 1.96*sd)
  QuadLongAgeRR <- cbind(astar, exp(lac2rr), exp(ci))
  dimnames(QuadLongAgeRR) <- list(c(), c("Age", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (5c) Complete Longitudinal Age Curve
  #
  XLA <- cbind(matrix(1, A), astar-abar, matrix(1, A)%*%(cref - cbar), XAQ, XAD, matrix(1, A)%*%XC[crefLOC,])
  lac <- lot + XLA%*%B[c(1, 2, 3, 4, Pt[[7]], 6, Pt[[9]])]
  lacv <- XLA%*%s2VAR[c(1, 2, 3, 4, Pt[[7]], 6, Pt[[9]]), c(1, 2, 3, 4, Pt[[7]], 6, Pt[[9]])]%*%t(XLA)
  sd <- matrix(sqrt(diag(lacv)))
  ci <- cbind(lac - 1.96*sd, lac + 1.96*sd)
  LongAge <- cbind(astar, exp(lac), exp(ci))
  if (A==3) {LongAge <- QuadLongAge; lacv <- lac2v}
  dimnames(LongAge) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  
  #
  # (5d) Longitudinal Age Rate Ratios
  #
  LAR <- ARR%*%XLA
  lacrr <- LAR%*%B[c(1, 2, 3, 4, Pt[[7]], 6, Pt[[9]])]
  lacrrv <- LAR%*%s2VAR[c(1, 2, 3, 4, Pt[[7]], 6, Pt[[9]]), c(1, 2, 3, 4, Pt[[7]], 6, Pt[[9]])]%*%t(LAR)
  sd <- matrix(sqrt(diag(lacrrv)))
  ci <- cbind(lacrr - 1.96*sd, lacrr + 1.96*sd)
  LongAgeRR <- cbind(astar, exp(lacrr), exp(ci))
  if (A==3) {LongAgeRR <- QuadLongAgeRR; lacrrv <- lac2rrv}
  dimnames(LongAgeRR) <- list(c(), c("Age", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (6a) Quadratic Cross-Sectional Age Curve - offset by complete period
  # deviation plus net drift times offset of reference value of period versus
  # mean value of period 
  #
  if (P>3){
    XXAQ <- cbind(matrix(1, A), astar-abar, matrix(1, A)%*%(pref - pbar) - (astar-abar), XAQ, matrix(1, A)%*%XP[prefLOC,])
    BINC2 <- c(1, 2, 3, 4, 5, Pt[[8]])}
  else{
    XXAQ <- cbind(matrix(1, A), astar-abar, matrix(1, A)%*%(pref - pbar) - (astar-abar), XAQ, matrix(1, A)%*%XPQ[prefLOC,])
    BINC2 <- c(1, 2, 3, 4, 5)}
  xac2 <- lot + XXAQ%*%B[BINC2]
  xac2v <- XXAQ%*%s2VAR[BINC2, BINC2]%*%t(XXAQ)
  sd <- matrix(sqrt(diag(xac2v)))
  ci <- cbind(xac2 - 1.96*sd, xac2 + 1.96*sd)
  QuadCrossAge <- cbind(astar, exp(xac2), exp(ci))
  dimnames(QuadCrossAge) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  
  #
  # (6b) Quadratic Cross-Sectional Age Rate Ratios
  #
  XARQ <- ARR%*%XXAQ
  xac2rr <- XARQ%*%B[BINC2]
  xac2rrv <- XARQ%*%s2VAR[BINC2,BINC2]%*%t(XARQ)
  sd <- matrix(sqrt(diag(xac2rrv)))
  ci <- cbind(xac2rr - 1.96*sd, xac2rr + 1.96*sd)
  QuadCrossAgeRR <- cbind(astar, exp(xac2rr), exp(ci))
  dimnames(QuadCrossAgeRR) <- list(c(), c("Age", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (6c) Complete Cross-Sectional Age Curve 
  #
  if (P>3){
    XXA <- cbind(matrix(1, A), astar-abar, matrix(1, A)%*%(pref - pbar) - (astar-abar), XAQ, XAD, matrix(1, A)%*%XP[prefLOC,])
    BINCc <- c(1, 2, 3, 4, Pt[[7]], 5, Pt[[8]])}
  else{
    XXA <- cbind(matrix(1, A), astar-abar, matrix(1, A)%*%(pref - pbar) - (astar-abar), XAQ, XAD, matrix(1, A)%*%XPQ[prefLOC,])
    BINCc <- c(1, 2, 3, 4, Pt[[7]], 5)}
  xac <- lot + XXA%*%B[BINCc]
  xacv <- XXA%*%s2VAR[BINCc, BINCc]%*%t(XXA)
  sd <- matrix(sqrt(diag(xacv)))
  ci <- cbind(xac - 1.96*sd, xac + 1.96*sd)
  CrossAge <- cbind(astar, exp(xac), exp(ci))
  if (A==3) {CrossAge <- QuadCrossAge; xacv <- xac2v}
  dimnames(CrossAge) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  
  #
  # (6d) Cross-Sectional Age Rate Ratios
  #
  XAR <- ARR%*%XXA
  xacrr <- XAR%*%B[BINCc]
  xacrrv <- XAR%*%s2VAR[BINCc,BINCc]%*%t(XAR)
  sd <- matrix(sqrt(diag(xacrrv)))
  ci <- cbind(xacrr - 1.96*sd, xacrr + 1.96*sd)
  CrossAgeRR <- cbind(astar, exp(xacrr), exp(ci))
  if (A==3) {CrossAgeRR <- QuadCrossAgeRR; xacrrv <- xac2rrv}
  dimnames(CrossAgeRR) <- list(c(), c("Age", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (5c:6c) Ratio of Longitudinal-to-Cross-Sectional Age Curves
  #
  # NOTE: XC <- cbind(XCQ, XCD) and XP <- cbind(XPQ, XPD)
  # XLA <- cbind(matrix(1, A), a-abar, matrix(1, A)%*%(cref - cbar), XAQ, XAD,       matrix(1, A)%*%XC[crefLOC,])
  # B[c(                   1,       2,                            3,   4, Pt[[7]], 6, Pt[[9]])]
  # XXA <- cbind(matrix(1, A), a-abar, matrix(1, A)%*%(pref - pbar) - (a-abar), XAQ, XAD,       matrix(1, A)%*%XP[prefLOC,])
  # B[c(                   1,       2,                                      3,    4, Pt[[7]], 5, Pt[[8]])]
  # For coefficients 1, 2, 4, Pt{7} there is no difference between the
  # longitudinal and cross-sectional age curves. There is a difference for 3,
  # 6, Pt{9}, 5, Pt{8}
  if (P>3){
    BINCR <- c(3, 6, Pt[[9]], 5, Pt[[8]])
    XLX <- cbind(matrix(1, A)*(cref-cbar) - matrix(1, A)*(pref-pbar) + (astar-abar),
                 matrix(1, A)%*%XC[crefLOC,], -1*matrix(1, A)%*%XP[prefLOC,])}
  else{
    BINCR <- c(3, 6, Pt[[9]], 5)
    XLX <- cbind(matrix(1, A)*(cref-cbar) - matrix(1, A)*(pref-pbar) + (astar-abar),
                 matrix(1, A)%*%XC[crefLOC,], -1*matrix(1, A)%*%XPQ[prefLOC,])}
  lvcrr <- XLX%*%B[BINCR]
  lvcrrv <- XLX%*%s2VAR[BINCR,BINCR]%*%t(XLX)
  sd <- matrix(sqrt(diag(lvcrrv)))
  ci <- cbind(lvcrr - 1.96*sd, lvcrr + 1.96*sd)
  Long2CrossRR <- cbind(astar, exp(lvcrr), exp(ci))
  dimnames(Long2CrossRR) <- list(c(), c("Age", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (7a) Gradient of Longitudinal Age Curve: linear & Quadratic Components
  #
  # Scale Factor
  Delta <- astar[2] - astar[1]
  # Gradient Operator
  G <- (1/Delta)*cbind(diag(A-1), matrix(0, A-1))%*%(-diag(A) + as.matrix(bandSparse(A, m = A, 1, matrix(1,A-1))))
  lac2_grad <- G%*%lac2
  lac2_gradv <- G%*%lac2v%*%t(G)
  sd <- matrix(sqrt(diag(lac2_gradv)))
  ci <- cbind(lac2_grad - 1.96*sd, lac2_grad + 1.96*sd)
  QuadLongAgeGrad <- cbind(astar[1:A-1], 100*(exp(cbind(lac2_grad, ci))-1))
  dimnames(QuadLongAgeGrad) <- list(c(), c("Age", "Percent Change per Year of Age", "CILo", "CIHi"))
  
  #
  # (7b) Gradient of Longitudinal Age Curve: Higher-Order Components
  #
  lach <- lot + XAD%*%B[Pt[[7]]]
  lachv <- XAD%*%s2VAR[Pt[[7]], Pt[[7]]]%*%t(XAD)
  lach_grad <- G%*%lach
  lach_gradv <- G%*%lachv%*%t(G)
  sd <- matrix(sqrt(diag(lach_gradv)))
  ci <- cbind(lach_grad - 1.96*sd, lach_grad + 1.96*sd)
  # Multiplier of gradient of linear and quadratic components
  HiOrdLongAgeGrad <- cbind(astar[1:A-1], exp(cbind(lach_grad, ci)))
  if (A==3) {HiOrdLongAgeGrad <- A3NULL}
  dimnames(HiOrdLongAgeGrad) <- list(c(),c("Age", "Rate Multiplier", "CILo", "CIHi"))
  
  #
  # (7c) Gradient of Complete Longitudinal Age Curve
  #
  lac_grad <- G%*%lac
  lac_gradv <- G%*%lacv%*%t(G)
  sd <- matrix(sqrt(diag(lac_gradv)))
  ci <- cbind(lac_grad - 1.96*sd, lac_grad + 1.96*sd)
  LongAgeGrad <- cbind(astar[1:A-1], 100*(exp(cbind(lac_grad, ci))-1))
  if (A==3) {LongAgeGrad <- QuadLongAgeGrad; lac_gradv <- lac2_gradv}
  dimnames(LongAgeGrad) <- list(c(), c("Age", "Percent Change per Year of Age", "CILo", "CIHi"))
  
  #
  # (8a) Gradient of Cross-Sectional Curve: linear & Quadratic Components
  #
  xac2_grad <- G%*%xac2
  xac2_gradv <- G%*%xac2v%*%t(G)
  sd <- matrix(sqrt(diag(xac2_gradv)))
  ci <- cbind(xac2_grad - 1.96*sd, xac2_grad + 1.96*sd)
  QuadCrossAgeGrad <- cbind(astar[1:A-1], 100*(exp(cbind(xac2_grad, ci))-1))
  dimnames(QuadCrossAgeGrad) <- list(c(), c("Age", "Percent Change per Year of Age", "CILo", "CIHi"))
  
  #
  # (8b) Gradient of Cross-Sectional Age Curve: Higher-Order Components
  #
  # Same as for Longitudinal Age Curve
  HiOrdCrossAgeGrad <- HiOrdLongAgeGrad
  xach_gradv <- lach_gradv
  
  #
  # (8c) Gradient of Complete Cross-Sectional Age Curve
  #
  xac_grad <- G%*%xac
  xac_gradv <- G%*%xacv%*%t(G)
  sd <- matrix(sqrt(diag(xac_gradv)))
  ci <- cbind(xac_grad - 1.96*sd, xac_grad + 1.96*sd)
  CrossAgeGrad <- cbind(astar[1:A-1], 100*(exp(cbind(xac_grad, ci))-1))
  if (A==3) {CrossAgeGrad <- QuadCrossAgeGrad; xac_gradv <- xac2_gradv}
  dimnames(CrossAgeGrad) <- list(c(), c("Age", "Percent Change per Year of Age", "CILo", "CIHi"))
  
  #
  # (9a) Quadratic Fitted Temporal Trends -  offset by complete age deviation
  # plus CAT times offset of reference value of age versus mean value of age
  # 
  XPTQ <- cbind(matrix(1, P), matrix(1, P)*(aref - abar), ((pstar-pbar) - matrix(1, P)*(aref-abar)), XPQ, matrix(1, P)%*%XA[arefLOC,])
  ftt2 <- lot + XPTQ%*%B[c(1, 2, 3, 5, 4, Pt[[7]])]
  ftt2v <- XPTQ%*%s2VAR[c(1, 2, 3, 5, 4, Pt[[7]]), c(1, 2, 3, 5, 4, Pt[[7]])]%*%t(XPTQ)
  sd <- matrix(sqrt(diag(ftt2v)))
  ci <- cbind(ftt2 - 1.96*sd, ftt2 + 1.96*sd)
  QuadFittedTemporalTrends <- cbind(pstar, exp(ftt2), exp(ci))
  dimnames(QuadFittedTemporalTrends) <- list(c(), c("Period", "Rate", "CILo", "CIHi"))
  
  #
  # (9b) Quadratic Period Rate Ratios
  #
  Xp <- cbind(pstar - pref, XPQ)
  TMP <- diag(0, nrow = P)
  TMP[,prefLOC] <- 1
  PRR <- diag(1, nrow = P) - TMP
  XPR <- PRR%*%Xp
  prr2 <- XPR%*%B[c(3, 5)]
  prr2v <- XPR%*%s2VAR[c(3, 5), c(3, 5)]%*%t(XPR)
  sd <- matrix(sqrt(diag(prr2v)))
  ci <- cbind(prr2 - 1.96*sd, prr2 + 1.96*sd)
  QuadPeriodRR <- cbind(pstar, exp(prr2), exp(ci))
  dimnames(QuadPeriodRR) <- list(c(), c("Period", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (9c) Complete Fitted Temporal Trends
  #
  if (A>3) {
    XPT <- cbind(matrix(1, P), matrix(1, P)*(aref - abar), ((pstar-pbar) - matrix(1, P)*(aref-abar)), XPQ, XPD, matrix(1, P)%*%XA[arefLOC,])
    INCT <- c(1, 2, 3, 5, Pt[[8]], 4, Pt[[7]])}
  else {
    XPT <- cbind(matrix(1, P), matrix(1, P)*(aref - abar), ((pstar-pbar) - matrix(1, P)*(aref-abar)), XPQ, XPD, matrix(1, P)%*%XAQ[arefLOC,])
    INCT <- c(1, 2, 3, 5, Pt[[8]], 4)}
  ftt <- lot + XPT%*%B[INCT]
  fttv <- XPT%*%s2VAR[INCT, INCT]%*%t(XPT)
  sd <- matrix(sqrt(diag(fttv)))
  ci <- cbind(ftt - 1.96*sd, ftt + 1.96*sd)
  FittedTemporalTrends <- cbind(pstar, exp(ftt), exp(ci))
  if (P==3) {FittedTemporalTrends <- QuadFittedTemporalTrends; fttv <- ftt2v}
  dimnames(FittedTemporalTrends) <- list(c(), c("Period", "Rate", "CI Lo", "CI Hi"))
  
  #
  # (9d) Complete Period Rate Ratios
  #
  if (PVP$HiP==1 && P>3){
    Xp <- cbind(pstar - pref, XP)
    XPR <- PRR%*%Xp
    prr <- XPR%*%B[c(3, 5, Pt[[8]])]
    prrv <- XPR%*%s2VAR[c(3, 5, Pt[[8]]), c(3, 5, Pt[[8]])]%*%t(XPR)
    sd <- matrix(sqrt(diag(prrv)))
    ci <- cbind(prr - 1.96*sd, prr + 1.96*sd)
    PeriodRR <- cbind(pstar, exp(prr), exp(ci))
    dimnames(PeriodRR) <- list(c(), c("Period", "Rate Ratio", "CI Lo", "CI Hi"))
    #  Wald test - any PeriodRR values different from 1?
    INC11 <- c(1:(prefLOC-1), (prefLOC+1):P)
    X211 <- t(matrix(prr[INC11]))%*%solve(prrv[INC11, INC11],matrix(prr[INC11]))
    df11 <- P - 1
    PVAL11 <- pchisq(X211, df11, lower.tail = FALSE)
  }
  else{
    Xp <- cbind(pstar - pref, XPQ)
    XPR <- PRR%*%Xp
    prr <- XPR%*%B[c(3, 5)]
    prrv <- XPR%*%s2VAR[c(3, 5), c(3, 5)]%*%t(XPR)
    sd <- matrix(sqrt(diag(prrv)))
    ci <- cbind(prr - 1.96*sd, prr + 1.96*sd)
    PeriodRR <- cbind(pstar, exp(prr), exp(ci))
    dimnames(PeriodRR) <- list(c(), c("Period", "Rate Ratio", "CI Lo", "CI Hi"))
    #  Wald test - any PeriodRR values different from 1?
    INC11 <- c(1, P)
    X211 <- t(matrix(prr[INC11]))%*%solve(prrv[INC11, INC11],matrix(prr[INC11]))
    df11 <- 2
    PVAL11 <- pchisq(X211, df11, lower.tail = FALSE)
  }
  
  #
  # (10a) Complete Fitted Cohort Pattern centered on the reference age -
  # offset by complete age deviation plus LAT times offset of reference value
  # of age versus mean value of age
  #
  if (A>3){
    XCT <- cbind(matrix(1, C), matrix(1, C)*(aref - abar), (cstar - cbar), XC, matrix(1, C)%*%XA[arefLOC,])
    INCF <- c(1, 2, 3, 6, Pt[[9]], 4, Pt[[7]])}
  else{
    XCT <-cbind(matrix(1, C), matrix(1, C)*(aref - abar), (cstar - cbar), XC, matrix(1, C)%*%XAQ[arefLOC,])
    INCF <- c(1, 2, 3, 6, Pt[[9]], 4)}
  fcp <- lot + XCT%*%B[INCF]
  fcpv <- XCT%*%s2VAR[INCF,INCF]%*%t(XCT)
  sd <- matrix(sqrt(diag(fcpv)))
  ci <- cbind(fcp - 1.96*sd, fcp + 1.96*sd)
  FittedCohortPattern <- cbind(cstar, exp(fcp), exp(ci))
  dimnames(FittedCohortPattern) <- list(c(), c("Cohort", "Rate", "CILo", "CIHi"))
  
  #
  # (10b) Complete Cohort Rate Ratios
  #
  TMP <- diag(0, nrow = C)
  TMP[,crefLOC] <- 1
  CRR <- diag(1, nrow = C) - TMP 
  if (PVP$HiC==1){
    Xc <- cbind(cstar - cref, XC)
    XCR <- CRR%*%Xc
    crr <- XCR%*%B[c(3, 6, Pt[[9]])]
    crrv <- XCR%*%s2VAR[c(3, 6, Pt[[9]]),c(3, 6, Pt[[9]])]%*%t(XCR)
    sd <- matrix(sqrt(diag(crrv)))
    ci <- cbind(crr - 1.96*sd, crr + 1.96*sd)
    CohortRR <- cbind(cstar, exp(crr), exp(ci))
    dimnames(CohortRR) <- list(c(), c("Cohort", "Rate Ratio", "CILo", "CIHi"))
    # Wald test - any CohortRR values different from 1?
    INC12 <- c(1:(crefLOC-1), (crefLOC+1):C)
    X212 <- t(matrix(crr[INC12]))%*%solve(crrv[INC12, INC12],matrix(crr[INC12]))
    df12 <- C - 1
    PVAL12 <- pchisq(X212, df12, lower.tail = FALSE)
  }
  else{
    Xc <- cbind(cstar - cref, XCQ)
    XCR <- CRR%*%Xc
    crr <- XCR%*%B[c(3, 6)]
    crrv <- XCR%*%s2VAR[c(3, 6),c(3, 6)]%*%t(XCR)
    sd <- matrix(sqrt(diag(crrv)))
    ci <- cbind(crr - 1.96*sd, crr + 1.96*sd)
    CohortRR <- cbind(cstar, exp(crr), exp(ci))
    dimnames(CohortRR) <- list(c(), c("Cohort", "Rate Ratio", "CILo", "CIHi"))
    # Wald test - any CohortRR values different from 1?
    INC12 <- c(1, C)
    X212 <- t(matrix(crr[INC12]))%*%solve(crrv[INC12, INC12],matrix(crr[INC12]))
    df12 <- 2
    PVAL12 <- pchisq(X212, df12, lower.tail = FALSE) 
  }
  
  #
  # (10c) Quadratic Fitted Cohort Pattern centered on the reference age
  #
  if (A>3) {
    XCTQ <- cbind(matrix(1, C), matrix(1, C)*(aref - abar), (cstar - cbar), XCQ, matrix(1, C)%*%XA[arefLOC,])
    INCF2 <- c(1, 2, 3, 6, 4, Pt[[7]])}
  else {
    XCTQ <- cbind(matrix(1, C), matrix(1, C)*(aref - abar), (cstar - cbar), XCQ, matrix(1, C)%*%XAQ[arefLOC,])
    INCF2 <- c(1, 2, 3, 6, 4)}    
  fcp2 <- lot + XCTQ%*%B[INCF2]
  fcp2v <- XCTQ%*%s2VAR[INCF2,INCF2]%*%t(XCTQ)
  sd <- matrix(sqrt(diag(fcp2v)))
  ci <- cbind(fcp2 - 1.96*sd, fcp2 + 1.96*sd)
  QuadFittedCohortPattern <- cbind(cstar, exp(fcp2), exp(ci))
  dimnames(QuadFittedCohortPattern) <- list(c(), c("Cohort", "Rate", "CILo", "CIHi"))
  
  #
  # (10d) Quadratic Cohort Rate Ratios
  #
  Xc <- cbind(cstar - cref, XCQ)
  XCRq <- CRR%*%Xc
  crr2 <- XCRq%*%B[c(3, 6)]
  crr2v <- XCRq%*%s2VAR[c(3, 6), c(3, 6)]%*%t(XCRq)
  sd <- matrix(sqrt(diag(crr2v)))
  ci <- cbind(crr2 - 1.96*sd, crr2 + 1.96*sd)
  QuadCohortRR <- cbind(cstar, exp(crr2), exp(ci))
  dimnames(QuadCohortRR) <- list(c(), c("Cohort", "Rate Ratio", "CILo", "CIHi"))
  
  #
  # (11a) Complete Local Drifts
  #
  XCB <- as.matrix(bdiag(list(XC, 1)))
  JP <- matrix(1, P)
  SP <- matrix(1:P)
  pbars <- (P+1)/2
  DP <- (12/(Delta*(P-1)*P*(P+1)))*t(SP - pbars*JP)
  KAC <- matrix(0, nrow=A, ncol=C)
  for (ag in 1:A) {
    # starting at the first age group, you have the most recent set of P cohorts
    i0 <- 1+A-ag
    i1 <- 1+A-ag+P-1
    KAC[ag, i0:i1] <- DP
  }
  g <- XCB%*%B[c(6, Pt[[9]], 3)]
  v <- XCB%*%s2VAR[c(6, Pt[[9]], 3), c(6, Pt[[9]], 3)]%*%t(XCB)
  CM <- cbind(KAC, matrix(1, A))
  ld <- CM%*%g
  ldv <- CM%*%v%*%t(CM)
  sd <- matrix(sqrt(diag(ldv)))
  ci <- cbind(ld - 1.96*sd, ld + 1.96*sd)
  LocalDrifts <- cbind(astar, 100*(exp(ld)-1), 100*(exp(ci)-1))
  dimnames(LocalDrifts) <- list(c(),c("Age", "Mean Percent Change per Calendar Year", "CILo", "CIHi"))
  
  #
  # (11b) Quadratic Local Drifts
  #
  XCQB <- as.matrix(bdiag(list(XCQ, 1)))
  g2 <- XCQB%*%B[c(6, 3)]
  g2v <- XCQB%*%s2VAR[c(6,3), c(6,3)]%*%t(XCQB)
  ld2 <- CM%*%g2
  ld2v <- CM%*%g2v%*%t(CM)
  sd <- matrix(sqrt(diag(ld2v)))
  ci <- cbind(ld2 - 1.96*sd, ld2 + 1.96*sd)
  QuadLocalDrifts <- cbind(astar, 100*(exp(ld2)-1), 100*(exp(ci)-1))
  dimnames(QuadLocalDrifts) <- list(c(), c("Age", "Mean Percent Change per Calendar Year", "CILo", "CIHi"))
  
  #
  # (11c) Complete Deflections
  #
  CM0 <- cbind(KAC, matrix(0, A))
  def <- CM0%*%g
  defv <- CM0%*%v%*%t(CM0)
  sd <- matrix(sqrt(diag(defv)))
  ci <- cbind(def - 1.96*sd, def + 1.96*sd)
  Deflections <- cbind(astar, def, ci)
  dimnames(Deflections) <- list(c(), c("Age", "Deflection of Local Drift From Net Drift", "CILo", "CIHi"))
  # Wald Test: Do all Local Drifts equal the Net Drift? 
  if (PVP$HiC==1){
    # If A==P or P==3 the test has A - 1 df, otherwise the test has A df.
    if (A==P || P==3) {
      X213 <- t(def[1:(A-1)])%*%solve(defv[1:(A-1), 1:(A-1)], def[1:(A-1)])
      df13 <- A - 1
    }
    else {
      X213 <- t(def)%*%solve(defv, def)
      df13 <- A
    }
    PVAL13 <- pchisq(X213, df13, lower.tail = FALSE)
  }
  else{
    X213 <- X28
    df13 <- df8
    PVAL13 <- PVAL8
  }
  
  #
  # (11d) Quadratic Deflections
  #
  def2 <- CM0%*%g2
  def2v <- CM0%*%g2v%*%t(CM0)
  # If A is odd, the arithmetic mean of a coincides with the middle observed
  # value of a. The variances and covariances of def2 should be exactly equal
  # to 0 at this point, but values may compute to -eps or smaller, resulting
  # in imaginary sqrt values. The fix is to plug in the theoretical value of
  # 0. Also, the value of def2 at this point should be set to 0.
  if (A %% 2) {
    def2v[arefLOC,] <- 0
    def2v[,arefLOC] <- 0
    def2[arefLOC] <- 0
  }
  sd <- matrix(sqrt(diag(def2v)))
  ci <- cbind(def2 - 1.96*sd, def2 + 1.96*sd)
  QuadDeflections <- cbind(astar, def2, ci)
  dimnames(QuadDeflections) <- list(c(), c("Age", "Deflection of Local Drift From Net Drift", "CILo", "CIHi"))
  
  #
  # Perturbations and Gradient Shifts 
  #
  
  JA <- matrix(1, A)
  SA <- matrix(1:A)
  abars <- (A+1)/2
  DA <- (12/(Delta*(A-1)*A*(A+1)))*t(SA - abars*JA)
  KPC <- matrix(0, nrow=P, ncol=C)
  for (pk in 1:P) {
    for (ck in 1:C) {
      if ((ck >= 1) && (ck < pk)) {
        KPC[pk, ck] = 0 }
      else if ((ck >= pk) && (ck <= pk + A -1)) {
        # Values are loaded in sequence {A, A-1, ..., 1}
        KPC[pk, ck] <- -DA[pk - ck + A]  }
      else {
        KPC[pk, ck] <- 0
      }
    }
  }
  
  # 
  # (12a) Complete Perturbations
  #
  CMp <- -KPC
  pert <- CMp%*%cd
  pertv <- CMp%*%cdv%*%t(CMp)
  sd <- matrix(sqrt(diag(pertv)))
  ci <- cbind(pert - 1.96*sd, pert + 1.96*sd)
  Perturbations <- cbind(pstar, 100*(exp(pert)-1), 100*(exp(ci)-1))
  dimnames(Perturbations) <- list(c(), c("Period", "Perturbation from CAT", "CILo", "CIHi"))
  
  #
  # (12b) Quadratic Perturbations
  #
  pert2 <- CMp%*%cd2
  pert2v <- CMp%*%cd2v%*%t(CMp)
  # If P is odd, the arithmetic mean of p coincides with the middle observed
  # value of p. The variance and covariances of pert2 should be exactly equal
  # to 0 at this point, but may compute to -eps or smaller, resulting in
  # imaginary sqrt values. The fix is to plug in the theoretical value of 0.
  # Also, the value of pert2 at this point should be set to 0.
  if (P %% 2)
    pert2v[prefLOC, ] <- 0
  pert2v[, prefLOC] <- 0
  pert2[prefLOC] <- 0
  end
  sd <- matrix(sqrt(diag(pert2v)))
  ci <- cbind(pert2 - 1.96*sd, pert2 + 1.96*sd)
  QuadPerturbations <- cbind(pstar, 100*(exp(pert2)-1), 100*(exp(ci)-1))
  dimnames(QuadPerturbations) <- list(c(), c("Period", "Perturbation from CAT", "CILo", "CIHi"))
  
  #
  # (12c) Complete Gradient Shifts
  #
  XCBp <- as.matrix(bdiag(list(XC, 1, 1)))
  # cohort deviations on top of LAT on top of NetDrift
  g <- XCBp%*%B[c(6, Pt[[9]], 2, 3)]
  v <- XCBp%*%s2VAR[c(6, Pt[[9]], 2, 3), c(6, Pt[[9]], 2, 3)]%*%t(XCBp)
  CM0 <- cbind(-KPC, matrix(1, P), -matrix(1, P))
  gs <- CM0%*%g
  gsv <- CM0%*%v%*%t(CM0)
  sd <- matrix(sqrt(diag(gsv)))
  ci <- cbind(gs - 1.96*sd, gs + 1.96*sd)
  GradientShifts <- cbind(pstar, 100*(exp(gs)-1), 100*(exp(ci)-1))
  dimnames(GradientShifts) <- list(c(), c("Period", "Mean Percent Change per Year of Age", "CILo", "CIHi"))
  # Wald test: Do all Gradient Shifts equal the CAT? 
  if (PVP$HiC==1){
    dgs <- -KPC%*%cd
    dgsv <- KPC%*%cdv%*%t(KPC)
    # If A==P the test has P - 1 df, otherwise the test has P df.
    if (A == P) {
      X214 <- t(dgs[1:(P-1)])%*%solve(dgsv[1:(P-1), 1:(P-1)], dgs[1:(P-1)])
      df14 <- P - 1
    }
    else {
      X214 <- t(dgs)%*%solve(dgsv, dgs)
      df14 <- P
    }
    PVAL14 <- pchisq(X214, df14, lower.tail = FALSE)
  }
  else{
    X214 <- X28
    df14 <- df8
    PVAL14 <- PVAL8
  }
  
  #
  # (12d) Quadratic Gradient Shifts
  #
  XCQp <- as.matrix(bdiag(list(XCQ, 1, 1)))
  g2 <- XCQp%*%B[c(6, 2, 3)]
  g2v <- XCQp%*%s2VAR[c(6, 2, 3), c(6, 2, 3)]%*%t(XCQp)
  gs2 <- CM0%*%g2
  gs2v <- CM0%*%g2v%*%t(CM0)
  sd <- matrix(sqrt(diag(gs2v)))
  ci <- cbind(gs2 - 1.96*sd, gs + 1.96*sd)
  QuadGradientShifts <- cbind(pstar, 100*(exp(gs2)-1), 100*(exp(ci)-1))
  dimnames(QuadGradientShifts) <- list(c(), c("Period", "Mean Percent Change per Year of Age", "CILo", "CIHi"))
  
  WaldTests <- matrix(
    c(X21, df1, PVAL1, X22, df2, PVAL2, 
      X23, df3, PVAL3, X24, df4, PVAL4, 
      X25, df5, PVAL5, X26, df6, PVAL6,
      X27, df7, PVAL7, X28, df8, PVAL8,
      X29, df9, PVAL9, X210, df10, PVAL10,
      X211, df11, PVAL11, X212, df12, PVAL12,
      X213, df13, PVAL13, X214, df14, PVAL14), 14, 3, byrow = TRUE)
  
  dimnames(WaldTests) <- list(
    c("NetDrift = 0", 
      "THETAa = 0",
      "All Higher-Order Age Deviations = 0",
      "All Age Deviations = 0", 
      "THETAp = 0",
      "All Higher-Order Period Deviations = 0",
      "All Period Deviations = 0",
      "THETAc = 0",
      "All Higher-Order Cohort Deviations = 0",
      "All Cohort Deviations = 0", 
      "All Period RR = 1", 
      "All Cohort RR = 1", 
      "All Local Drifts = Net Drift",
      "All Gradient Shifts = CAT"),
    c("X2", "df", "P-Value"))
  
  CombinationTests <- matrix(NaN, nrow = 4)
  CombinationTests[1] <- min(c(2*PVAL5, 2*PVAL6, 1))
  CombinationTests[2] <- min(c(3*PVAL1, 3*PVAL5, 3*PVAL6, 1))
  CombinationTests[3] <- min(c(2*PVAL8, 2*PVAL9, 1))
  CombinationTests[4] <- min(c(3*PVAL1, 3*PVAL8, 3*PVAL9, 1)) 
  dimnames(CombinationTests) <- list(c("All Period Deviations = 0", 
                                       "All PRR = 1 <=> FTT = constant", 
                                       "All Cohort Deviations = 0", 
                                       "All CRR = 1 <=> FCP = constant"), c()) 
  
  
  # Matrix that converts from C-2 cohort parameters to C cohort deviations.
  # (For use outside this function).
  nc <- D$nc
  vC <- (1/(cstar[C]-cstar[1]))*( cstar[1]*matrix(nc[2:(C-1)]) - matrix(cstar[2:(C-1)]*nc[2:(C-1)]))
  v1 <- matrix(-nc[2:(C-1)]) - vC
  H <- rbind(t(v1), diag(C - 2), t(vC))
  
  Variances <- list(
    ad        = adv       ,
    ad2       = ad2v      ,
    adh       = adhv      ,
    pd        = pdv       ,
    pd2       = pd2v      ,
    pdh       = pdhv      ,
    cd        = cdv       ,
    cd2       = cd2v      ,
    cdh       = cdhv      ,
    lac       = lacv      ,
    lac2      = lac2v     ,
    lacrr     = lacrrv    ,
    lac2rr    = lac2rrv   ,
    xac       = xacv      ,
    xac2      = xac2v     ,
    xacrr     = xacrrv    ,
    xac2rr    = xac2rrv   ,
    lvcrr     = lvcrrv    ,
    lac_grad  = lac_gradv ,
    lac2_grad = lac2_gradv,
    lach_grad = lach_gradv,
    xac_grad  = xac_gradv ,
    xac2_grad = xac2_gradv,
    xach_grad = lach_gradv,
    ftt       = fttv      ,
    ftt2      = ftt2v     ,
    prr       = prrv      ,
    prr2      = prr2v     ,
    fcp       = fcpv      ,
    fcp2      = fcp2v     ,
    crr       = crrv      ,
    crr2      = crr2v     ,
    ld        = ldv       ,
    ld2       = ld2v      ,
    def       = defv      ,
    def2      = def2v     ,
    gs        = gsv       ,
    gs2       = gs2v      ,
    pert      = pertv     ,
    pert2     = pert2v    )        
  
  Matrices <- list(
    X    = D$X   ,
    XCO  = XCO ,
    XAQ  = XAQ ,
    XAD  = XAD ,
    XPQ  = XPQ ,
    XPD  = XPD ,
    XCQ  = XCQ ,
    XCD  = XCD ,
    XLA  = XLA ,
    XLAQ = XLAQ,
    XXA  = XXA ,
    XXAQ = XXAQ,
    XPT  = XPT ,
    XPTQ = XPTQ,
    XPR  = XPR ,
    XCR  = XCR ,
    XCB  = XCB ,
    CM   = CM  ,
    XCT  = XCT ,
    XCTQ = XCTQ,
    XLX  = XLX ,
    LAR  = LAR ,
    KAC  = KAC ,
    KPC  = KPC ,
    GA   = G   ,
    H    = H   )
  
  
  M <- list(
    Inputs                   = PVP                     ,
    FittedRates              = FittedRates             ,
    Coefficients             = Coefficients            ,
    NetDrift                 = NetDrift                ,
    GlobalCurvature          = GlobalCurvature         ,
    QuadAgeDeviations        = QuadAgeDeviations       ,
    HiOrdAgeDeviations       = HiOrdAgeDeviations      ,
    AgeDeviations            = AgeDeviations           ,
    QuadPerDeviations        = QuadPerDeviations       ,
    HiOrdPerDeviations       = HiOrdPerDeviations      ,
    PerDeviations            = PerDeviations           ,
    QuadCohDeviations        = QuadCohDeviations       ,
    HiOrdCohDeviations       = HiOrdCohDeviations      ,
    CohDeviations            = CohDeviations           ,
    LongAge                  = LongAge                 ,
    LongAgeRR                = LongAgeRR               ,
    QuadLongAge              = QuadLongAge             ,
    QuadLongAgeRR            = QuadLongAgeRR           ,
    CrossAge                 = CrossAge                ,
    CrossAgeRR               = CrossAgeRR              ,
    QuadCrossAge             = QuadCrossAge            ,
    QuadCrossAgeRR           = QuadCrossAgeRR          ,
    Long2CrossRR             = Long2CrossRR            ,
    QuadLongAgeGrad          = QuadLongAgeGrad         ,
    HiOrdLongAgeGrad         = HiOrdLongAgeGrad        ,
    LongAgeGrad              = LongAgeGrad             ,
    QuadCrossAgeGrad         = QuadCrossAgeGrad        ,
    HiOrdCrossAgeGrad        = HiOrdCrossAgeGrad       ,
    CrossAgeGrad             = CrossAgeGrad            ,
    QuadFittedTemporalTrends = QuadFittedTemporalTrends,
    QuadPeriodRR             = QuadPeriodRR            ,
    FittedTemporalTrends     = FittedTemporalTrends    ,
    PeriodRR                 = PeriodRR                ,
    FittedCohortPattern      = FittedCohortPattern     ,
    CohortRR                 = CohortRR                ,
    QuadFittedCohortPattern  = QuadFittedCohortPattern ,
    QuadCohortRR             = QuadCohortRR            ,
    LocalDrifts              = LocalDrifts             ,
    QuadLocalDrifts          = QuadLocalDrifts         ,
    Deflections              = Deflections             ,
    QuadDeflections          = QuadDeflections         ,
    Perturbations            = Perturbations           ,
    QuadPerturbations        = QuadPerturbations       ,
    GradientShifts           = GradientShifts          ,
    QuadGradientShifts       = QuadGradientShifts      ,
    WaldTests                = WaldTests               ,
    CombinationTests         = CombinationTests        ,
    Variances                = Variances               ,
    Matrices                 = Matrices                ,
    APCModel                 = apcM                    ,
    Pt                       = Pt                      ,
    nc                       = nc                      ) 
  
  M
}


checkPVPPAIRS <- function(R, OverDispersion = 1, offset_tick = 10^5, zero_fill = 0.1, RVals = c(NaN, NaN, NaN), HiC = TRUE, HiP = TRUE, HiA = TRUE)
{
  # Pre-process input data and parameters
  D <- rates2data_set(R)
  
  if (all(is.nan(RVals))) {
    # Calculate default reference values    
    A <- length(D$a)
    P <- length(D$p)
    aref <- floor((A+1)/2)
    pref <- floor((P+1)/2)
    cref <- pref - aref + A
    ageref <- D$a[aref]
    perref <- D$p[pref]
    cohref <- D$c[cref]
    RVals <- c(ageref, perref, cohref)
  }
  else {
    # Valdidate user-supplied reference values 
    RVals <- floor(RVals)
    
    TA <- is.element(RVals[1], D$a)
    if (!TA) {
      RVals[1] <- floor(RVals[1]) + 0.5
      TA <- is.element(RVals[1], D$a)
    }
    TB <- is.element(RVals[2], D$p)
    if (!TB) {
      RVals[2] <- floor(RVals[2]) + 0.5
      TB <- is.element(RVals[2], D$p)
    }
    TC <- is.element(RVals[3], D$c)
    if (!TC) {
      RVals[3] <- floor(RVals[3]) + 0.5
      TC <- is.element(RVals[3], D$c)
    }
    
    if (!(TA && TB && TC))
      stop("Invalid Age, Period, or Cohort reference value.")
    end
    
  }
  
  # Replace 0 events with zero_fill value.
  e <- matrix(D$DATA[,4])
  e[e==0] <- zero_fill
  D$DATA[,4] <- e

  PVP <- list(D = D, 
              RVals = RVals, 
              OverDispersion = OverDispersion, 
              offset_tick = offset_tick,
              zero_fill = zero_fill,
              HiC = HiC,
              HiP = HiP,
              HiA = HiA)
  
  PVP
}


APCFIT = function(PVP, X)
{
  # Fit APC model by weighted least squares
  
  A <- length(PVP$D$a)
  P <- length(PVP$D$p)
  n <- nrow(X)
  p <- ncol(X)
  
  p0 = ncol(X)
  
  Y <- PVP$D$DATA[,4:5];
  offset <- matrix(log(Y[,2]))
  y <- matrix(Y[,1])
  ly <- log(y)
  
  W <- y
  WX <- (W%*%matrix(1,ncol=p0))*X
  z <- (ly - offset)
  XTWX <- t(X)%*%WX
  B <- solve(t(X)%*%WX,t(WX)%*%z)
  V <- solve(t(X)%*%WX)
  u <- matrix(Y[,2]*exp(X%*%B))
  wr2 <- matrix(W*(z-X%*%B)^2)
  DEVRESIDS <- sign(y-u)*sqrt(wr2)
  DEV <- sum(wr2)
  
  if (PVP$OverDispersion==1){
    s2 <- max(1, DEV/(n-p0))
  }  else    {s2 <- 1}
  
  s2V = s2*V
  
  APCMODEL <- list(B = B, 
                   s2 = s2,
                   s2VAR = s2V, 
                   DEV = DEV, 
                   DevResids = DEVRESIDS)
  
  APCMODEL
}

designmatrix <- function(PVP)
{
  # Calculate design matrix for APC model
  N <- nrow(PVP$D$DATA)
  J <- matrix(1, nrow = N)
  I.N <- diag(N)
  
  astar <- matrix(PVP$D$DATA[,1])
  astarbar <- mean(astar)
  astar_0 <- astar - astarbar
  A <- length(PVP$D$a)
  
  
  pstar <- matrix(PVP$D$DATA[,2])
  pstarbar <- mean(pstar)
  pstar_0 <- pstar - pstarbar
  P <- length(PVP$D$p)
  
  cstar <- matrix(PVP$D$DATA[,3])
  cstarbar <- pstarbar - astarbar
  cstar_0 <- cstar - cstarbar
  C <- length(PVP$D$c)
  
  # Age 
  Ad <- kronecker(matrix(1, nrow=P), diag(A))
  
  # Per
  Pd <- kronecker(diag(P), matrix(1, nrow=A))
  
  # Coh
  Cd <- matrix(NaN,nrow=N,ncol=C)
  for (i in 1:C)
    Cd[,i] <- cstar == PVP$D$c[i]
  end
  nc <- t( diag( t(Cd)%*%Cd ) )
  
  
  # Projection matrix orthogonal to intercept, linear age, and quadratic age
  Delta <- astar[2] - astar[1]
  a00 <- astar[1] - 0.5*Delta
  if (A>=3) {
    
    qa2 <- astar^2 - (Delta*A + 2*a00)*astar + (a00 + Delta*A/2)^2 - ((Delta^2)/12)*(A-1)*(A+1)
    Xa2 <- cbind(J, astar_0, qa2)
    xtxi <- (1/P)*diag(   1/ c(A, (Delta^2/12)*(A-1)*A*(A+1), (Delta^4/180)*(A-2)*(A-1)*A*(A+1)*(A+2) )   )
    Ra2 <- xtxi%*%t(Xa2)
    PA12 <- Xa2%*%Ra2
    XAD12 <- I.N - PA12
    
    
  }
  else {
    
    qa2 <- matrix(0, N)
    Xa2 <- cbind(J, astar_0)
    xtxi <- (1/P)*diag(1/ c(A, (Delta^2/12)*(A-1)*A*(A+1))  )
    Ra2 <- xtxi%*%t(Xa2)
    PA12 <- Xa2%*%Ra2
    XAD12 <- I.N - PA12
    
  }
  
  # Projection matrix orthogonal to intercept, linear period, and quadratic period
  p00 <- pstar[1] - 0.5*Delta
  if (P >= 3) {
    
    qp2 <- pstar^2 - (Delta*P + 2*p00)*pstar + (p00 + Delta*P/2)^2 - ((Delta^2)/12)*(P-1)*(P+1)
    Xp2 <- cbind(J, pstar_0, qp2)
    xtxi <- (1/A)*diag(   1/ c(P, (Delta^2/12)*(P-1)*P*(P+1), (Delta^4/180)*(P-2)*(P-1)*P*(P+1)*(P+2) )   )
    Rp2 <- xtxi%*%t(Xp2)
    PP12 <- Xp2%*%Rp2
    XPD12 <- I.N - PP12
    
  }
  else {
    
    qp2 <- pstar^2 - (Delta*P + 2*p00)*pstar + (p00 + Delta*P/2)^2 - ((Delta^2)/12)*(P-1)*(P+1)
    Xp2 <- cbind(J, pstar_0)
    xtxi <- (1/A)*diag(   1/ c(P, (Delta^2/12)*(P-1)*P*(P+1) )   )
    Rp2 <- xtxi%*%t(Xp2)
    PP12 <- Xp2%*%Rp2
    XPD12 <- I.N - PP12
    
  }
  
  c00 <- p00 - a00
  qc2 <- cstar^2 - (Delta*(A+P) + 2*(c00 - Delta*A))*cstar + (c00 - Delta*A)^2 + (A+P)*Delta*cstarbar - ((Delta^2)/6)*(2*A^2 + 3*A*P + 2*P^2 - 1)
  Xc2 <- cbind(J, cstar_0, qc2)
  xtxi <- (1/(A*P))*diag( 1/ c(1, (Delta^2/12)*(A^2 + P^2 - 2), (Delta^4/180)*( (A^2 + P^2)^2 - 10*(A^2 + P^2) + 3*A^2*P^2 + 13 )  ) )
  Rc2 <- xtxi%*%t(Xc2)
  PC12 <- Xc2%*%Rc2
  XCD12 <- I.N - PC12
  
  # Orthogonalize the deviations
  Ad0 <- XAD12%*%Ad
  Pd0 <- XPD12%*%Pd
  Cd0 <- XCD12%*%Cd
  
  if (A>3) {
    aCOL <- 2:(A-2)
  }
  else {
    # Ad0 column space is empty
    aCOL <- NULL
  }
  if (P>3){
    pCOL <- 2:(P-2)
  }
  else {
    # Pd0 column space is empty
    pCOL <- NULL
  }
  
  X <- cbind(J, astar_0, cstar_0, qa2, qp2, qc2, Ad0[,aCOL], Pd0[,pCOL], Cd0[,2:(C-2)])
  
  # Pointers for Higher-Order Deviations:
  # 
  # * For age:
  #     1:(A - 3) shift forward by 6 -> (1 + 6):(A - 3 + 6) = 7:(A + 3)
  # * For period:
  #     1:(P - 3) shift forward by 6 + (A - 3) -> 
  #       (1 + 6 + A - 3):(P - 3 + 6 + A - 3) = (A + 4):(A + P)
  # * For cohort:
  #     1:(C - 3) shift forward by 6 + (A - 3) + (P - 3) ->
  #       (1 + 6 + (A - 3) + (P - 3)):(C - 3) + 6 + (A - 3) + (P - 3) =
  #       (A + P + 1):(A + P + C - 3)
  
  Pt <- vector("list", 9)
  Pt[[1]] <- 1
  Pt[[2]] <- 2
  Pt[[3]] <- 3
  Pt[[4]] <- 4
  Pt[[5]] <- 5
  Pt[[6]] <- 6
  Pt[[7]] <- 7:(A+3)
  Pt[[8]] <- (A+4):(A+P)
  Pt[[9]] <- (A+P+1):(A+P+C-3)
  
  if (A==3) {
    # 7:(A+3) runs backwards but (A+4) = 7
    # need to reset Pt[[7]] to NaN but Pt{8} & Pt{9} are correct
    Pt[[7]] <- NaN
  }
  
  if (P==3) {
    Pt[[8]] <- NaN
  }
  
  # Pointer for cohort effects, (last f/u, South+East edges of Lexis)
  INCc <- c(A*(1:P), P*A-(1:(A-1)))
  # Pointer for age effects, (South edge of Lexis)
  INCa <- ((P-1)*A+1):(P*A)
  # Pointer for period effects (East edge of Lexis)
  INCp <-A*(1:P)
  
  D <- list(X = X, INCa = INCa, INCp = INCp, INCc = INCc, Pt = Pt, nc = nc)
  D
}



rates <- function(EVENTS, OFFSET, 
                  fullname = character(0), 
                  label = character(0), 
                  description = character(0), 
                  event_label = "events", 
                  offset_units = "offset units", 
                  offset_tick = 100000, 
                  api = c(NaN, NaN, NaN),
                  ages = NaN,
                  periods = NaN)
  
{
  ###
  # Validate EVENTS and OFFSET
  ###
  if (is.matrix(EVENTS) && all(EVENTS >= 0))
  {
    A <- nrow(EVENTS)
    P <- ncol(EVENTS)
  }
  else
  {
    stop("events must be a matrix of non-negative values.")
  }
  
  if (!(is.matrix(OFFSET) && all(OFFSET >= 0) && nrow(OFFSET)==A && ncol(OFFSET)==P))
  {
    stop("offset must be a matrix of non-negative values the same size as events.")
  }
  
  ###
  # Validate ages and periods specified by api or ages/periods combination.
  ###
  if (!all(is.nan(api)))
  {
    if ( !( length(api==3) && is.numeric(api) ) )
    {stop("api must be a 3 element vector.")}
    if (!all(api==round(api)))
    {stop("api values must be single-years.")}
    
    a0 <- api[1]
    p0 <- api[2]
    INTERVAL <- api[3]
    ages <- a0 + INTERVAL*(0:A)
    periods <- p0 + INTERVAL*(0:P)
    a <- ages[1:A-1] + 0.5*INTERVAL
    p <- periods[1:P-1] + 0.5*INTERVAL
  }
  else
  {
  if (! ( !any(is.nan(ages)) && !any(is.nan(periods)) && 
          all(ages==round(ages)) && all(periods==round(periods)) && 
          (length(ages)==A+1) && (length(periods)==P+1) ) )  
    {stop("Invalid cutpoints for ages or periods.")}  
  }
  

  ###
  # Validate text inputs.
  ###
  
  if (!is.character(fullname))
  {stop('fullname must be a character string.')}
  else
  {fullname <- gsub("^\\s+|\\s+$", '', fullname)}
  
  if (length(fullname)==0)
  {fullname <- paste(c(toString(A), '-by-', toString(P), ' rates object'), collapse = "")}
  
  if (length(description)==0)
  {description <- paste(c('Created ', date()), collapse = "")}
 
  if (!is.character(event_label))
  {stop('event_label must be a character string.')}
  
  if (!is.character(offset_units))
  {stop('offset_units must be a character string.')}
  
  if (length(label)==0)
 {label <- fullname}
 
  #aL <- as.character(seq(from = ages[1], to = ages[A], by = 2))
  #aH <- as.character(seq(from = ages[2], to = ages[A+1], by = 2))
  da <- ages[2:(A+1)] - ages[1:A]
  if (!all(da==1))
  { aL <- as.character(seq(from = ages[1], to = ages[A], by = 2))
  aH <- as.character(seq(from = ages[2], to = ages[A+1], by = 2)) 
  age_labels <- paste(cbind(aL), ' - ', cbind(aH))}
  else
  {aL <- as.character(seq(from = ages[1], to = ages[A], by = 1))
    age_labels <- aL}
  
  #pL <- as.character(seq(from = periods[1], to = periods[P], by = 2))
  #pH <- as.character(seq(from = periods[2], to = periods[P+1], by = 2))
  dp <- periods[2:(P+1)] - periods[1:P]
  if (!all(dp==1))
  { pL <- as.character(seq(from = periods[1], to = periods[P], by = 2))
  pH <- as.character(seq(from = periods[2], to = periods[P+1], by = 2))
  per_labels <- paste(cbind(pL), ' - ', cbind(pH))}
  else
  {  pL <- as.character(seq(from = periods[1], to = periods[P], by = 1))

    per_labels <- pL}

  a <- ages[1:A] + da/2
  p <- periods[1:P] + dp/2
  
  R <- list(fullname = fullname,
            label = label,
            description = description,
            events = EVENTS,
            event_label = event_label,
            offset = OFFSET,
            offset_units = offset_units,
            offset_tick = offset_tick,
            ages = ages,
            age_labels = age_labels,
            a = a,
            periods = periods,
            per_labels = per_labels,
            p = p)
  
  R
}


rates2data_set <- function(R) {
  
  
  A <- nrow(R$events)
  P <- ncol(R$events)
  
  da <- R$ages[2:(A+1)] - R$ages[1:A]
  D.a <- R$ages[1:A] + 0.5*da
  
  dp <- R$periods[2:(P+1)] - R$periods[1:P]
  D.p <- R$periods[1:P] + 0.5*dp
  
  
  
  ADATA <- kronecker(matrix(1, nrow=P),  matrix(D.a, nrow=A))
  PDATA <- kronecker(matrix(D.p, nrow=P), matrix(1, nrow=A))
  CDATA <- PDATA - ADATA
  D.c <- sort(c(unique(CDATA)))
  E <- c(R$events)
  O <- c(R$offset)
  D.DATA = cbind(ADATA, PDATA, CDATA, E, O)
  colnames(D.DATA)<-c("Age","Period","Cohort","Events","Offset")
  
  
  D <- list(name = R$name,
            description = R$description,
            DATA = D.DATA, 
            a = D.a,
            p = D.p,
            c = D.c)
  D
  
}

rates2csv <- function(R, FILE = "rates.csv")
  
{
  A <- nrow(R$events)
  P <- ncol(R$events)
  comma = ','
  commas <- paste(replicate(P-1, ','), collapse="")
  
  # fprintf(fid, ['Title: ' R.fullname commas '\n']);
  
  s1 <- paste(c('Title: ', R$fullname, commas), collapse = "")
  s2 <- paste(c('Description: ', R$description, commas), collapse = "")
  s3 <- paste(c('Start Year: ', toString(R$p[1]), commas), collapse = "")
  s4 <- paste(c('Start Age: ', toString(R$a[1]), commas), collapse = "")
  s5 <- paste(c('Interval (Years): ', toString(R$a[2]-R$a[1]), commas), collapse = "")
 
  DATA <- matrix(NaN, A, 2*P)
  DATA[,seq(1, 2*P-1, by = 2)] <- R$events
  DATA[,seq(2, 2*P, by = 2)] <- R$offset
  
  cat(s1, s2, s3, s4, s5, file = FILE, sep = "\n", append = FALSE)
  write(t(DATA), file = FILE, append = TRUE, sep = ',', ncolumns = 2*P)
  
  Fout <- list(s1 = s1, s2 = s2, s3 = s3, s3 = s4, s5 = s5, DATA = DATA)
  Fout
  
}

simple_csv2rates <- function(FILE,StartYear,StartAge,Interval,fullname,description) 
{
  # DATA is a data.frame
  DATA <- read.table(FILE, header = FALSE, sep = ',')
  PP <- ncol(DATA)
  A <- nrow(DATA)
  E = as.matrix(DATA[, seq(1, PP-1, by = 2)])
  dimnames(E) <- NULL
  O = as.matrix(DATA[, seq(2, PP, by = 2)])
  dimnames(O) <- NULL

  a <- seq(from = StartAge, by = Interval, to = StartAge + Interval*A)
  p <- seq(from = StartYear, by = Interval, to = StartYear + Interval*PP/2)
 
  R <- rates(E, O,
             fullname = fullname,
             description = description,
             ages = a,
             periods = p)
  R
}

prepare_rates <- function(DATA,StartYear,StartAge,Interval,fullname,description) 
{
  # DATA is a data.frame
  PP <- ncol(DATA)
  A <- nrow(DATA)
  E = as.matrix(DATA[, seq(1, PP-1, by = 2)])
  dimnames(E) <- NULL
  O = as.matrix(DATA[, seq(2, PP, by = 2)])
  dimnames(O) <- NULL
  
  a <- seq(from = StartAge, by = Interval, to = StartAge + Interval*A)
  p <- seq(from = StartYear, by = Interval, to = StartYear + Interval*PP/2)
  
  R <- rates(E, O,
             fullname = fullname,
             description = description,
             ages = a,
             periods = p)
  R
}


type <- function(R, comp = 'r')
{
  A <- nrow(R$events)
  P <- ncol(R$events)
  
  if (comp == "r") {
    Tout <- R$offset_tick*R$events/R$offset
    dn <- paste('Rates - ', R$fullname)
    
  } else if (comp == "e") {
    Tout <- R$events
    dn <- paste("Events -", R$fullname)
    
  } else if (comp == "o") {
    Tout <- R$offset
    dn <- paste('Offset - ', R$fullname)
    
  } else if (comp == "eo") {
    
    DATA <- matrix(NaN, nrow = A, ncol = 2*P)
    DATA[,seq.int(1, 2*P, 2)]<-R$events
    DATA[,seq.int(2, 2*P, 2)]<-R$offset
    Tout <- DATA
    dn <- paste("Events & Offset -", R$fullname)
    
  } else if (comp == "er") {
    r <- R$offset_tick*R$events/R$offset
    DATA <- matrix(NaN, nrow = A, ncol = 2*P)
    DATA[,seq.int(1, 2*P, 2)]<-R$events
    DATA[,seq.int(2, 2*P, 2)]<-r
    Tout <- DATA
    dn <- paste("Events and Rates - ", R$fullname)
    
  } else if (comp == "eor") {
    
    r <- R$offset_tick*R$events/R$offset
    DATA <- matrix(NaN, nrow = A, ncol = 3*P)
    DATA[,seq.int(1, 3*P, 3)]<-R$events
    DATA[,seq.int(2, 3*P, 3)]<-R$offset
    DATA[,seq.int(3, 3*P, 3)]<-r
    Tout <- DATA
    dn <- paste("Events, offset, and Rates - ", R$fullname)
    
  } else if (comp == "rci") {
    
    r <- R$offset_tick*R$events/R$offset
    v <- (R$offset_tick^2)*R$events/R$offset^2
    cilo <- r - 1.96*sqrt(v)
    cilo[cilo<0] <- 0
    cihi <- r + 1.96*sqrt(v)
    DATA <- matrix(NaN, nrow = A, ncol = 3*P)
    DATA[,seq.int(1, 3*P, 3)]<-r
    DATA[,seq.int(2, 3*P, 3)]<-cilo
    DATA[,seq.int(3, 3*P, 3)]<-cihi
    Tout <- DATA
    dn <- paste("Rates and 95% CI - ", R$fullname)
    
  } else {
    
  }
  
  Tout <- list(name = dn, DATA = Tout, ages = R$ages, periods = R$periods) 
  Tout
  
}


csv2rates <- function(FILE) 
{
  
  StartYear <- 0
  StartAge <- 0
  Interval <- 1

  
  header <- scan(FILE, nlines = 5, what = character(0), sep = '/', quiet = 1)
  # Strip any excess delimeters
  header <- gsub(",", "", header)
  # Strip leading and trailing white space
  header <- gsub("^\\s+|\\s+$", "", header)
  H = length(header)
  
  k <- 0
  for (h in 1:H) {
  headerh = header[h]
   nc = nchar(headerh)
   f <- regexpr("Title:", headerh, ignore.case = TRUE)
   d <- regexpr("Description:", headerh, ignore.case = TRUE)
   p <- regexpr("Start Year:", headerh, ignore.case = TRUE)
   a <- regexpr("Start Age:", headerh, ignore.case = TRUE)
   i <- regexpr("Interval \\(Years\\):", headerh, ignore.case = TRUE)
   if (f==1)
   {
    fullname <- gsub("^\\s+|\\s+$", "", substr(headerh, attr(f, "match.length")+1, nc))
    k <- k + 1
   }
  if (d==1)
  {
    description <- gsub("^\\s+|\\s+$", "", substr(headerh, attr(d, "match.length")+1,nc))
    k <- k + 1
  }
   if (p==1)
   {
     StartYear <- as.numeric(substr(headerh, attr(p, "match.length")+1,nc))
     k <- k + 1
   }
   if (a==1)
   {
     StartAge <- as.numeric(substr(headerh, attr(a, "match.length")+1,nc))
     k <- k + 1
   }
   
   if (i==1)
   {
     Interval <- as.numeric(substr(headerh, attr(i, "match.length")+1,nc))
     k <- k + 1
   }
  }
  # DATA is a data.frame
  DATA <- read.table(FILE, skip = k, header = FALSE, sep = ',')
  PP <- ncol(DATA)
  A <- nrow(DATA)
  E = as.matrix(DATA[, seq(1, PP-1, by = 2)])
  dimnames(E) <- NULL
  O = as.matrix(DATA[, seq(2, PP, by = 2)])
  dimnames(O) <- NULL

  a <- seq(from = StartAge, by = Interval, to = StartAge + Interval*A)
  p <- seq(from = StartYear, by = Interval, to = StartYear + Interval*PP/2)
 
  R <- rates(E, O,
             fullname = fullname,
             description = description,
             ages = a,
             periods = p)
  R
}


plot.apc1 <- function(M)
{
  # Plot first set of age-period-cohort estimable functions.
  
  par(mfrow = c(4,3))
  
  
  DATA <- cbind(matrix(M$LongAge[,1]), (M$LongAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Longitudinal Age Curve", cex.main = 1)

  DATA <- cbind(matrix(M$CrossAge[,1]), (M$CrossAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  title(main = "Cross-Sectional Age Curve", cex.main = 1)
  
  pcurve(M$Long2CrossRR, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  abline(1,0, lty = 3)
  title(main = "Long vs. Cross RR", cex.main = 1)
  
  pcurve(M$FittedTemporalTrends, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Fitted Temporal Trends", cex.main = 1)
  
  pcurve(M$PeriodRR, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Period RR", cex.main = 1)
  
  pcurve(M$CohortRR, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Cohort RR", cex.main = 1)
  
  pcurve(M$LocalDrifts, col = "black", colf = "grey88", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Local Drifts", cex.main = 1)
  
  pcurve(M$AgeDeviations, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Age Deviations", cex.main = 1)
  
  pcurve(M$PerDeviations, col = "steelblue4", colf = "slategray1", lwd = 3 , cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Period Deviations", cex.main = 1)
  
  pcurve(M$CohDeviations, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Cohort Deviations", cex.main = 1)
  
  pcurve(M$FittedCohortPattern, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Fitted Cohort Pattern", cex.main = 1)
  
}

plot.apc2 <- function(M)
{
  # Plot second set of age-period-cohort estimable functions.
  
  par(mfrow = c(4,3))
  
  
  DATA <- cbind(matrix(M$QuadLongAge[,1]), (M$QuadLongAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Longitudinal Age Curve", cex.main = 1)

  DATA <- cbind(matrix(M$QuadCrossAge[,1]), (M$QuadCrossAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  title(main = "Cross-Sectional Age Curve", cex.main = 1)
  
  pcurve(M$Long2CrossRR, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  abline(1,0, lty = 3)
  title(main = "Long vs. Cross RR", cex.main = 1)
  
  pcurve(M$QuadFittedTemporalTrends, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Fitted Temporal Trends", cex.main = 1)
  
  pcurve(M$QuadPeriodRR, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Period RR", cex.main = 1)
  
  pcurve(M$QuadCohortRR, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Cohort RR", cex.main = 1)
  
  pcurve(M$QuadLocalDrifts, col = "black", colf = "grey88", lwd = 3, cex = 1.0, pch = 21)
  abline(M$NetDrift[1,1], 0, lty = 3)
  title(main = "Local Drifts", cex.main = 1)
  
  pcurve(M$QuadAgeDeviations, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Age Deviations", cex.main = 1)
  
  pcurve(M$QuadPerDeviations, col = "steelblue4", colf = "slategray1", lwd = 3 , cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Period Deviations", cex.main = 1)
  
  pcurve(M$QuadCohDeviations, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Cohort Deviations", cex.main = 1)
  
  pcurve(M$QuadFittedCohortPattern, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Fitted Cohort Pattern", cex.main = 1)
  
  DATA <- cbind(matrix(M$QuadGradientShifts[,1]), (M$QuadGradientShifts[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Period", "Mean % Change Per Year of Age", "CILo", "CIHi"))
  pcurve(DATA, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Gradient Shifts", cex.main = 1)
  abline(100*(exp(M$Coefficients[4,1]) - 1), 0, lty = 3)
  
 
}

pcurve <- function(DATA, col = 'steelblue4', colf = 'slategray1', bg = 'grey99', pch = 1, type = 'b', lty = 1, lwd = 2, XLim = NA, YLim = NA, cex = 1.5)
{
 # Plot a selected output from an age-period-cohort model. 
  
  x <- DATA[,1]
  rangex <- range(x)[2]-range(x)[1]
  if (is.na(XLim[1])) {XLim <- c(min(x)-0.05*rangex, max(x)+0.05*rangex)}
  xl <- dimnames(DATA)[[2]][1] 

  y <- DATA[,2]
  rangey <- range(y)[2]-range(y)[1]
  if (is.na(YLim[1])) {YLim <- c(min(y)-0.05*rangey, max(y)+0.05*rangey)}
  yl <- dimnames(DATA)[[2]][2] 
  

  if (ncol(DATA)==4){
    xci <- c(x, rev(x))
    yci <- c(DATA[, 3], rev(DATA[, 4]))
    rangey <- range(yci)[2]-range(yci)[1]
    YLim <- c(min(yci) - 0.05*rangey, max(yci) + 0.05*rangey)}
  else {yci <- NULL}
  
  
  plot(x, y, col = col, pch = pch, type = type, lty = lty, lwd = lwd, cex = cex, 
       xlab = xl, ylab = yl, xlim = XLim, ylim = YLim, las = 1, bg = bg)   
  
  if (!is.null(yci[1])){
    polygon(xci, yci, col = colf, border = colf)
  }
  
  points(x, y, col = col, pch = pch, type = type, lty = lty, lwd = lwd, cex = cex, 
         xlab = xl, ylab = yl, xlim = XLim, ylim = YLim, las = 1, bg = bg)   
  
}
