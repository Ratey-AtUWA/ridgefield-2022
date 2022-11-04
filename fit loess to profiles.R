# THIS IS VERY
# PRELIMINARY!

levels(rf2018$LF5) # "LS" "MS" "RE" "US" "VF"
levels(rf2018$LF3) # "Depos" "Eros"  "Resid"
rf18Dep <- droplevels(subset(rf2018, rf2018$LF3=="Depos"))
rf18Ero <- droplevels(subset(rf2018, rf2018$LF3=="Eros"))
rf18Res <- droplevels(subset(rf2018, rf2018$LF3=="Resid"))

depfs <- seq(0,100,5)
profs <- unique(rf18Res$Profile)
profs <- profs[-13]
par(mfrow=c(6,3), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
for (i in 1:length(profs)){
  with(rf18Res, plot(C.pct ~ Depth.mean, ylim=c(0,18),
       subset = rf18Res$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
  mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
  assign(paste0("loess",profs[i]), loess(C.pct ~ Depth.mean, data = rf18Res,
                                         subset = rf18Res$Profile==profs[i]))
  lss0 <- loess(C.pct ~ Depth.mean, data = rf18Res,
                subset = rf18Res$Profile==profs[i], span=1)
  assign(paste0("smooth",profs[i]), predict(lss0, depfs))
  smth0 <- predict(lss0,depfs)
  lines(depfs, smth0)
}

i <- 1
with(rf18Res, plot(C.pct~Depth.mean, subset=rf18Res$Profile==profs[i], pch=19))
i <- i+1
