# THIS IS VERY
# PRELIMINARY!

library(rlist)

levels(rf2018$LF5) # "LS" "MS" "RE" "US" "VF"
levels(rf2018$LF3) # "Depos" "Eros"  "Resid"

# make all profile names strings of length 3
rf2018$Profile <- as.character(rf2018$Profile)
l1 <- which(str_length(rf2018$Profile)==1)
rf2018$Profile[l1] <- paste0("0",rf2018$Profile[l1])
l2 <- which(str_length(rf2018$Profile)==2)
rf2018$Profile[l2] <- paste0("0",rf2018$Profile[l2])

rf18Dep <- droplevels(subset(rf2018, rf2018$LF3=="Depos"))
rf18Ero <- droplevels(subset(rf2018, rf2018$LF3=="Eros"))
rf18Res <- droplevels(subset(rf2018, rf2018$LF3=="Resid"))

depfs <- seq(0,100,5)
profs <- as.character(unique(rf18Res$Profile))

par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsRs <- list()
for (i in 1:length(profs)){
  if(NROW(subset(rf18Res, subset=rf18Res$Profile==profs[i]))>=2){
  with(rf18Res, plot(Depth.mean ~ C.pct, xlim=c(0,18), ylim=c(100,0),
       subset = rf18Res$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
  mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
  assign(paste0("loess",profs[i]), loess(C.pct ~ Depth.mean, data = rf18Res,
                                         subset = rf18Res$Profile==profs[i]))
  lss0 <- loess(C.pct ~ Depth.mean, data = rf18Res,
                subset = rf18Res$Profile==profs[i], span=1)
  assign(paste0("smooth",profs[i]), predict(lss0, depfs))
  smth0 <- predict(lss0,depfs)
  lst0 <- c(`names<-`(list(smth0), paste0("smooth",profs[i])))
  loessModsRs <- list.append(loessModsRs,lst0)
  lines(depfs ~ smth0)
  }
}
all_loess_Res <- as.data.frame(matrix(unlist(loessModsRs),
                                      ncol=length(loessModsRs)))
colnames(all_loess_Res) <- unique(str_trunc(names(unlist(loessModsRs)),
                                            width=9, side="right", ""))
all_loess_Res$smoothMean <- apply(all_loess_Res, MARGIN = 1,
                                  function(x){mean(x,na.rm=T)})
with(all_loess_Res,plot(smoothMean, depfs, xlim=c(0,18), ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")

