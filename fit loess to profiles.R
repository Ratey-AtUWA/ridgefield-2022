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

depfs <- seq(5,100,10)
profs <- as.character(unique(rf18Res$Profile))

par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsRs <- list()
for (i in 1:length(profs)){
  if(NROW(subset(rf18Res, subset=rf18Res$Profile==profs[i]))>=2){
  with(rf18Res, plot(Depth.mean ~ C.pct, xlim=c(0,18), ylim=c(100,0),
       subset = rf18Res$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
  mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
  lss0 <- loess(C.pct ~ Depth.mean, data = rf18Res,
                subset = rf18Res$Profile==profs[i], span=1)
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
all_loess_Res <- cbind(depfs,all_loess_Res)
all_loess_Res$smoothMean <- apply(all_loess_Res, MARGIN = 1,
                                  function(x){mean(x,na.rm=T)})
with(all_loess_Res,plot(smoothMean, depfs, xlim=c(0,18), ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._
#
depfs <- seq(5,100,10)
profs <- as.character(unique(rf18Ero$Profile))

par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsEro <- list()
for (i in 1:length(profs)){
  if(NROW(subset(rf18Ero, subset=rf18Ero$Profile==profs[i]))>=2){
    with(rf18Ero, plot(Depth.mean ~ C.pct, xlim=c(0,18), ylim=c(100,0),
                       subset = rf18Ero$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
    mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
    lss0 <- loess(C.pct ~ Depth.mean, data = rf18Ero,
                  subset = rf18Ero$Profile==profs[i], span=1)
    smth0 <- predict(lss0,depfs)
    lst0 <- c(`names<-`(list(smth0), paste0("smooth",profs[i])))
    loessModsEro <- list.append(loessModsEro,lst0)
    lines(depfs ~ smth0)
  }
}
all_loess_Ero <- as.data.frame(matrix(unlist(loessModsEro),
                                      ncol=length(loessModsEro)))
colnames(all_loess_Ero) <- unique(str_trunc(names(unlist(loessModsEro)),
                                            width=9, side="right", ""))
all_loess_Ero <- cbind(depfs,all_loess_Ero)
all_loess_Ero$smoothMean <- apply(all_loess_Ero, MARGIN = 1,
                                  function(x){mean(x,na.rm=T)})
with(all_loess_Ero,plot(smoothMean, depfs, xlim=c(0,18), ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._
#
depfs <- seq(5,100,10)
profs <- as.character(unique(rf18Dep$Profile))

par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsDep <- list()
for (i in 1:length(profs)){
  if(NROW(subset(rf18Dep, subset=rf18Dep$Profile==profs[i]))>=2){
    with(rf18Dep, plot(Depth.mean ~ C.pct, xlim=c(0,18), ylim=c(100,0),
                       subset = rf18Dep$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
    mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
    lss0 <- loess(C.pct ~ Depth.mean, data = rf18Dep,
                  subset = rf18Dep$Profile==profs[i], span=1)
    smth0 <- predict(lss0,depfs)
    lst0 <- c(`names<-`(list(smth0), paste0("smooth",profs[i])))
    loessModsDep <- list.append(loessModsDep,lst0)
    lines(depfs ~ smth0)
  }
}
all_loess_Dep <- as.data.frame(matrix(unlist(loessModsDep),
                                      ncol=length(loessModsDep)))
colnames(all_loess_Dep) <- unique(str_trunc(names(unlist(loessModsDep)),
                                            width=9, side="right", ""))
all_loess_Dep <- cbind(depfs,all_loess_Dep)
all_loess_Dep$smoothMean <- apply(all_loess_Dep, MARGIN = 1,
                                  function(x){mean(x,na.rm=T)})
with(all_loess_Dep,plot(smoothMean, depfs, xlim=c(0,18), ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")

loessModsMeansLF3 <- data.frame(Depth=depfs,
                                Depositional=all_loess_Dep$smoothMean,
                                Erosional=all_loess_Ero$smoothMean,
                                Residual=all_loess_Res$smoothMean)
par(mfrow=c(1,1), mar = c(1,4,4,1), font.lab=2)
with(loessModsMeansLF3, plot(Depth ~ Depositional, type="b",
                             xlim=c(0,10), ylim=c(100,0),
                             xlab="",xaxt="n",
                             ylab="Depth (cm)", cex.lab=1.4))
axis(3);mtext("Interpolated mean SOC (%)",3,2,font=2,cex=1.4)
with(loessModsMeansLF3, lines(Depth ~ Erosional, type="b", col="blue",pch=2))
with(loessModsMeansLF3, lines(Depth ~ Residual, type="b", col="red3",pch=19))
legend("bottom", bty = "n", inset = 0.02, y.intersp = 1.2, cex = 1.4,
       legend=names(loessModsMeansLF3[,-1]), col=c(1,"blue","red3"),
       pch=c(1,2,19), title="Landform category")
