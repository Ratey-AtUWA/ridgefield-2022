# THIS IS VERY
# PRELIMINARY!
# but getting more reliable

#### set-up ####

library(rlist)

levels(rf2018$LF5) # "LS" "MS" "RE" "US" "VF"
levels(rf2018$LF3) # "Depos" "Eros"  "Resid"

# make all profile names strings of length 3
rf2018$Profile <- as.character(rf2018$Profile)
l1 <- which(str_length(rf2018$Profile)==1)
rf2018$Profile[l1] <- paste0("0",rf2018$Profile[l1])
l2 <- which(str_length(rf2018$Profile)==2)
rf2018$Profile[l2] <- paste0("0",rf2018$Profile[l2])

# make subsets of data for each landform category
rf18Dep <- droplevels(subset(rf2018, rf2018$LF3=="Depos"))
rf18Ero <- droplevels(subset(rf2018, rf2018$LF3=="Eros"))
rf18Res <- droplevels(subset(rf2018, rf2018$LF3=="Resid"))
# this finishes set-up

# _._._._._._._._._._._._._._._Residual._._._._._._._._._._._._._._._._._._####

depfs <- seq(5,100,10) # consistent depth intervals for mean calculation
profs <- as.character(unique(rf18Res$Profile)) # list of profiles in landform
# initialize some parameters
# search-and-replace to change variable (sorry)
xrng <- range(rf2018$pH.CaCl2, na.rm = TRUE)
par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsRes <- list() # empty list for this landform, to be appended
j <- 0 # counter for un-interpolatable profiles
# loop through all the profiles in this landform...
for (i in 1:length(profs)){
  # .. so long as they have 2 or more depths...
  check1 <- NROW(subset(rf18Res, rf18Res$Profile==profs[i]))
  # ...and so long as they don't have too many missing values for variable
  check2 <- NROW(is.na(subset(rf18Res$pH.CaCl2, rf18Res$Profile==profs[i]))) -
    sum(is.na(subset(rf18Res$pH.CaCl2, rf18Res$Profile==profs[i])))
  if(check1 < 2 | check2 <= 1){
    j <- j+1
  } else {
    # plots are to check points & model
    with(rf18Res, plot(Depth.mean ~ pH.CaCl2, xlim=xrng, ylim=c(100,0),
       subset = rf18Res$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
  mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
  # make loess model for profile-variable combo
  lss0 <- loess(pH.CaCl2 ~ Depth.mean, data = rf18Res,
                subset = rf18Res$Profile==profs[i], span=1)
  # make vector of loess predictions for profile-variable combo
  smth0 <- predict(lss0,depfs)
  # make loess model vector into a list with a name...
  lst0 <- c(`names<-`(list(smth0), paste0("smooth",profs[i])))
  # and add this to the overall list for this landform
  loessModsRes <- list.append(loessModsRes,lst0)
  # check model by plotting
  lines(depfs ~ smth0)
  }
}
cat("number of non-interpolated profiles: ",j," out of ",length(profs),"\n")
all_loess_Res <- as.data.frame(matrix(unlist(loessModsRes),
                                      ncol=length(loessModsRes)))
colnames(all_loess_Res) <- unique(str_trunc(names(unlist(loessModsRes)),
                                            width=9, side="right", ""))
all_loess_Res$smoothMean <- rowMeans(all_loess_Res, na.rm=T)
all_loess_Res <- cbind(depfs,all_loess_Res)
with(all_loess_Res,plot(smoothMean, depfs, xlim=xrng, ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")

# _._._._._._._._._._._._._._._._Erosional_._._._._._._._._._._._._._._._._####

depfs <- seq(5,100,10)
profs <- as.character(unique(rf18Ero$Profile))
xrng <- range(rf2018$pH.CaCl2, na.rm = TRUE)
par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsEro <- list() ; j <- 0 # i <- i+1
for (i in 1:length(profs)){
  check1 <- NROW(subset(rf18Ero, rf18Ero$Profile==profs[i]))
  check2 <- NROW(is.na(subset(rf18Ero$pH.CaCl2, rf18Ero$Profile==profs[i]))) -
    sum(is.na(subset(rf18Ero$pH.CaCl2, rf18Ero$Profile==profs[i])))
  if(check1 < 2 | check2 <= 1){
    j <- j+1
  } else {
    with(rf18Ero, plot(Depth.mean ~ pH.CaCl2, xlim=xrng, ylim=c(100,0),
           subset = rf18Ero$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
    mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
    lss0 <- loess(pH.CaCl2 ~ Depth.mean, data = rf18Ero,
                  subset = rf18Ero$Profile==profs[i], span=1)
    smth0 <- predict(lss0,depfs)
    lst0 <- c(`names<-`(list(smth0), paste0("smooth",profs[i])))
    loessModsEro <- list.append(loessModsEro,lst0)
    lines(depfs ~ smth0)
  }
}
cat("number of non-interpolated profiles: ",j," out of ",length(profs),"\n")
all_loess_Ero <- as.data.frame(matrix(unlist(loessModsEro),
                                      ncol=length(loessModsEro)))
colnames(all_loess_Ero) <- unique(str_trunc(names(unlist(loessModsEro)),
                                            width=9, side="right", ""))
all_loess_Ero$smoothMean <- rowMeans(all_loess_Ero, na.rm=T)
all_loess_Ero <- cbind(depfs,all_loess_Ero)
with(all_loess_Ero,plot(smoothMean, depfs, xlim=xrng, ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")

# _._._._._._._._._._._._._._Depositional_._._._._._._._._._._._._._._._._####
#
depfs <- seq(5,100,10)
profs <- as.character(unique(rf18Dep$Profile))
xrng <- range(rf2018$pH.CaCl2, na.rm = TRUE)
par(mfrow=c(3,6), mar = c(3,3,0.2,0.5), mgp=c(1.2,0.2,0), tcl=0.15)
loessModsDep <- list() ; j <- 0 # i <- i+1
for (i in 1:length(profs)){
  check1 <- NROW(subset(rf18Dep, rf18Dep$Profile==profs[i]))
  check2 <- NROW(is.na(subset(rf18Dep$pH.CaCl2, rf18Dep$Profile==profs[i]))) -
    sum(is.na(subset(rf18Dep$pH.CaCl2, rf18Dep$Profile==profs[i])))
  if(check1 < 2 | check2 <= 1){
    j <- j+1
  } else {
    with(rf18Dep, plot(Depth.mean ~ pH.CaCl2, xlim=xrng, ylim=c(100,0),
          subset = rf18Dep$Profile==profs[i], pch=19, col = "red3", cex = 1.4))
    mtext(paste("Profile",profs[i]), 3, -1.5, adj=0.99, col = "tan")
    lss0 <- loess(pH.CaCl2 ~ Depth.mean, data = rf18Dep,
                  subset = rf18Dep$Profile==profs[i], span=1)
    smth0 <- predict(lss0,depfs)
    lst0 <- c(`names<-`(list(smth0), paste0("smooth",profs[i])))
    loessModsDep <- list.append(loessModsDep,lst0)
    lines(depfs ~ smth0)
  }
}
cat("number of non-interpolated profiles: ",j," out of ",length(profs),"\n")
all_loess_Dep <- as.data.frame(matrix(unlist(loessModsDep),
                                      ncol=length(loessModsDep)))
colnames(all_loess_Dep) <- unique(str_trunc(names(unlist(loessModsDep)),
                                            width=9, side="right", ""))
all_loess_Dep$smoothMean <- rowMeans(all_loess_Dep, na.rm=T)
all_loess_Dep <- cbind(depfs,all_loess_Dep)
with(all_loess_Dep,plot(smoothMean, depfs, xlim=xrng, ylim=c(100,0)))
mtext("Mean", 3, -1.5, adj=0.99, col = "tan")

# _._._._._._._._._._._._._._._._._Overall_._._._._._._._._._._._._._._._._####

# this would need editing if there were more landform categories!

loessModsMeansLF3_pH.CaCl2 <- data.frame(Depth=depfs,
                                Depositional=all_loess_Dep$smoothMean,
                                Erosional=all_loess_Ero$smoothMean,
                                Residual=all_loess_Res$smoothMean)
par(mfrow=c(1,1), mar = c(1,4,4,1), font.lab=2)
with(loessModsMeansLF3_pH.CaCl2, plot(Depth ~ Depositional, type="b",
                             xlim=xrng, ylim=c(100,0),
                             xlab="",xaxt="n",
                             ylab="Depth (cm)", cex.lab=1.4))
axis(3);mtext("Interpolated mean pH (CaCl2)",3,2,font=2,cex=1.4)
with(loessModsMeansLF3_pH.CaCl2, lines(Depth ~ Erosional, type="b",
                                      col="blue", pch=2))
with(loessModsMeansLF3_pH.CaCl2, lines(Depth ~ Residual, type="b",
                                      col="red3", pch=19))
legend("topright", bty = "n", inset = 0.02, y.intersp = 1.2, cex = 1.4,
       legend=names(loessModsMeansLF3_pH.CaCl2[,-1]), col=c(1,"blue","red3"),
       pch=c(1,2,19), title="Landform category")
