library('scales')

D1 = read.csv("data/Training.csv")
D2 = read.csv("data/blood.csv")
D3 = read.csv("data/HRV.csv")
D4 = read.csv("data/survey.csv")
colnames(D4)[dim(D4)[2]] = "睡眠得分"
colnames(D1);colnames(D2);colnames(D3);colnames(D4)
DD = data.frame(D2,D1[,6:12],D3[,8:21],D4[,5:29])
colnames(DD)[7:27] = c('身高','內臟脂肪','體重','BMI','體脂肪','筋肉','骨',
                       'kcal','體內年齡','水份','收縮壓','舒張壓','心率','血氧',
                       '2.44公尺(1)','2.44公尺(2)','3公尺(1)','3公尺(2)','30秒坐站','抬腿時間','蝦米時間')

DD[,2] = as.character(DD[,2])
for(i in 1:dim(DD)[1]){
  if(DD[i,2]=="4/11(三)"){
    DD[i,2] = "4/11"
  } else {
    if(DD[i,2]=="4/14(六)"){
      DD[i,2] = "4/14"
    } else {
      if(DD[i,2]=="4/16(一)"){
        DD[i,2] = "4/16"
      } else {
        DD[i,2] = "4/18"
      }
    }
  }
}

#2.44公尺、3公尺 取最佳
for(i in 1:dim(DD)[1]){
  if(is.na(DD[i,21])){
    DD[i,21] = NA 
  } else {
    if(which.min(DD[i,21:22])[[1]]==1){
      DD[i,67] = DD[i,21]
    } else { DD[i,67] = DD[i,22] }
  }
}

for(i in 1:dim(DD)[1]){
  if(is.na(DD[i,23])){
    DD[i,23] = NA 
  } else {
    if(which.min(DD[i,23:24])[[1]]==1){
      DD[i,68] = DD[i,23]
    } else { DD[i,68] = DD[i,24] }
  }
}
DD[,21:22] = DD[,67:68]
colnames(DD)[21:22] = c("2.44公尺","3公尺")
DD = DD[,-c(23,24,67,68)]
DD = cbind(DD,D1[,13:20])
colnames(DD)[3] = "ID"
DD[,3] = as.character(DD[,3])

# write.csv(DD,"data/清明斷食.csv")

################# DD12 #################
D112 = read.csv("data/Training12.csv")
D212 = read.csv("data/blood12.csv")
D312 = read.csv("data/HRV12.csv")
D412 = read.csv("data/survey12.csv")
colnames(D412)[dim(D412)[2]] = "睡眠得分"
DD12 = data.frame(D212,D112[,6:12],D312[,8:21],D412[,5:29])
colnames(DD12)[7:27] = c('身高','內臟脂肪','體重','BMI','體脂肪','筋肉','骨',
                       'kcal','體內年齡','水份','收縮壓','舒張壓','心率','血氧',
                       '2.44公尺(1)','2.44公尺(2)','3公尺(1)','3公尺(2)','30秒坐站','抬腿時間','蝦米時間')

DD12[,2] = as.character(DD12[,2])
for(i in 1:dim(DD12)[1]){
  if(DD12[i,2]=="4/11(三)"){
    DD12[i,2] = "4/11"
  } else {
    if(DD12[i,2]=="4/14(六)"){
      DD12[i,2] = "4/14"
    } else {
      if(DD12[i,2]=="4/16(一)"){
        DD12[i,2] = "4/16"
      } else {
        DD12[i,2] = "4/18"
      }
    }
  }
}
#2.44公尺、3公尺 取最佳
for(i in 1:dim(DD12)[1]){
  if(is.na(DD12[i,21])){
    DD12[i,21] = NA 
  } else {
    if(which.min(DD12[i,21:22])[[1]]==1){
      DD12[i,67] = DD12[i,21]
    } else { DD12[i,67] = DD12[i,22] }
  }
}
  
for(i in 1:dim(DD12)[1]){
  if(is.na(DD12[i,23])){
    DD12[i,23] = NA 
  } else {
    if(which.min(DD12[i,23:24])[[1]]==1){
      DD12[i,68] = DD12[i,23]
    } else { DD12[i,68] = DD12[i,24] }
 }
}
DD12[,21:22] = DD12[,67:68]
colnames(DD12)[21:22] = c("2.44公尺","3公尺")
DD12 = DD12[,-c(23,24,67,68)]
DD12 = cbind(DD12,D112[,13:20])
colnames(DD12)[3] = "ID"

################# fmfs #################
c = DD12[,c(3,7:44,45,46:50,51,52:55,56,61:64,72)]

c1 = c[which(c[,56]=="初"),]
c2 = c[which(c[,56]=="斷3"),]
c3 = c[which(c[,56]=="復2"),] ; c3 = c3[order(c3$ID),]

t1 = matrix(0,11,56) ; t2 = matrix(0,11,56)
for(i in 2:55){
  for(j in 1:11){
    if(is.na(c2[j,i])){  
      t1[j,i] = is.na(t1[j,i])
    } else {
      t1[j,i] = (c2[j,i] - c1[j,i]) / c1[j,i]
    }
    t2[j,i] = (c3[j,i] - c1[j,i]) / c1[j,i]
  }
}
t1[4,54] = 0 ; t2[4,54] = 0
t1[,1] = unique(c$ID) ; t2[,1] = unique(c$ID)
t1[,56] = "斷3-初" ; t2[,56] = "復2-初"
t = rbind(t1,t2)
t = as.data.frame(t, stringsAsFactors=FALSE) ; colnames(t) = colnames(c)[1:55]
for(i in 2:55){
  t[,i] = as.numeric(t[,i])
}
colnames(t)[56] = "狀態"
fm1 = apply(t[which(t[,56]=="斷3-初"),2:55],2,mean,na.rm=T)
fs1 = apply(t[which(t[,56]=="斷3-初"),2:55],2,sd,na.rm=T)
fm2 = apply(t[which(t[,56]=="復2-初"),2:55],2,mean,na.rm=T)
fs2 = apply(t[which(t[,56]=="復2-初"),2:55],2,sd,na.rm=T)
fmfs = data.frame(rbind(0,t(fm1),t(fm2),0,t(fs1),t(fs2)))
fmfs = data.frame(value = c("mean","mean","mean","sd","sd","sd"),"狀態"=c("0","斷3-初","復2-初","0","斷3-初","復2-初"),fmfs)
colnames(fmfs)[16:18] = c("2.44公尺","3公尺","30秒坐站")
fmfs[,2] = as.character(fmfs[,2])
fmfs[,2] = factor(fmfs[,2],c("0","斷3-初","復2-初"))

################# fmfs05 #################
d = DD12[which(c$ID %in% c$ID[which(c[,56]=="斷5")]),c(3,7:44,45,46:50,51,52:55,56,61:64,72)]

d1 = d[which(d[,56]=="初"),] ; d2 = d[which(d[,56]=="斷3"),]
d3 = d[which(d[,56]=="斷5"),] ; d4 = d[which(d[,56]=="復2"),]

f1 = matrix(0,5,56) ; f2 = matrix(0,5,56) ; f3 = matrix(0,5,56)
for(i in 2:55){
  for(j in 1:5){
    if(is.na(d2[j,i])){  
      f1[j,i] = is.na(f1[j,i])
    } else {
      f1[j,i] = (d2[j,i] - d1[j,i]) / d1[j,i]
    }
    f2[j,i] = (d3[j,i] - d1[j,i]) / d1[j,i]
    f3[j,i] = (d4[j,i] - d1[j,i]) / d1[j,i]
  }
}
f1[,1] = 1:5 ; f2[,1] = 1:5 ; f3[,1] = 1:5
f1[,56] = "斷3-初" ; f2[,56] = "斷5-初" ; f3[,56] = "復2-初"
f = rbind(f1,f2,f3)
f = as.data.frame(f, stringsAsFactors=FALSE) ; colnames(f) = colnames(c)[1:55]
for(i in 2:55){
  f[,i] = as.numeric(f[,i])
}
fm1 = apply(f[which(f[,56]=="斷3-初"),2:55],2,mean,na.rm=T)
fs1 = apply(f[which(f[,56]=="斷3-初"),2:55],2,sd,na.rm=T)
fm2 = apply(f[which(f[,56]=="斷5-初"),2:55],2,mean,na.rm=T)
fs2 = apply(f[which(f[,56]=="斷5-初"),2:55],2,sd,na.rm=T)
fm3 = apply(f[which(f[,56]=="復2-初"),2:55],2,mean,na.rm=T)
fs3 = apply(f[which(f[,56]=="復2-初"),2:55],2,sd,na.rm=T)
fmfs05 = data.frame(rbind(0,t(fm1),t(fm2),t(fm3),0,t(fs1),t(fs2),t(fs3)))
fmfs05 = data.frame(value = c("mean","mean","mean","mean","sd","sd","sd","sd"),
                  "狀態"=c("0","斷3-初","斷5-初","復2-初","0","斷3-初","斷5-初","復2-初"),fmfs05)
colnames(fmfs05)[16:18] = c("2.44公尺","3公尺","30秒坐站")
fmfs05[,2] = as.character(fmfs05[,2])
fmfs05[,2] = factor(fmfs05[,2],c("0","斷3-初","斷5-初","復2-初"))

