library(ggplot2)
library(fmsb)
#library(plotly)
function(input, output) {
  
  output$T1 <- DT::renderDataTable(DT::datatable({
    DD[which(DD[,2]=="4/11"),c(3,6:7,
                               if("電阻式體脂計" %in% input$VAR){8:16},
                               if("血壓" %in% input$VAR){17:20},
                               if("體能" %in% input$VAR){21:25},
                               if("HRV" %in% input$VAR){26:39},
                               if("問卷得分" %in% input$VAR){61:64},
                               if("陽虛體質" %in% input$VAR){40:45},
                               if("陰虛體質" %in% input$VAR){46:51},
                               if("痰瘀體質" %in% input$VAR){52:56}
    )]
  }))
  output$T2 <- DT::renderDataTable(DT::datatable({
    DD[which(DD[,2]=="4/14"),c(4,6:7,
                               if("電阻式體脂計" %in% input$VAR){8:16},
                               if("血壓" %in% input$VAR){17:20},
                               if("體能" %in% input$VAR){21:25},
                               if("HRV" %in% input$VAR){26:39},
                               if("問卷得分" %in% input$VAR){61:64},
                               if("陽虛體質" %in% input$VAR){40:45},
                               if("陰虛體質" %in% input$VAR){46:51},
                               if("痰瘀體質" %in% input$VAR){52:56}
    )]
  }))
  output$T3 <- DT::renderDataTable(DT::datatable({
    DD[which(DD[,2]=="4/16"),c(4,6:7,
                               if("電阻式體脂計" %in% input$VAR){8:16},
                               if("血壓" %in% input$VAR){17:20},
                               if("體能" %in% input$VAR){21:25},
                               if("HRV" %in% input$VAR){26:39},
                               if("問卷得分" %in% input$VAR){61:64},
                               if("陽虛體質" %in% input$VAR){40:45},
                               if("陰虛體質" %in% input$VAR){46:51},
                               if("痰瘀體質" %in% input$VAR){52:56}
    )]
  }))
  output$T4 <- DT::renderDataTable(DT::datatable({
    DD[which(DD[,2]=="4/18"),c(4,6:7,
                               if("電阻式體脂計" %in% input$VAR){8:16},
                               if("血壓" %in% input$VAR){17:20},
                               if("體能" %in% input$VAR){21:25},
                               if("HRV" %in% input$VAR){26:39},
                               if("問卷得分" %in% input$VAR){61:64},
                               if("陽虛體質" %in% input$VAR){40:45},
                               if("陰虛體質" %in% input$VAR){46:51},
                               if("痰瘀體質" %in% input$VAR){52:56}
    )]
  }))
  output$P11 <- renderPlot({
    if(input$group == 'HRV'){
      ggplot(DD, aes(x=DD[,2], y=DD[,which(colnames(DD)==input$vvrr1)],color=DD[,2])) +
        geom_boxplot(size=1) + theme_bw()+ labs(colour = "Date",x = "Date", y = colnames(DD)[which(colnames(DD) == input$vvrr1)])
    } else{
      if(input$group == '電阻式體脂計'){
        ggplot(DD, aes(x=DD[,2], y=DD[,which(colnames(DD)==input$vvrr2)],color=DD[,2])) + 
          geom_boxplot(size=1) + theme_bw()+ labs(colour = "Date",x = "Date", y = colnames(DD)[which(colnames(DD) == input$vvrr2)])
      } else {
        if(input$group == '血壓'){
          ggplot(DD, aes(x=DD[,2], y=DD[,which(colnames(DD)==input$vvrr3)],color=DD[,2])) + 
            geom_boxplot(size=1) + theme_bw()+ labs(colour = "Date",x = "Date", y = colnames(DD)[which(colnames(DD) == input$vvrr3)])
        } else {
          if(input$group == '體能'){
            ggplot(DD, aes(x=DD[,2], y=DD[,which(colnames(DD)==input$vvrr4)],color=DD[,2])) + 
              geom_boxplot(size=1) + theme_bw()+ labs(colour = "Date",x = "Date", y = colnames(DD)[which(colnames(DD) == input$vvrr4)])
          } else {
            if(input$group == '問卷得分'){
              ggplot(DD, aes(x=DD[,2], y=DD[,which(colnames(DD)==input$vvrr5)],color=DD[,2])) + 
                geom_boxplot(size=1) + theme_bw()+ labs(colour = "Date",x = "Date", y = colnames(DD)[which(colnames(DD) == input$vvrr5)])
            } else {
              ggplot(DD, aes(x=DD[,2], y=DD[,which(colnames(DD)==input$vvrr6)],color=DD[,2])) + 
                geom_boxplot(size=1) + theme_bw()+ labs(colour = "Date",x = "Date", y = colnames(DD)[which(colnames(DD) == input$vvrr6)])
            }
          }
        }
      }
    }
    
    
  })
  output$P12 <- renderPlot({
    a = DD12
    a = a[which(a[,dim(a)[2]]==c("初") | a[,dim(a)[2]]==c("斷3") | a[,dim(a)[2]]==c("復2")),]
    a[,dim(a)[2]] = factor(a[,dim(a)[2]], levels=c("初","斷3","斷5","復2","復4"))
    if(input$group3 == 'HRV'){
      ggplot(a, aes(x=a[,dim(a)[2]],y=a[,which(colnames(a)==input$vvrr13)],color=a[,dim(a)[2]])) + 
        geom_boxplot(size=1) + theme_bw()+ labs(colour = "分群",x = "狀態", y = colnames(a)[which(colnames(a) == input$vvrr13)])
    } else{
      if(input$group3 == '電阻式體脂計'){
        ggplot(a, aes(x=a[,dim(a)[2]],y=a[,which(colnames(a)==input$vvrr14)],color=a[,dim(a)[2]])) + 
          geom_boxplot(size=1) + theme_bw()+ labs(colour = "分群",x = "狀態", y = colnames(a)[which(colnames(a) == input$vvrr14)])
      } else {
        if(input$group3 == '血壓'){
          ggplot(a, aes(x=a[,dim(a)[2]],y=a[,which(colnames(a)==input$vvrr15)],color=a[,dim(a)[2]])) + 
            geom_boxplot(size=1) + theme_bw()+ labs(colour = "分群",x = "狀態", y = colnames(a)[which(colnames(a) == input$vvrr15)])
        } else {
          if(input$group3 == '體能'){
            ggplot(a, aes(x=a[,dim(a)[2]],y=a[,which(colnames(a)==input$vvrr16)],color=a[,dim(a)[2]])) + 
              geom_boxplot(size=1) + theme_bw()+ labs(colour = "分群",x = "狀態", y = colnames(a)[which(colnames(a) == input$vvrr16)])
          } else {
            if(input$group3 == '問卷得分'){
              ggplot(a, aes(x=a[,dim(a)[2]],y=a[,which(colnames(a)==input$vvrr17)],color=a[,dim(a)[2]])) + 
                geom_boxplot(size=1) + theme_bw()+ labs(colour = "分群",x = "狀態", y = colnames(a)[which(colnames(a) == input$vvrr17)])
            } else {
              ggplot(a, aes(x=a[,dim(a)[2]],y=a[,which(colnames(a)==input$vvrr18)],color=a[,dim(a)[2]])) + 
                geom_boxplot(size=1) + theme_bw()+ labs(colour = "分群",x = "狀態", y = colnames(a)[which(colnames(a) == input$vvrr18)])
            }
          }
        }
      }
    }
    
    
  })
  output$S11 <- renderPrint({
    if(input$group == 'HRV'){
      a = rbind(boxplot(DD[which(DD[,2]=="4/11"),which(colnames(DD)==input$vvrr1)])$stats[,1],
                boxplot(DD[which(DD[,2]=="4/14"),which(colnames(DD)==input$vvrr1)])$stats[,1],
                boxplot(DD[which(DD[,2]=="4/16"),which(colnames(DD)==input$vvrr1)])$stats[,1],
                boxplot(DD[which(DD[,2]=="4/18"),which(colnames(DD)==input$vvrr1)])$stats[,1])
      rownames(a) = unique(DD[,2]) ; colnames(a) = c("min","Q1","median","Q3","max");a
    } else{
      if(input$group == '電阻式體脂計'){
        
        a = rbind(boxplot(DD[which(DD[,2]=="4/11"),which(colnames(DD)==input$vvrr2)])$stats[,1],
                  boxplot(DD[which(DD[,2]=="4/14"),which(colnames(DD)==input$vvrr2)])$stats[,1],
                  boxplot(DD[which(DD[,2]=="4/16"),which(colnames(DD)==input$vvrr2)])$stats[,1],
                  boxplot(DD[which(DD[,2]=="4/18"),which(colnames(DD)==input$vvrr2)])$stats[,1])
        rownames(a) = unique(DD[,2]) ; colnames(a) = c("min","Q1","median","Q3","max");a
      } else {
        if(input$group == '血壓'){
          a = rbind(boxplot(DD[which(DD[,2]=="4/11"),which(colnames(DD)==input$vvrr3)])$stats[,1],
                    boxplot(DD[which(DD[,2]=="4/14"),which(colnames(DD)==input$vvrr3)])$stats[,1],
                    boxplot(DD[which(DD[,2]=="4/16"),which(colnames(DD)==input$vvrr3)])$stats[,1],
                    boxplot(DD[which(DD[,2]=="4/18"),which(colnames(DD)==input$vvrr3)])$stats[,1])
          rownames(a) = unique(DD[,2]) ; colnames(a) = c("min","Q1","median","Q3","max");a
        } else {
          if(input$group == '體能'){
            a = rbind(boxplot(DD[which(DD[,2]=="4/11"),which(colnames(DD)==input$vvrr4)])$stats[,1],
                      boxplot(DD[which(DD[,2]=="4/14"),which(colnames(DD)==input$vvrr4)])$stats[,1],
                      boxplot(DD[which(DD[,2]=="4/16"),which(colnames(DD)==input$vvrr4)])$stats[,1],
                      boxplot(DD[which(DD[,2]=="4/18"),which(colnames(DD)==input$vvrr4)])$stats[,1])
            rownames(a) = unique(DD[,2]) ; colnames(a) = c("min","Q1","median","Q3","max");a
          } else {
            if(input$group == '體能'){
              a = rbind(boxplot(DD[which(DD[,2]=="4/11"),which(colnames(DD)==input$vvrr5)])$stats[,1],
                        boxplot(DD[which(DD[,2]=="4/14"),which(colnames(DD)==input$vvrr5)])$stats[,1],
                        boxplot(DD[which(DD[,2]=="4/16"),which(colnames(DD)==input$vvrr5)])$stats[,1],
                        boxplot(DD[which(DD[,2]=="4/18"),which(colnames(DD)==input$vvrr5)])$stats[,1])
              rownames(a) = unique(DD[,2]) ; colnames(a) = c("min","Q1","median","Q3","max");a
            } else {
              a = rbind(boxplot(DD[which(DD[,2]=="4/11"),which(colnames(DD)==input$vvrr6)])$stats[,1],
                        boxplot(DD[which(DD[,2]=="4/14"),which(colnames(DD)==input$vvrr6)])$stats[,1],
                        boxplot(DD[which(DD[,2]=="4/16"),which(colnames(DD)==input$vvrr6)])$stats[,1],
                        boxplot(DD[which(DD[,2]=="4/18"),which(colnames(DD)==input$vvrr6)])$stats[,1])
              rownames(a) = unique(DD[,2]) ; colnames(a) = c("min","Q1","median","Q3","max");a
            }
          }
        }
      }
    }
    
    
  })
  output$S12 <- renderPrint({
    b = DD12
    b = b[which(b[,dim(b)[2]]==c("初") | b[,dim(b)[2]]==c("斷3") | b[,dim(b)[2]]==c("復2")),]
    b[,dim(b)[2]] = factor(b[,dim(b)[2]], levels=c("初","斷3","斷5","復2","復4"))
    if(input$group == 'HRV'){
      a = rbind(boxplot(b[which(b[,dim(b)[2]]=="初"),which(colnames(b)==input$vvrr1)])$stats[,1],
                boxplot(b[which(b[,dim(b)[2]]=="斷3"),which(colnames(b)==input$vvrr1)])$stats[,1],
                boxplot(b[which(b[,dim(b)[2]]=="復2"),which(colnames(b)==input$vvrr1)])$stats[,1])
      rownames(a) = unique(b[,dim(b)[2]]) ; colnames(a) = c("min","Q1","median","Q3","max");a
    } else{
      if(input$group == '電阻式體脂計'){
        a = rbind(boxplot(b[which(b[,dim(b)[2]]=="初"),which(colnames(b)==input$vvrr2)])$stats[,1],
                  boxplot(b[which(b[,dim(b)[2]]=="斷3"),which(colnames(b)==input$vvrr2)])$stats[,1],
                  boxplot(b[which(b[,dim(b)[2]]=="復2"),which(colnames(b)==input$vvrr2)])$stats[,1])
        rownames(a) = unique(b[,dim(b)[2]]) ; colnames(a) = c("min","Q1","median","Q3","max");a
      } else {
        if(input$group == '血壓'){
          a = rbind(boxplot(b[which(b[,dim(b)[2]]=="初"),which(colnames(b)==input$vvrr3)])$stats[,1],
                    boxplot(b[which(b[,dim(b)[2]]=="斷3"),which(colnames(b)==input$vvrr3)])$stats[,1],
                    boxplot(b[which(b[,dim(b)[2]]=="復2"),which(colnames(b)==input$vvrr3)])$stats[,1])
          rownames(a) = unique(b[,dim(b)[2]]) ; colnames(a) = c("min","Q1","median","Q3","max");a
        } else {
          if(input$group == '體能'){
            a = rbind(boxplot(b[which(b[,dim(b)[2]]=="初"),which(colnames(b)==input$vvrr4)])$stats[,1],
                      boxplot(b[which(b[,dim(b)[2]]=="斷3"),which(colnames(b)==input$vvrr4)])$stats[,1],
                      boxplot(b[which(b[,dim(b)[2]]=="復2"),which(colnames(b)==input$vvrr4)])$stats[,1])
            rownames(a) = unique(b[,dim(b)[2]]) ; colnames(a) = c("min","Q1","median","Q3","max");a
          } else {
            if(input$group == '體能'){
              a = rbind(boxplot(b[which(b[,dim(b)[2]]=="初"),which(colnames(b)==input$vvrr5)])$stats[,1],
                        boxplot(b[which(b[,dim(b)[2]]=="斷3"),which(colnames(b)==input$vvrr5)])$stats[,1],
                        boxplot(b[which(b[,dim(b)[2]]=="復2"),which(colnames(b)==input$vvrr5)])$stats[,1])
              rownames(a) = unique(b[,dim(b)[2]]) ; colnames(a) = c("min","Q1","median","Q3","max");a
            } else {
              a = rbind(boxplot(b[which(b[,dim(b)[2]]=="初"),which(colnames(b)==input$vvrr6)])$stats[,1],
                        boxplot(b[which(b[,dim(b)[2]]=="斷3"),which(colnames(b)==input$vvrr6)])$stats[,1],
                        boxplot(b[which(b[,dim(b)[2]]=="復2"),which(colnames(b)==input$vvrr6)])$stats[,1])
              rownames(a) = unique(b[,dim(b)[2]]) ; colnames(a) = c("min","Q1","median","Q3","max");a
            }
          }
        }
      }
    }
    
    
  })
  output$P21 <- renderPlot({
    w = c() ; e = c()
    for(i in 40:44){ w[i-39] = DD[which(DD[,3]==input$Num & DD[,2]=="4/11"),i] }
    if(input$Num %in% c(1,3,6,11,13,15)){
      for(i in 40:44){ e[i-39] = DD[which(DD[,3]==input$Num & DD[,2]=="4/16"),i] }
    } else {
      if(input$Num %in% c(2,4,7:10)){
        for(i in 40:44){ e[i-39] = DD[which(DD[,3]==input$Num & DD[,2]=="4/14"),i] }
      } else {
        for(i in 40:44){ e[i-39] = DD[which(DD[,3]==input$Num & DD[,2]=="4/14"),i] }
      }
    }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("頭部陽虛" , "胸腔陽虛" , "四肢陽虛" , "腹腔陽虛" , "體表陽虛")
    rownames(data) = c("禁食","復食")
    data = rbind(rep(15,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart(data  , axistype=1,seg=5 ,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,15,3), cglwd=1.3, vlcex=1.3,
               title  = paste0("總斷食分鐘數=",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),69],"時",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),70],"分"))
    legend(x=0.9, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=19 , col=colors_in , text.col = "grey", cex=1.4, pt.cex=2.5)
  },width = 800, height = 600)
  output$P22 <- renderPlot({
    w = c() ; e = c()
    for(i in 46:50){ w[i-45] = DD[which(DD[,3]==input$Num & DD[,2]=="4/11"),i] }
    if(input$Num %in% c(1,3,6,11,13,15)){
      for(i in 46:50){ e[i-45] = DD[which(DD[,3]==input$Num & DD[,2]=="4/16"),i] }
    } else {
      if(input$Num %in% c(2,4,7:10)){
        for(i in 46:50){ e[i-45] = DD[which(DD[,3]==input$Num & DD[,2]=="4/14"),i] }
      } else {
        for(i in 46:50){ e[i-45] = DD[which(DD[,3]==input$Num & DD[,2]=="4/14"),i] }
      }
    }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("頭部陰虛" , "四肢陰虛" , "腸胃道陰虛" , "體表陰虛" , "腹腔陰虛")
    rownames(data) = c("禁食","復食")
    data = rbind(rep(20,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart( data  , axistype=1 ,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=1.3, vlcex=1.3,
                title  = paste0("總斷食分鐘數=",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),69],"時",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),70],"分")) 
    legend(x=0.9, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=19, col=colors_in , text.col = "grey", cex=1.4, pt.cex=2.5)
  },width = 800, height = 600)
  output$P23 <- renderPlot({
    w = c() ; e = c()
    for(i in 52:55){ w[i-51] = DD[which(DD[,3]==input$Num & DD[,2]=="4/11"),i] }
    if(input$Num %in% c(1,3,6,11,13,15)){
      for(i in 52:55){ e[i-51] = DD[which(DD[,3]==input$Num & DD[,2]=="4/16"),i] }
    } else {
      if(input$Num %in% c(2,4,7:10)){
        for(i in 52:55){ e[i-51] = DD[which(DD[,3]==input$Num & DD[,2]=="4/14"),i] }
      } else {
        for(i in 52:55){ e[i-51] = DD[which(DD[,3]==input$Num & DD[,2]=="4/14"),i] }
      }
    }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("軀幹痰瘀" , "體表痰瘀" , "頭部痰瘀" , "腸胃道痰瘀")
    rownames(data) = c("禁食","復食")
    data = rbind(rep(15,4) , rep(0,4) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart( data  , axistype=1,seg=5,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,15,3), cglwd=1.5, vlcex=1.5,
                title  = paste0("總斷食分鐘數=",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),69],"時",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),70],"分")) 
    legend(x=0.9, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=19 , col=colors_in , text.col = "grey", cex=1.4, pt.cex=2.5)
  },width = 800, height = 600)
  
  output$P2222 <- renderPlot({
    
    w = c() ; e = c()
    for(i in 36:40){ w[i-35] = t[which(t[,1] == input$Num1 & t[,56]=="斷3-初"),i] }
    for(i in 36:40){ e[i-35] = t[which(t[,1] == input$Num1 & t[,56]=="復2-初"),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("頭部陽虛" , "胸腔陽虛" , "四肢陽虛" , "腹腔陽虛" , "體表陽虛")
    rownames(data) = c("禁食","復食")
    data = rbind(rep(2,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart(data  , axistype=1,seg=4 ,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
               cglcol="grey", cglty=1, axislabcol="grey",caxislabels=paste0(seq(0,200,50),"%"), cglwd=1.3, vlcex=1.3,
               title  = paste0("總斷食分鐘數=",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),69],"時",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),70],"分"))
    legend(x=0.8, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=19 , col=colors_in , text.col = "grey", cex=1.4, pt.cex=2.5)
  },width = 800, height = 600)
  
  output$PPAP <- renderPlot({
    w = c() ; e = c()
    for(i in 41:45){ w[i-40] = t[which(t[,1] == input$Num1 & t[,56]=="斷3-初"),i] }
    for(i in 41:45){ e[i-40] = t[which(t[,1] == input$Num1 & t[,56]=="復2-初"),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("頭部陰虛" , "四肢陰虛" , "腸胃道陰虛" , "體表陰虛" , "腹腔陰虛")
    rownames(data) = c("禁食","復食")
    data = rbind(rep(2,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart( data  , axistype=1 ,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=paste0(seq(0,200,50),"%"), cglwd=1.3, vlcex=1.3,
                title  = paste0("總斷食分鐘數=",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),69],"時",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),70],"分")) 
    legend(x=0.9, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=19, col=colors_in , text.col = "grey", cex=1.4, pt.cex=2.5)
  },width = 800, height = 600)
  
  output$PPPP <- renderPlot({
    w = c() ; e = c()
    for(i in 47:50){ w[i-46] = t[which(t[,1] == input$Num1 & t[,56]=="斷3-初"),i] }
    for(i in 47:50){ e[i-46] = t[which(t[,1] == input$Num1 & t[,56]=="復2-初"),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("軀幹痰瘀" , "體表痰瘀" , "頭部痰瘀" , "腸胃道痰瘀")
    rownames(data) = c("禁食","復食")
    data = rbind(rep(2,4) , rep(0,4) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    radarchart( data  , axistype=1,seg=4,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=paste0(seq(0,200,50),"%"), cglwd=1.5, vlcex=1.5,
                title  = paste0("總斷食分鐘數=",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),69],"時",DD[which(DD[,3]==input$Num & DD[,2]=="4/18"),70],"分")) 
    legend(x=0.9, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=19 , col=colors_in , text.col = "grey", cex=1.4, pt.cex=2.5)
  },width = 800, height = 600)
  
  
  
  output$P31 <- renderPlot({
    DD[,2] = factor(DD[,2],c("4/11","4/14","4/16","4/18"))
    DD[,3] = factor(DD[,3],1:15)
    DD = DD[which(DD$ID%in%input$WhoseNum),]
    if(input$group2 == 'HRV'){
      qplot(DD[,2], DD[,which(colnames(DD)==input$vvrr7)], data = DD, geom = "line",group = DD$ID,colour = DD$ID)+
        theme_bw()+geom_point(size=3)+labs(colour = "ID",x = "Date", y = colnames(DD)[which(colnames(DD)==input$vvrr7)]) +
        theme(text = element_text(size=15))
    } else{
      if(input$group2 == '電阻式體脂計'){
        qplot(DD[,2], DD[,which(colnames(DD)==input$vvrr8)], data = DD, geom = "line",group = DD$ID,colour = DD$ID)+
          theme_bw()+geom_point(size=3)+labs(colour = "ID",x = "Date", y = colnames(DD)[which(colnames(DD)==input$vvrr8)]) +
          theme(text = element_text(size=15))
      } else {
        if(input$group2 == '血壓'){
          qplot(DD[,2], DD[,which(colnames(DD)==input$vvrr9)], data = DD, geom = "line",group = DD$ID,colour = DD$ID)+ 
            theme_bw()+geom_point(size=3)+labs(colour = "ID",x = "Date", y = colnames(DD)[which(colnames(DD)==input$vvrr9)])  +
            theme(text = element_text(size=15))
          } else {
          if(input$group2 == '體能'){
            qplot(DD[,2], DD[,which(colnames(DD)==input$vvrr10)], data = DD, geom = "line",group = DD$ID,colour = DD$ID)+
              theme_bw()+geom_point(size=3)+labs(colour = "ID",x = "Date", y = colnames(DD)[which(colnames(DD)==input$vvrr10)]) +
              theme(text = element_text(size=15))
            } else {
            if(input$group2 == '問卷得分'){      
              qplot(DD[,2], DD[,which(colnames(DD)==input$vvrr11)], data = DD, geom = "line",group = DD$ID,colour = DD$ID)+
                theme_bw()+geom_point(size=3)+labs(colour = "ID",x = "Date", y = colnames(DD)[which(colnames(DD)==input$vvrr11)]) +
                theme(text = element_text(size=15))
            } else {
              qplot(DD[,2], DD[,which(colnames(DD)==input$vvrr12)], data = DD, geom = "line",group = DD$ID,colour = DD$ID)+
                theme_bw()+geom_point(size=3)+labs(colour = "ID",x = "Date", y = colnames(DD)[which(colnames(DD)==input$vvrr12)]) +
                theme(text = element_text(size=15))
            }
          }
        }
      }
    }
    
    
  })
  output$P32 <- renderPlot({
    if(input$group4 == 'HRV'){
      if(input$vvrr19 %in% c("HF.","LF.HF","ANS","Sym.Vag")){
        qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr19)],
              data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
          labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr19)]) + theme(text = element_text(size=15)) + 
          geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr19)] - 
                              fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr19)],
                            ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr19)] + 
                              fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr19)]), width=.1) + guides(colour=FALSE)
      } else {
        qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr19)],
              data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
          labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr19)]) + theme(text = element_text(size=15)) + 
          geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr19)] - 
                              fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr19)],
                            ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr19)] + 
                              fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr19)]), width=.1) + guides(colour=FALSE) +
          scale_y_continuous(limits = c(-1, 1))
      }
    } else{
      if(input$group4 == '電阻式體脂計'){
        qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr20)],
              data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
          labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr20)]) + theme(text = element_text(size=15)) + 
          geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr20)] - 
                              fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr20)],
                            ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr20)] + 
                              fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr20)]), width=.1) + guides(colour=FALSE) +
          scale_y_continuous(limits = c(-1, 1))
      } else {
        if(input$group4 == '血壓'){
          qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr21)],
                data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
            labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr21)]) + theme(text = element_text(size=15)) + 
            geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr21)] - 
                                fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr21)],
                              ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr21)] + 
                                fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr21)]), width=.1) + guides(colour=FALSE) +
            scale_y_continuous(limits = c(-1, 1))
        } else {
          if(input$group4 == '體能'){
            qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr22)],
                  data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
              labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr22)]) + theme(text = element_text(size=15)) + 
              geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr22)] - 
                                  fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr22)],
                                ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr22)] + 
                                  fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr22)]), width=.1) + guides(colour=FALSE) +
              scale_y_continuous(limits = c(-1, 1))
          } else {
            if(input$group4 == '問卷得分'){
              if(input$vvrr23 %in% c("簡式得分","壓力得分")){
                qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr23)],
                      data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr23)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr23)] - 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr23)],
                                    ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr23)] + 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr23)]), width=.1) + guides(colour=FALSE)
              } else {
                qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr23)],
                      data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr23)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr23)] - 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr23)],
                                    ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr23)] + 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr23)]), width=.1) + guides(colour=FALSE) +
                  scale_y_continuous(limits = c(-1, 1))
              }
            } else {
              if(input$vvrr24 == "頭部陽虛"){
                qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr24)],
                      data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr24)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr24)] - 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr24)],
                                    ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr24)] + 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr24)]), width=.1) + guides(colour=FALSE)
              } else {
                qplot(fmfs[which(fmfs[,1]=="mean"),2], fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr24)],
                      data = fmfs[which(fmfs[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs)[which(colnames(fmfs)==input$vvrr24)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr24)] - 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr24)],
                                    ymax=fmfs[which(fmfs[,1]=="mean"),which(colnames(fmfs)==input$vvrr24)] + 
                                      fmfs[which(fmfs[,1]=="sd"),which(colnames(fmfs)==input$vvrr24)]), width=.1) + guides(colour=FALSE) +
                  scale_y_continuous(limits = c(-1, 1))
              }
            }
          }
        }
      }
    }
    
    
  })
  output$P33 <- renderPlot({
    if(input$group5 == 'HRV'){
      if(input$vvrr25 %in% c("LF.HF","Sym.Vag")){
        qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr25)],
              data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
          labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr25)]) + theme(text = element_text(size=15)) + 
          geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr25)] - 
                              fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr25)],
                            ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr25)] + 
                              fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr25)]), width=.1) + guides(colour=FALSE)
      } else {
        qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr25)],
              data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
          labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr25)]) + theme(text = element_text(size=15)) + 
          geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr25)] - 
                              fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr25)],
                            ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr25)] + 
                              fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr25)]), width=.1) + guides(colour=FALSE) +
          scale_y_continuous(limits = c(-1, 1))
      }
    } else{
      if(input$group5 == '電阻式體脂計'){
        qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr26)],
              data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
          labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr26)]) + theme(text = element_text(size=15)) + 
          geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr26)] - 
                              fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr26)],
                            ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr26)] + 
                              fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr26)]), width=.1) + guides(colour=FALSE) +
          scale_y_continuous(limits = c(-1, 1))
      } else {
        if(input$group5 == '血壓'){
          qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr27)],
                data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
            labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr27)]) + theme(text = element_text(size=15)) + 
            geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr27)] - 
                                fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr27)],
                              ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr27)] + 
                                fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr27)]), width=.1) + guides(colour=FALSE) +
            scale_y_continuous(limits = c(-1, 1))
        } else {
          if(input$group5 == '體能'){
            qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr28)],
                  data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
              labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr28)]) + theme(text = element_text(size=15)) + 
              geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr28)] - 
                                  fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr28)],
                                ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr28)] + 
                                  fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr28)]), width=.1) + guides(colour=FALSE) +
              scale_y_continuous(limits = c(-1, 1))
          } else {
            if(input$group5 == '問卷得分'){
              if(input$vvrr29 %in% c("簡式得分","睡眠得分","壓力得分")){
                qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr29)],
                      data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr29)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr29)] - 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr29)],
                                    ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr29)] + 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr29)]), width=.1) + guides(colour=FALSE)
              } else {
                qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr29)],
                      data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr29)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr29)] - 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr29)],
                                    ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr29)] + 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr29)]), width=.1) + guides(colour=FALSE) +
                  scale_y_continuous(limits = c(-1, 1))
              }
            } else {
              if(input$vvrr30 %in% c("頭部陽虛","胸腔陽虛","體表陰虛")){
                qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr30)],
                      data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr30)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr30)] - 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr30)],
                                    ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr30)] + 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr30)]), width=.1) + guides(colour=FALSE)
              } else {
                qplot(fmfs05[which(fmfs05[,1]=="mean"),2], fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr30)],
                      data = fmfs05[which(fmfs05[,1]=="mean"),], geom = "line",group = 1 ,color="...")+ theme_bw() + geom_point(size=3) +
                  labs(x = "狀態", y = colnames(fmfs05)[which(colnames(fmfs05)==input$vvrr30)]) + theme(text = element_text(size=15)) + 
                  geom_errorbar(aes(ymin=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr30)] - 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr30)],
                                    ymax=fmfs05[which(fmfs05[,1]=="mean"),which(colnames(fmfs05)==input$vvrr30)] + 
                                      fmfs05[which(fmfs05[,1]=="sd"),which(colnames(fmfs05)==input$vvrr30)]), width=.1) + guides(colour=FALSE) +
                  scale_y_continuous(limits = c(-1, 1))
              }
            }
          }
        }
      }
    }
    
    
    
  })
  output$P34 <- renderPlot({
    t = t[which(t$ID %in% input$WhoseNum2),]
    t[,56] = factor(t[,56],c("斷3-初","復2-初"))
    t[,1] = factor(t[,1],t$ID)
    if(input$group6 == 'HRV'){
      qplot(t[,56], t[,which(colnames(t)==input$vvrr31)], data = t, geom = "line",group = t$ID,colour = t$ID)+
        theme_bw() + geom_point(size=3) + labs(colour = "ID",x = "狀態", y = colnames(t)[which(colnames(t)==input$vvrr31)]) +
        theme(text = element_text(size=15))
    } else{
      if(input$group6 == '電阻式體脂計'){
        qplot(t[,56], t[,which(colnames(t)==input$vvrr32)], data = t, geom = "line",group = t$ID,colour = t$ID)+
          theme_bw() + geom_point(size=3) + labs(colour = "ID",x = "狀態", y = colnames(t)[which(colnames(t)==input$vvrr32)]) +
          theme(text = element_text(size=15))
      } else {
        if(input$group6 == '血壓'){
          qplot(t[,56], t[,which(colnames(t)==input$vvrr33)], data = t, geom = "line",group = t$ID,colour = t$ID)+
            theme_bw() + geom_point(size=3) + labs(colour = "ID",x = "狀態", y = colnames(t)[which(colnames(t)==input$vvrr33)]) +
            theme(text = element_text(size=15))
        } else {
          if(input$group6 == '體能'){
            qplot(t[,56], t[,which(colnames(t)==input$vvrr34)], data = t, geom = "line",group = t$ID,colour = t$ID)+
              theme_bw() + geom_point(size=3) + labs(colour = "ID",x = "狀態", y = colnames(t)[which(colnames(t)==input$vvrr34)]) +
              theme(text = element_text(size=15))
          } else {
            if(input$group6 == '問卷得分'){      
              qplot(t[,56], t[,which(colnames(t)==input$vvrr35)], data = t, geom = "line",group = t$ID,colour = t$ID)+
                theme_bw() + geom_point(size=3) + labs(colour = "ID",x = "狀態", y = colnames(t)[which(colnames(t)==input$vvrr35)]) +
                theme(text = element_text(size=15))
            } else {
              qplot(t[,56], t[,which(colnames(t)==input$vvrr36)], data = t, geom = "line",group = t$ID,colour = t$ID)+
                theme_bw() + geom_point(size=3) + labs(colour = "ID",x = "狀態", y = colnames(t)[which(colnames(t)==input$vvrr36)]) +
                theme(text = element_text(size=15))
            }
          }
        }
      }
    }
    
    
  })
}