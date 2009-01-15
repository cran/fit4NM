`fit4NM` <-
function()
{  options(guiToolkit="RGtk2")
   require(gWidgets)
   require(tcltk)
   require(tkrplot)
   require(RGtk2)
   require(gWidgetsRGtk2)

#### -- setupWD handler
   setupWD<-function(h,...)  
   {  setwd(tclvalue(tkchooseDirectory()))
   }

#### -- OpenDataFile
   OpenDataFile<-function(h,...)
   {  file.data<<-tclvalue(tkgetOpenFile())
      D.temp<-readLines(file.data)
      flag<-TRUE
      skip.n<-0
      D.split<-strsplit(D.temp,split="",fixed=TRUE)
      while(flag)
      {  for(i in 1:round(length(D.temp)*0.1))
         {  if(D.split[[i]][1]=="C")
            {  skip.n<-i
            } else 
            { flag<-FALSE
            }
         }
         for(i in 1:round(length(D.temp)*0.1))
         {  if(D.split[[i]][1]=="#")
            { skip.n<-i
            } else 
            { flag<-FALSE
            }
         }
      }
      D.data<<-read.csv(file.data,skip=skip.n,header=FALSE,na.strings=".")
      D.data<<-D.data[,which(apply(D.data,2,function(x) sum(is.na(x)))!=nrow(D.data))]
   }

#### -- OpenControlFile

   OpenControlFile<-function(h,...)
   {  file.ctl<<-tclvalue(tkgetOpenFile())
      D.temp<-readLines(file.ctl)
      Current.CTL<<-D.temp
      NONMEM.CTL<<-D.temp
      temp.list<-strsplit(D.temp[grep("\\$INPUT",D.temp)],split=" ")[[1]][-1]
      if(length(which(temp.list==""))!=0) temp.list<-temp.list[-which(temp.list=="")]
      if(length(which(temp.list=="C"))!=0) temp.list<-temp.list[-which(temp.list=="C")]
      temp<-strsplit(temp.list,"=")
      if(length(unlist(temp))!=length(temp))
      {  for(i in 1:length(temp))
           if(length(temp[[i]])!=1) temp.list[i]<-temp[[i]][1]
      }
      Var.Name<<-temp.list
      var.list<-c("ID","TIME","DV","AMT","RATE","MDV","COV","DOSE","PK paramameter","Ignore                  ")
      a<-list()
      for(i in 1:length(Var.Name))
         a[[i]]<-gdroplist(var.list)
      button<-gbutton("OK",handler=function(h,...)
                        {  var.prop<-NULL
                           for(i in 1:length(Var.Name))
                              var.prop<-c(var.prop,svalue(a[[i]]))
                           Var.Prop.Keep<<-var.prop
                           DV.id<<-which(Var.Prop.Keep=="DV")
                           ID.id<<-which(Var.Prop.Keep=="ID")
                           TIME.id<<-which(Var.Prop.Keep=="TIME")
                           AMT.id<<-which(Var.Prop.Keep=="AMT")
                           MDV.id<<-which(Var.Prop.Keep=="MDV")
                           dispose(tt)
                         })
      tt <- gwindow("Choose Variable Properties",width=50)
      g<-ggroup(horizontal=FALSE,cont=tt)
      tmp<-gframe("Choose Variable Properties",cont=g)
      for(i in 1:length(Var.Name))
      {  tmp<-gframe(Var.Name[i],cont=g)   
         add(tmp,a[[i]])
      }
      tmp<-gframe("",cont=g)   
      add(tmp,button)
   }

#### -- Editor
   Editor<-function(h,...) 
   {  if(is.null(Current.CTL))
      {  edit.txt<-" "
      } else
      {  edit.txt<-Current.CTL[1]
         for(i in 2:length(Current.CTL))
           edit.txt<-paste(edit.txt,Current.CTL[i],sep="\n")
      }
      edit.win<-gwindow("Edit Control File")
      g<-ggroup(horizontal=FALSE,cont=edit.win)
      tmp<-gframe("Editor",container=g)
      a<-gtext(edit.txt,width=600,height=500,font.attr=c(sizes="large",family="monospace"))
      add(tmp,a)
      b<-gbutton("ok",handler=function(h,...){ 
             write.table(svalue(a),"temp.ctl",quote=FALSE,row.names=FALSE,col.names=FALSE)
             Current.CTL<<-readLines("temp.ctl")
             dispose(edit.win)})
      tmp<-gframe("Save",container=g)  
      add(tmp,b)
   }

#### -- XY.plot
   XY.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  condX.V <-svalue(VarList.X,index=T)
         condY.V<-svalue(VarList.Y,index=T)
         select.data<-D.data
         if(!is.na(condX.V)& !is.na(condY.V))
         {  X<-select.data[,condX.V]
            Y<-select.data[,condY.V]
            plot(X,Y,xlab=Var.Name[condX.V],ylab=Var.Name[condY.V])
         }
       }
       saveData<-function(h,...)
       {  condX.V <-svalue(VarList.X,index=T)
          condY.V<-svalue(VarList.Y,index=T)
          select.data<-D.data[,c(ID.id,condX.V,condY.V)]
          colnames(select.data)<-Var.Name[c(ID.id,condX.V,condY.V)]
          write.csv(select.data,tclvalue(tkgetSaveFile()))
       }
       VarList.X<-gdroplist(Var.Name)
       VarList.Y<-gdroplist(Var.Name)

       Button1<-gbutton("OK",handler=updatePlot)
       Button2<-gbutton("SAVE",handler=saveData)
    
       win<-gwindow("XY plot")
       BigGroup<-ggroup(cont=win)
       group<-ggroup(horizontal=FALSE,cont=BigGroup)
       tmp<-gframe(" X variable",container=group)
       add(tmp,VarList.X)
       tmp<-gframe(" Y variable",container=group)
       add(tmp,VarList.Y)

       tmp<-gframe("Plot",container=group)
       add(tmp,Button1,expand=TRUE)
       tmp<-gframe("Save",container=group)
       add(tmp,Button2,expand=TRUE)
       add(BigGroup,ggraphics())
   }

#### -- ID.plot
   ID.plot<-function(h,...)
   {   ID<-sort(unique(D.data[,ID.id]))
       ID<-matrix(ID,nrow=length(ID))
       colnames(ID)<-c("ID")
       cov.list<-which(Var.Prop.Keep=="COV")
       COV.data<-as.matrix(D.data[,cov.list])
       colnames(COV.data)<-Var.Name[cov.list]

       updatePlot<-function(h,...) 
       {  par(mfrow=c(1,1))
          plot(D.data[,TIME.id],D.data[,DV.id],type='n',xlab="TIME",ylab="DV")
          id<-names(table(D.data[,ID.id]))
          for(i in 1:length(id))
          {   data<-D.data[which(D.data[,ID.id]==as.numeric(id[i])),]
              data<-data[!is.na(data[,DV.id]),]
              lines(data[,TIME.id],data[,DV.id],lty=3)
          }
          data<-D.data[which(D.data[,ID.id]==svalue(IDlist)),]
          data<-data[!is.na(data[,DV.id]),]
          lines(data[,TIME.id],data[,DV.id],col=2,lwd=2)
       }
   
       Plot1<-function(h,...) 
       {  par(mfrow=c(1,1))
          plot(D.data[,TIME.id],D.data[,DV.id],type='n',xlab="TIME",ylab="DV")
          id<-names(table(D.data[,ID.id]))
          for(i in 1:length(id))
          {   data<-D.data[which(D.data[,ID.id]==as.numeric(id[i])),]
              data<-data[!is.na(data[,DV.id]),]
              lines(data[,TIME.id],data[,DV.id])
          }
       }

       Plot2<-function(h,...) 
       {  p<-ceiling(sqrt(length(ID)))
          if(p<=5)
          {  par(mfrow=c(p,p))
             x.r<-c(min(D.data[,TIME.id],na.rm=T),max(D.data[,TIME.id],na.rm=T))
             y.r<-c(min(D.data[,DV.id],na.rm=T),max(D.data[,DV.id],na.rm=T))
             id<-names(table(D.data[,ID.id]))
             for(i in 1:length(id))
             {  data<-D.data[which(D.data[,ID.id]==as.numeric(id[i])),]
                data<-data[!is.na(data[,DV.id]),]
                plot(data[,TIME.id],data[,DV.id],xlim=x.r,ylim=y.r,
                     type='l', main=paste("ID: ",id[i]),xlab="TIME",ylab="DV")
                points(data[,TIME.id],data[,DV.id])
             }
          }
       }      
 
      IDlist <- gdroplist(ID,handler=updatePlot) 
      Button1<-gbutton("OK",handler=Plot1)
      Button2<-gbutton("OK",handler=Plot2)
   
      window <- gwindow("ID : DV vs Time")
      BigGroup <- ggroup(cont=window,anchor=c(-1,1))
      group<-ggroup(cont=BigGroup,horizontal=FALSE)
      tmp<-gframe("ID",container=group)
      add(tmp,IDlist)
      tmp<-gframe("All in one",container=group)
      add(tmp,Button1)
      tmp<-gframe("Individual groups",container=group)
      add(tmp,Button2,expand=TRUE)
      add(BigGroup, ggraphics())
   }

#### -- PK.plot
   PK.plot<-function(h,...)
   {   cov.list<-which(Var.Prop.Keep=="COV")
       COV.data<-as.matrix(D.data[,c(ID.id,cov.list)])
       colnames(COV.data)<-c("ID",Var.Name[cov.list])
       sample.data<-D.data
       sample.data[is.na(sample.data)]<-"NA"
       colnames(sample.data)<-Var.Name
       gtable(sample.data, cont=gwindow("Data"),do.subset=TRUE)

       updatePlot<-function(h,...)
       {  cond1.V<-svalue(VarList.g1)
          cond1.C<-svalue(VarType.g1)
          cond1.F<-as.numeric(svalue(From.id.g1))
          cond1.T<-as.numeric(svalue(To.id.g1))
          cond2.V<-svalue(VarList.g2)
          cond2.C<-svalue(VarType.g2)
          cond2.F<-as.numeric(svalue(From.id.g2))
          cond2.T<-as.numeric(svalue(To.id.g2))
          select.data<-D.data
          if(!is.na(cond1.V))
          {  if(!is.na(cond1.F))
             {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]>=cond1.F)
                select.data<-select.data[selected.id,]
             }
             if(!is.na(cond1.T))
             {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]<=cond1.T)
                select.data<-select.data[selected.id,]
             }
          }
          if(!is.na(cond2.V))
          {  if(!is.na(cond2.F))
             {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]>=cond2.F)
                select.data<-select.data[selected.id,]
             }
             if(!is.na(cond2.T))
             {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]<=cond2.T)
                select.data<-select.data[selected.id,]
             }
          }
          id<-names(table(select.data[,1]))
          if(length(id)!=0)
          {  data<-D.data[which(D.data[,ID.id]==id[1]),c(ID.id,DV.id,TIME.id)]
             data<-data[!is.na(data[,2]),]
             plot(data[,3],data[,2],type='l',xlab="TIME",ylab="Conc",
                xlim=range(D.data[,TIME.id],na.rm=T),ylim=range(D.data[,DV.id],na.rm=T))
             if(length(id)!=1)
                for(i in 2:length(id))
                {   data<-D.data[which(D.data[,ID.id]==id[i]),c(ID.id,DV.id,TIME.id)]
                    data<-data[!is.na(data[,2]),]
                    lines(data[,3],data[,2])
                }
          }
       }
    
       saveData<-function(h,...)
       {  cond1.V<-svalue(VarList.g1)
          cond1.C<-svalue(VarType.g1)
          cond1.F<-as.numeric(svalue(From.id.g1))
          cond1.T<-as.numeric(svalue(To.id.g1))
          cond2.V<-svalue(VarList.g2)
          cond2.C<-svalue(VarType.g2)
          cond2.F<-as.numeric(svalue(From.id.g2))
          cond2.T<-as.numeric(svalue(To.id.g2))
          select.data<-D.data
          if(!is.na(cond1.V))
          {  if(!is.na(cond1.F))
             {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]>=cond1.F)
                select.data<-select.data[selected.id,]
             }
             if(!is.na(cond1.T))
             {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]<=cond1.T)
                select.data<-select.data[selected.id,]
             }
          }
          if(!is.na(cond2.V))
          {  if(!is.na(cond2.F))
             {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]>=cond2.F)
                select.data<-select.data[selected.id,]
             }
             if(!is.na(cond2.T))
             {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]<=cond2.T)
                select.data<-select.data[selected.id,]
             }
          }
          id<-names(table(select.data[,1]))
          if(length(id)!=0)
          {  data<-NULL
             for(i in 1:length(id))
                data<-rbind(data,D.data[which(D.data[,ID.id]==id[i]),])
          } 
          write.csv(data,tclvalue(tkgetSaveFile()))
       }

       From.label.1<-glabel("From")
       To.label.1<-glabel("To")
       VarList.g1<-gdroplist(Var.Name)
       VarType.g1<-gradio(c("Category","Cont"))
       From.id.g1<-gedit(" ",width=10)
       To.id.g1<-gedit(" ",width=10)
    
       From.label.2<-glabel("From")
       To.label.2<-glabel("To")
       VarList.g2<-gdroplist(Var.Name)
       VarType.g2<-gradio(c("Category","Cont"))
       From.id.g2<-gedit(" ",width=10)
       To.id.g2<-gedit(" ",width=10)
       Button1<-gbutton("OK",handler=updatePlot)
       Button2<-gbutton("Save",handler=saveData)
    
       win<-gwindow("PK Explorer : Conc vs Time")
       BigGroup<-ggroup(cont=win)
       group<-ggroup(horizontal=FALSE,cont=BigGroup)
       tmp<-gframe("Condition 1 : variable",container=group)
       add(tmp,VarList.g1)
       add(tmp,VarType.g1)
       tmp<-gframe("Condition 1 : range",container=group)
       add(tmp,From.label.1);add(tmp,From.id.g1)
       add(tmp,To.label.1);add(tmp,To.id.g1)
       tmp<-gframe("Condition 1 : range",container=group)
       add(tmp,From.label.1);add(tmp,From.id.g1)
       add(tmp,To.label.1);add(tmp,To.id.g1)
    
       tmp<-gframe("Condition 2 : variable",container=group)
       add(tmp,VarList.g2)
       add(tmp,VarType.g2)
       tmp<-gframe("Condition 2 : range",container=group)
       add(tmp,From.label.2);add(tmp,From.id.g2)
       add(tmp,To.label.2);add(tmp,To.id.g2)
       tmp<-gframe("Plot",container=group)
       add(tmp,Button1,expand=TRUE)
       tmp<-gframe("Save",container=group)
       add(tmp,Button2,expand=TRUE)   
       add(BigGroup,ggraphics())
   }

#### -- PD.plot
   PD.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  cond.effect <-svalue(VarList.effect,index=T)
         cond.conc<-svalue(VarList.conc,index=T)
         cond.time<-svalue(VarList.time,index=T)
         select.data<-D.data
         if(!is.na(cond.effect)& !is.na(cond.conc)&!is.na(cond.time))
         {  effect<-select.data[,cond.effect]
            time<-select.data[,cond.time]
            conc<-ifelse(svalue(VarList.conc)=="None",NA,select.data[,cond.conc])
            if(is.na(conc))
            {  par(mfrow=c(1,1))
               plot(time,effect,xlab="effect",ylab="time")
            } else
            {  par(mfrow=c(1,2))
               ID.list<-names(table(select.data[,ID.id]))
               plot(conc,effect,xlab="effect",ylab="conc",type='n')
               for(i in 1:length(ID.list))
               {  select.id<-which(select.data[,ID.id]==as.numeric(ID.list[i]))
                  lines(effect[select.id],conc[select.id],lty=1)
               }
               plot(time,effect,xlab="effect",ylab="time")
            }  
         }
      }

      saveData<-function(h,...)
      {  cond.effect <-svalue(VarList.effect,index=T)
         cond.conc<-svalue(VarList.conc,index=T)
         cond.time<-svalue(VarList.time,index=T)
         if(svalue(VarList.conc)=="None")
         {  data<-D.data[,c(ID.id,cond.time,cond.effect)]
         } else
         {  data<-D.data[,c(ID.id,cond.time,cond.effect,cond.conc)]
         }
         write.csv(data,tclvalue(tkgetSaveFile()))
      }

      VarList.effect<-gdroplist(Var.Name)
      VarList.time<-gdroplist(Var.Name)
      VarList.conc<-gdroplist(c(Var.Name,"None"))
      Button1<-gbutton("OK",handler=updatePlot)
      Button2<-gbutton("Save",handler=saveData)

      win<-gwindow("PD Explorer : Effect Conc vs Time")
      BigGroup<-ggroup(cont=win)
      group<-ggroup(horizontal=FALSE,cont=BigGroup)
      tmp<-gframe(" Effect",container=group)
      add(tmp,VarList.effect)
      tmp<-gframe(" Time",container=group)
      add(tmp,VarList.time)
      tmp<-gframe(" Conc",container=group)
      add(tmp,VarList.conc)
      tmp<-gframe("Plot",container=group)
      add(tmp,Button1,expand=TRUE)
      tmp<-gframe("Save",container=group)
      add(tmp,Button2,expand=TRUE)
      add(BigGroup,ggraphics())
   }

#### -- runNONMEM
   parsing<-function()
   {  temp<-strsplit(Current.CTL,"")
      temp.keep<-NULL
      for(i in 1:length(temp))
      {  if(length(temp[[i]])!=0 &temp[[i]][1]!=";")
         {  temp.keep<-rbind(temp.keep,Current.CTL[i])
         }
       } 

   # remove comment
       temp<-strsplit(temp.keep,";")
       temp.keep<-NULL
       for(i in 1:length(temp))
          temp.keep<-rbind(temp.keep,temp[[i]][1])
       temp.CTL<-temp.keep

   # remove empty line
       temp<-strsplit(temp.CTL," ")
       temp.keep<-NULL
       for(i in 1:length(temp))
       {  if(sum(temp[[i]]=="")!=length(temp[[i]]))
          {  temp.keep<-rbind(temp.keep,temp.CTL[i])
          }
       }
       temp<-strsplit(temp.keep," ")
       id.input<-min(grep("\\$INPUT",temp))
       input.txt<-"$INPUT"
       for(i in 1:length(Var.Name)) 
          input.txt<-paste(input.txt,Var.Name[i],sep=" ")
       temp.keep[id.input,1]<-input.txt

       temp<-strsplit(temp.keep," ")
       id.data<-min(grep("\\$DATA",temp))
       data.txt<-paste("$DATA ",RunID,".csv",sep="")
       temp.keep[id.data,1]<-data.txt

       temp<-strsplit(temp.keep,";")
       temp.keep<-NULL
       for(i in 1:length(temp))
          temp.keep<-rbind(temp.keep,temp[[i]][1])
       temp.CTL<-temp.keep

   ## # of theta & eta
       flag<-TRUE
       i<-1
       sep.temp.CTL<-strsplit(temp.CTL," ")
       while(flag)
       {  ind<-grep(paste("ETA\\(",i,"\\)",sep=""),sep.temp.CTL)
          if(length(ind)!=0)
          {  for(j in ind)
             {  test<-strsplit(temp.CTL[j],split="")[[1]]
                s.id<-which(test=="E")
                for(k in s.id)
                {  test.text<-test[k]
                   for(l in 1:5)
                      test.text<-paste(test.text,test[k+l],sep="")
                   if(test.text==paste("ETA(",i,")",sep=""))
                   {  if(paste(test[k-2],test[k-1],sep="")=="TH")
                      {  n.theta<-i
                      } else
                      { n.eta<-i 
                      }
                   }
                } 
             }     
             i<-i+1
          } else
          {  flag<-FALSE
          }
       }   
       N.theta<<-n.theta
       N.eta<<-n.eta
   
   ## # of epsillon
       flag<-TRUE
       i<-1
       while(flag)
       {  if(length(grep(paste("EPS\\(",i,"\\)",sep=""),sep.temp.CTL)!=0))
          {  i<-i+1
          } else
          {  flag<-FALSE
          }
       }
       N.eps<<-i-1

       if(N.eta==0)
       {  flag<-TRUE 
          while(flag)
          {  if(length(grep(paste(" ETA\\(",i,"\\)\\)",sep=""),temp.CTL)!=0))
             {   i<-i+1
             } else
             { flag<-FALSE
             }
          }
          N.eta<<-i-1
       }
       if(N.eta==0)
       {  flag<-TRUE
          while(flag)
          {  if(length(grep(paste("\\+ETA\\(",i,"\\)",sep=""),temp.CTL)!=0))
             {  i<-i+1
             } else
             {  flag<-FALSE
             }
          }
          N.eta<<-i-1
       }
       Current.CTL<<-temp.CTL
   }
##

   add.CTL<-function() 
   {  temp.CTL<-Current.CTL
    # set Method.flag / inter.flag / posthoc.flag
      temp<-strsplit(temp.CTL," ")
      id.est<-min(grep("\\$EST",temp))
      temp<-unlist(strsplit(strsplit(temp.CTL[id.est],"=")[[1]]," "))
      INTER.flag<<-ifelse(sum(temp=="INTER")!=0,TRUE,FALSE)
      FO.flag<<-ifelse(sum(temp=="0" | temp=="ZERO")!=0,TRUE,FALSE)
      FOCE.flag<<-ifelse(sum(temp=="1"| temp=="COND")!=0,TRUE,FALSE)
      POSTHOC.flag<<-ifelse(sum(temp=="POSTHOC")!=0,TRUE,FALSE)

    ### $DATA 다음에 $ABBREVIATED DERIV2=NO COMRES=4;5 삽입
      temp<-strsplit(temp.CTL,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
         indicator<-rbind(indicator,temp[[i]][1])
      n.CTL<-length(temp.CTL)

      line1<-paste("$ABBREVIATED DERIV2=NO COMRES=",N.eta+N.eps,sep="")
      id<-which(indicator=="$DATA")
      temp.CTL[id]<-paste("$DATA ",RunID,".csv ",sep="")
      temp.CTL<-c(temp.CTL[1:id],line1,temp.CTL[(id+1):n.CTL])

   ## add WRITE 
      temp<-strsplit(temp.CTL,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
         indicator<-rbind(indicator,temp[[i]][1])
      n.CTL<-length(temp.CTL)

      line2<-c("  IF(ICALL.EQ.3) THEN;",    
               "     WRITE (52,*) OBJECT",
               "     WRITE (53,*) THETA",
               "     WRITE (53,*) SETHET",
               "     WRITE (54,*) OMEGA(BLOCK)",
               "     WRITE (54,*) SEOMEG(BLOCK)",
               "     WRITE (55,*) SIGMA(BLOCK)",
               "     WRITE (55,*) SESIGM(BLOCK)",
               "     DO WHILE(DATA)",
               "       IF(NEWIND.LE.1) WRITE(56,*) ETA",
               "     ENDDO",
               "  ENDIF","",
               "\"LAST")
      com.id<-1
      for(i in 1:N.eta)                          
      {  line2<-c(line2, paste("\" COM(",com.id,") = G(",i,",1)",sep=""))
         com.id<-com.id+1
      }
      for(i in 1:N.eps)                          
      {  line2<-c(line2, paste("\" COM(",com.id,") = H(",i,",1)",sep=""))
         com.id<-com.id+1
      }
      id<-which(indicator=="$THETA")
      temp.CTL<-c(temp.CTL[1:(id-1)],line2,temp.CTL[id:n.CTL])

    ## add POSTHOC
      temp<-strsplit(temp.CTL,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
         indicator<-rbind(indicator,temp[[i]][1])
      n.CTL<-length(temp.CTL)
      id<-which(indicator=="$EST"| indicator=="$ESTIMATION")
      temp.CTL[id]<-"$ESTIMATION METHOD=0 POSTHOC"   

   ## add Table
      line3<-c(paste("$TABLE ID TIME  IPRE FILE=",RunID,".FIT NOPRINT ONEHEADER",sep=""))
      line4<-"$TABLE ID TIME " 
      line5<-"$TABLE ID TIME " 
      comres<-1
      for(i in 1:N.eta)
      {  line4<-paste(line4,"COM(",comres,")=G",i,"1 ",sep="")
         line5<-paste(line5,"ETA(",comres,") ",sep="")
         comres<-comres+1
      }
      for(i in 1:N.eps)
      {  line4<-paste(line4,"COM(",comres,")=H",i,"1 ",sep="")
         comres<-comres+1
      }
      line4<-paste(line4,"\n      FILE=",RunID,".DR NOPRINT ONEHEADER",sep="")
      line5<-paste(line5,"\n      FILE=",RunID,".ETA NOPRINT ONEHEADER",sep="")
      temp.CTL<-c(temp.CTL,line3,line4)
      NONMEM.CTL<<-temp.CTL
   }

##
   rm.CTL<-function()
   {  ETA<<-read.table("fort.56",header=FALSE)
      temp<-read.table(paste(RunID,".DR",sep=""),header=T,skip=1)
      G.p<<-as.matrix(temp[,(1:N.eta)+2])
      H.p<<-as.matrix(temp[,(1:N.eps)+(2+N.eta)])

      temp.CTL<-NONMEM.CTL
      temp<-strsplit(temp.CTL,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
         indicator<-rbind(indicator,temp[[i]][1])
      n.CTL<-length(temp.CTL)

      temp.CTL2<-Current.CTL
      temp2<-strsplit(temp.CTL2,split=" ")
      indicator2<-NULL
      for(i in 1:length(temp2))
         indicator2<-rbind(indicator2,temp2[[i]][1])
      n.CTL2<-length(temp.CTL2)

      id<-which(indicator=="$EST"| indicator=="$ESTIMATION")
      id2<-which(indicator2=="$EST"| indicator2=="$ESTIMATION")
      temp.CTL[id]<-temp.CTL2[id2]
      NONMEM.CTL<<-temp.CTL
   }

##
   NONMEM.output<-function()
   {  OBJ<<-read.table("fort.52")
      temp<-read.table("fort.53")
      THETA<<-temp[1,]
      seTHETA<<-temp[2,]
      temp<-read.table("fort.54")
      OMEGA<<-as.matrix(temp[1:N.eta,])
      seOMEGA<<-as.matrix(temp[(N.eta+1):nrow(temp),])
      temp<-read.table("fort.55")
      SIGMA<<-as.matrix(temp[1:N.eps,])
      seSIGMA<<-as.matrix(temp[(N.eps+1):nrow(temp),])

      temp<-read.table(paste(RunID,".DR",sep=""),header=T,skip=1)
      G<<-as.matrix(temp[,(1:N.eta)+2])
      H<<-as.matrix(temp[,(1:N.eps)+(2+N.eta)])
      temp<-read.table(paste(RunID,".FIT",sep=""),header=T,skip=1)
      NM.FIT<<-temp
      Y<-as.matrix(NM.FIT$DV)
      F<-as.matrix(NM.FIT$PRED)
      n<-nrow(NM.FIT)

     ## individual OFV / CWRES 계산
      sqrtcov<-function(x)
      {  evx <- eigen(as.matrix(x))
         return(evx$vectors %*% diag(sqrt(evx$values)) %*% t(evx$vectors))
      }
      id.table<-names(table(NM.FIT$ID))
      RES.tot<-NULL
      for(i in 1:length(id.table))
      {  id.select<-which(NM.FIT$ID==id.table[i])
         G.i <- as.matrix(G[id.select,])
         H.i <- as.matrix(H[id.select,])
         RES.i<-NM.FIT$RES[id.select]
         Y.i<-NM.FIT$DV[id.select]
         PRED.i<-NM.FIT$PRED[id.select]
         COV  <- G.i %*% OMEGA %*% t(G.i) + diag(diag(H.i %*% SIGMA %*% t(H.i)))
         WRES.i <- sqrtcov(solve(COV)) %*% RES.i
         if(FO.flag)
         {  CWRES.i<-WRES.i
         } else
         {  IPRE.i<-NM.FIT$IPRE[id.select]
            G1.i <- as.matrix(G.p[id.select,])
            H1.i <- as.matrix(H.p[id.select,])
            ETA.i<-t(as.matrix(ETA[i,]))
            IRES.i<-Y.i-IPRE.i
            if(INTER.flag)
            {  COV<-G1.i %*% OMEGA %*% t(G1.i) + diag(diag(H1.i %*% SIGMA %*% t(H1.i)))
               CWRES.i<-sqrtcov(solve(COV))%*%(IRES.i+G1.i%*%ETA.i)
            } else
            {  COV<-G1.i %*% OMEGA %*% t(G1.i) + diag(diag(H.i %*% SIGMA %*% t(H.i)))
               CWRES.i<-sqrtcov(solve(COV))%*%(IRES.i+G1.i%*%ETA.i)
            }   
         }
         RES.tot<-rbind(RES.tot,cbind(as.numeric(id.table[i]),WRES.i,CWRES.i))
      }
   
     ## eta shrinkage : POPULATION ## FO에서는 POSTHOC 필요
      pop.eta.shrinkage<-sd(ETA)/sqrt(diag(OMEGA))*100

     ## epsillon shrinkage 
      Sigma.sh<-NA

     ### %RSE 
      RSE.THETA<-seTHETA/THETA*100
      RSE.OMEGA<-seOMEGA/OMEGA*100
      RSE.SIGMA<-seSIGMA/SIGMA*100

     ### CI
      L.THETA<-THETA-1.96*seTHETA
      U.THETA<-THETA+1.96*seTHETA
      L.OMEGA<-OMEGA-1.96*seOMEGA
      U.OMEGA<-OMEGA+1.96*seOMEGA
      L.SIGMA<-SIGMA-1.96*seSIGMA
      U.SIGMA<-SIGMA+1.96*seSIGMA

     ### %CV 
      if(FO.flag)
      {  CV.OMEGA<-sqrt(diag(OMEGA))*100
      } else
      {  CV.OMEGA<-sqrt(exp(diag(OMEGA))-1)*100
      } 

     ### AIC (???)
      block.eta<-TRUE
      block.eps<-FALSE
      p<-N.theta+ifelse(block.eta,N.eta*(N.eta+1)/2,N.eta)+
              ifelse(block.eps,N.eps*(N.eps+1)/2,N.eps)
      AIC.nm<<-OBJ+2*p 
      BIC.nm<<-OBJ+p*log(n)
 
     ### Summary Output !!
      CWRES<-RES.tot[,3]
      Output.Table<<-cbind(NM.FIT,CWRES)
      THETA.tot<-cbind(t(THETA),t(seTHETA),t(RSE.THETA),t(L.THETA),t(U.THETA),NA,NA)
        rownames(THETA.tot)<-paste("THETA",1:N.theta,sep=" ")

     if(block.eta)
     {  id.1<-NULL
        OMEGA.tot<-NULL
        for(i in 1:N.eta)
        for(j in i:N.eta)
        {  if(i!=j)
           {  OMEGA.tot<-rbind(OMEGA.tot,c(OMEGA[i,j],seOMEGA[i,j],
                    RSE.OMEGA[i,j],L.OMEGA[i,j],U.OMEGA[i,j],NA,NA))
           } else
           {  OMEGA.tot<-rbind(OMEGA.tot,c(OMEGA[i,j],seOMEGA[i,j],
                    RSE.OMEGA[i,j],L.OMEGA[i,j],U.OMEGA[i,j],pop.eta.shrinkage[i],CV.OMEGA[i]))
           } 
           id.1<-rbind(id.1,c(i,j))
        }
        rownames(OMEGA.tot)<-paste("OMEGA ",id.1[,1],",",id.1[,2],sep="")
     } else
     {  OMEGA.tot<-cbind(matrix(c(OMEGA),length(OMEGA)),matrix(c(seOMEGA),length(seOMEGA)),
                 matrix(c(RSE.OMEGA),length(RSE.OMEGA)),matrix(c(L.OMEGA),length(L.OMEGA)),
                 matrix(c(U.OMEGA),length(U.OMEGA)),
                 matrix(c(pop.eta.shrinkage),length(pop.eta.shrinkage)),
                 matrix(c(CV.OMEGA),length(CV.OMEGA)))
        rownames(OMEGA.tot)<-paste("OMEGA ",1:N.eta,",",1:N.eta,sep="")
     }
     if(N.eps==2)
     {  id.1<-NULL
        SIGMA.tot<-NULL
        for(i in 1:N.eps)
        for(j in i:N.eps)
        {  if(i!=j)
           {  SIGMA.tot<-rbind(SIGMA.tot,c(SIGMA[i,j],seSIGMA[i,j],
                    RSE.SIGMA[i,j],L.SIGMA[i,j],U.SIGMA[i,j],NA,NA))
           } else
           {  SIGMA.tot<-rbind(SIGMA.tot,c(SIGMA[i,j],seSIGMA[i,j],
                    RSE.SIGMA[i,j],L.SIGMA[i,j],U.SIGMA[i,j],Sigma.sh[i],NA))
           }
           id.1<-rbind(id.1,c(i,j))
        }
        rownames(SIGMA.tot)<-paste("SIGMA ",id.1[,1],",",id.1[,2],sep="")
     } else
     {  SIGMA.tot<-cbind(matrix(c(SIGMA),length(SIGMA)),matrix(c(seSIGMA),length(seSIGMA)),
                 matrix(c(RSE.SIGMA),length(RSE.SIGMA)),matrix(c(L.SIGMA),length(L.SIGMA)),
                 matrix(c(U.SIGMA),length(U.SIGMA)),matrix(c(Sigma.sh),length(Sigma.sh)),
                 NA)
        rownames(SIGMA.tot)<-paste("SIGMA ",1:N.eps,",",1:N.eps,sep="")
     }   
     colnames(THETA.tot)<-c("Estimation","SE","%RSE","Lower","Upper","%Shrinkage","%CV")
     colnames(OMEGA.tot)<-c("Estimation","SE","%RSE","Lower","Upper","%Shrinkage","%CV")
     colnames(SIGMA.tot)<-c("Estimation","SE","%RSE","Lower","Upper","%Shrinkage","%CV")

     EST.tot<<-rbind(THETA.tot,OMEGA.tot,SIGMA.tot)
   }
##
   parsing.LST<-function()
   {  temp.id<-which(D.LST==" ************************************************************************************************************************")
      temp.LST<-D.LST[1:temp.id[1]]
      temp<-strsplit(temp.LST,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
         indicator<-rbind(indicator,temp[[i]][1])
      cov.id<-which(indicator=="0COVARIANCE")
      min.id<-which(indicator=="0MINIMIZATION")
      Cov.desc<<-ifelse(strsplit(D.LST[cov.id],split="  ")[[1]][3]=="NO ","YES","NO")
      Min.desc<<-ifelse(strsplit(D.LST[min.id],split=" ")[[1]][2]=="SUCCESSFUL","YES","NO")
   }

##
   runNONMEM<-function(h,...)
   {  ginput("Enter your model Description",title="Model Description Input",
             text=" ", handler=function(h,...){ Model.desc<<-h$input})
      RunID<<-RunID+100
      Cur.Run<<-Cur.Run+1
      parsing()

      file.name<-paste(RunID,".CTL",sep="")
      file.name.d<-paste(RunID,".csv",sep="")
      add.CTL()
      write.table(NONMEM.CTL,file.name,quote=FALSE,row.names=FALSE,col.names=FALSE)
      write.table(D.data,file.name.d,quote=FALSE,row.names=FALSE,col.names=FALSE,na=".",sep=",")
      NONMEM.run<-paste("nmfe6",file.name, paste(RunID,"out",sep="."))
      system(NONMEM.run)
      system("nonmem.exe")
      rm.CTL()
      write.table(NONMEM.CTL,file.name,quote=FALSE,row.names=FALSE,col.names=FALSE)
      write.table(D.data,file.name.d,quote=FALSE,row.names=FALSE,col.names=FALSE,na=".",sep=",")
      NONMEM.run<-paste("nmfe6",file.name, paste(RunID,"out",sep="."))
      system(NONMEM.run)
      system("nonmem.exe")
    
      D.LST<<-readLines("output")

      temp.nonmem<-1:11
      temp.nonmem[1]<-RunID
      temp.nonmem[2]<-readLines(paste(RunID,"OUT",sep="."))[1]
      temp.nonmem[3]<-readLines(paste(RunID,"OUT",sep="."))[2]
      write.table(D.LST,paste(RunID,"OUT",sep="."),quote=FALSE,row.names=FALSE,col.names=FALSE)
      NONMEM.output()
      parsing.LST()
      temp.nonmem[6]<-c(round(OBJ[1,1],2))
      temp.nonmem[7]<-c(round(AIC.nm[1,1],2))
      temp.nonmem[8]<-c(round(BIC.nm[1,1],2))
      temp.nonmem[11]<-Model.desc
      temp.nonmem[9]<-file.name
      temp.nonmem[10]<-file.name.d
      temp.nonmem[4]<-Min.desc
      temp.nonmem[5]<-Cov.desc
   
      nonmem.run<<-temp.nonmem
      run.table[][Cur.Run,]<-temp.nonmem
    # from D.TAB
      D.TAB<<-NM.FIT
      temp.list<-colnames(D.TAB)
      PRED.id<<-which(temp.list=="PRED")
      RES.id<<-which(temp.list=="RES")
      WRES.id<<-which(temp.list=="WRES")
      write.table(EST.tot,paste(RunID,"PAR",sep="."),quote=FALSE,row.names=FALSE)
      write.table(cbind(G,H),paste(RunID,"GH",sep="."),quote=FALSE,row.names=FALSE)
      write.table(Output.Table,paste(RunID,"RES",sep="."),quote=FALSE,row.names=FALSE)   
   }

#### -- show.parameters
   show.parameters<-function(h,...)
   {  show.data<-cbind(rownames(EST.tot),round(EST.tot,3))
      show.data[is.na(show.data)]<-"."
      colnames(show.data)[1]<-"parameters"
      win<-gwindow("Parameter Estimation",width=600,height=300)
      gtable(show.data, cont=win,do.subset=TRUE)
   }

#### -- show.GandH
   show.GandH<-function(h,...)
   {  GH<-cbind(D.data,G,H)
      colnames(GH)<-c(colnames(D.data),paste("G",1:N.eta,sep=""),paste("H",1:N.eps,sep=""))
      GH[is.na(GH)]<-"."
      gtable(GH, cont=gwindow("G and H"),do.subset=TRUE)
   }

#### -- show.FittedResidual
   show.FittedResidual<-function(h,...)
   {  gtable(Output.Table, cont=gwindow("Fitted and Residual"),do.subset=TRUE)
   }

#### -- show.LST
   show.LST<-function(h,...)
   {  nm.LST<-readLines(paste(RunID,".out",sep=""))
      nm.print<-nm.LST[1]
      for(i in 2:length(nm.LST))
        nm.print<-paste(nm.print,nm.LST[i],sep="\n")

      edit.win<-gwindow("Edit Control File")
      g<-ggroup(horizontal=FALSE,cont=edit.win)
      tmp<-gframe("LST file",container=g)
      a<-gtext(nm.print,width=1050,height=500,font.attr=c(sizes="large",family="monospace"))
      add(tmp,a)
   }

#### -- DVvsPRED.plot
   DVvsPRED.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  condX.V <-svalue(VarList.X)
         condY.V<-svalue(VarList.Y)
         select.data<-Output.Table
         if(!is.na(condX.V)& !is.na(condY.V))
         {  X<-select.data[,condX.V]
            Y<-select.data[,condY.V]
            plot(X,Y,xlab=condX.V,ylab=condY.V)
            abline(a=0,b=1,col=2)
         }
      }
      VarList.X<-gdroplist("DV")
      if(sum(colnames(NM.FIT)=="IPRE")!=0)
      {  VarList.Y<-gdroplist(c("PRED","IPRE"))
      } else
      {  VarList.Y<-gdroplist(c("PRED"))
      }
      Button<-gbutton("OK",handler=updatePlot)
    
      win<-gwindow("DV vs PRED plot")
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
      tmp=gframe(" X variable",container=group)
      add(tmp,VarList.X)
      tmp=gframe(" Y variable",container=group)
      add(tmp,VarList.Y)

      tmp=gframe("Plot",container=group)
      add(tmp,Button,expand=TRUE)
      add(BigGroup,ggraphics())
   }

#### -- DVvsRES.plot
   DVvsRES.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  par(mfrow=c(1,1))
         condX.V <-svalue(VarList.X)
         condY.V<-svalue(VarList.Y)
         select.data<-Output.Table
         if(!is.na(condX.V)& !is.na(condY.V))
         {  X<-select.data[,condX.V]
            Y<-select.data[,condY.V]
            plot(X,Y,xlab=condX.V,ylab=condY.V)
            abline(h=0,col=2)
         }
      }

      Plot1<-function(h,...) 
      {  par(mfrow=c(1,2))
         plot(Output.Table$DV,Output.Table$RES,xlab="DV",ylab="RES")
         abline(h=0,col=2)
         if(FO.flag)
         {  plot(Output.Table$DV,Output.Table$WRES,xlab="DV",ylab="WRES")
         } else
         {  plot(Output.Table$DV,Output.Table$CWRES,xlab="DV",ylab="CWRES")
         }       
         abline(h=0,col=2)
      }
    
      if(FO.flag)
      {  VarList.Y<-gdroplist(c("RES","WRES"))
      } else
      {  VarList.Y<-gdroplist(c("RES","CWRES"))
      }   
      VarList.X<-gdroplist(c("DV"))  
      Button1<-gbutton("OK",handler=updatePlot)
      Button2<-gbutton("OK",handler=Plot1)
    
      win<-gwindow("DV vs RES plot")
      BigGroup<-ggroup(cont=win)
      group<-ggroup(horizontal=FALSE,cont=BigGroup)
      tmp<-gframe(" X variable",container=group)
      add(tmp,VarList.X)
      tmp<-gframe(" Y variable",container=group)
      add(tmp,VarList.Y)
      tmp<-gframe("Plot",container=group)
      add(tmp,Button1,expand=TRUE)
      tmp<-gframe("All Plots",container=group)
      add(tmp,Button2,expand=TRUE) 
      add(BigGroup,ggraphics())
   }

#### -- TIMEvsRES.plot
   TIMEvsRES.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  condX.V <-svalue(VarList.X)
         condY.V<-svalue(VarList.Y)
         select.data<-Output.Table
         if(!is.na(condX.V)& !is.na(condY.V))
         {  X<-select.data[,condX.V]
            Y<-select.data[,condY.V]
            plot(X,Y,xlab=condX.V,ylab=condY.V,type='n')
            id<-as.numeric(names(table(Output.Table$ID)))
            for(i in 1:length(id))
            {  X.data<-X[which(select.data$ID==id[i])]
               Y.data<-Y[which(select.data$ID==id[i])]
               lines(X.data,Y.data,lty=2)
            }
            abline(h=0,col=2)
         }
      }

      if(FO.flag)
      {  VarList.Y<-gdroplist(c("RES","WRES"))
      } else
      {  VarList.Y<-gdroplist(c("RES","CWRES"))
      }   
      VarList.X<-gdroplist(c("TIME"))
      Button<-gbutton("OK",handler=updatePlot)
    
      win<-gwindow("TIME vs RES plot")
      BigGroup<-ggroup(cont=win)
      group<-ggroup(horizontal=FALSE,cont=BigGroup)
      tmp<-gframe(" X variable",container=group)
      add(tmp,VarList.X)
      tmp<-gframe(" Y variable",container=group)
      add(tmp,VarList.Y)
      tmp<-gframe("Plot",container=group)
      add(tmp,Button,expand=TRUE)
      add(BigGroup,ggraphics())
   }

#### -- TIMEvsDVandPRED.plot
   TIMEvsDVandPRED.plot<-function(h,...)
   {  ID<-sort(unique(D.data[,ID.id]))
      ID<-matrix(ID,nrow=length(ID))
      colnames(ID)<-c("ID")

      updatePlot<-function(h,...) 
      {  id<-svalue(IDlist)
         data<-Output.Table[which(Output.Table$ID==id[1]),]
         data<-data[!is.na(data$DV),]
         par(mfrow=c(1,1))
         plot(data$TIME,data$DV,type='l',xlab="TIME",ylab="Conc",
            xlim=range(Output.Table$TIME,na.rm=T),
            ylim=range(c(Output.Table$DV,Output.Table$PRED),na.rm=T),
            main=paste("ID:",id))
         lines(data$TIME,data$PRED,lty=1,col=4)
         lines(data$TIME,data$IPRE,lty=1,col=2)       
      }
   
      Plot1<-function(h,...) 
      {  par(mfrow=c(1,1))
         plot(Output.Table[,TIME.id],Output.Table[,DV.id],type='n',xlab="TIME",ylab="DV")
         id<-names(table(Output.Table[,ID.id]))
         for(i in 1:length(id))
         {  data<-Output.Table[which(Output.Table[,ID.id]==as.numeric(id[i])),]
            lines(data$TIME,data$DV,lty=1,col=1)
            lines(data$TIME,data$PRED,lty=1,col=4)
            lines(data$TIME,data$IPRE,lty=1,col=2)
         }
      }

      Plot2<-function(h,...) 
      {  p<-ceiling(sqrt(length(ID)))
         if(p<=5)
         {  par(mfrow=c(p,p))
            x.r<-c(min(Output.Table[,TIME.id],na.rm=T),max(Output.Table[,TIME.id],na.rm=T))
            y.r<-c(min(Output.Table[,DV.id],na.rm=T),max(Output.Table[,DV.id],na.rm=T))
            for(i in 1:length(ID))
            {   data<-Output.Table[which(Output.Table[,ID.id]==as.numeric(ID[i])),]
                plot(data$TIME,data$DV,type='l',xlab="TIME",ylab="Conc",
                   xlim=range(Output.Table$TIME,na.rm=T),
                   ylim=range(c(Output.Table$DV,Output.Table$PRED),na.rm=T),
                   main=paste("ID:",i))
                lines(data$TIME,data$PRED,lty=1,col=4)
                lines(data$TIME,data$IPRE,lty=1,col=2)
            }
         }
      }   
    
      IDlist <- gdroplist(ID,handler=updatePlot) 
      Button1<-gbutton("OK",handler=Plot1)
      Button2<-gbutton("OK",handler=Plot2)
   
      window <- gwindow("ID : Conc vs Time")
      BigGroup <- ggroup(cont=window,anchor=c(-1,1))
      group<-ggroup(cont=BigGroup,horizontal=FALSE)
      tmp<-gframe("ID",container=group)
      add(tmp,IDlist)
      tmp<-gframe("All in one",container=group)
      add(tmp,Button1)
      tmp<-gframe("Individual groups",container=group)
      add(tmp,Button2,expand=TRUE)
      tmp<-gframe("DV   : black",container=group)
      tmp<-gframe("PRED : blue",container=group)
      tmp<-gframe("IPRE : red",container=group)  
      add(BigGroup, ggraphics())
   }

#### -- EBEvsCOV.plot1
   EBEvsCOV.plot<-function(h,...)
   {  ETA.list<-NULL
      for(i in 1:N.eta)
         ETA.list<-c(ETA.list,paste("ETA(",i,")",sep=""))
       COV.list<-which(Var.Prop.Keep=="COV")
       select.data<-as.matrix(D.data[,COV.list])
       select.data<-select.data[!is.na(select.data[,1]),]   
       win<-gwindow("COV vs ETA plot")
       BigGroup<-ggroup(cont=win)
       add(BigGroup,ggraphics())
       graph.data<-cbind(select.data,ETA)
       name.list<-c(Var.Name[COV.list],ETA.list)
       panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
       {  usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- abs(cor(x, y))
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if(missing(cex.cor)) cex.cor <- 1#0.5/strwidth(txt)
          text.col<-ifelse(r>0.95,2,1)
          text(0.5, 0.5, txt, cex = cex.cor,col=text.col)# * r)
       }
       pairs(graph.data,labels=name.list, lower.panel=panel.smooth, upper.panel=panel.cor)
   }
#### -- Boot
   Boot.ctl<-function()
   {  temp.CTL<-Current.CTL
      temp<-strsplit(temp.CTL,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
        indicator<-rbind(indicator,temp[[i]][1])
      n.CTL<-length(temp.CTL)

      line2<-c("  IF(ICALL.EQ.3) THEN;",    
               "     WRITE (52,*) OBJECT",
               "     WRITE (53,*) THETA",
               "     WRITE (54,*) OMEGA(BLOCK)",
               "     WRITE (55,*) SIGMA(BLOCK)",
               "     DO WHILE(DATA)",
               "       IF(NEWIND.LE.1) WRITE(56,*) ETA",
               "     ENDDO",
               "  ENDIF","",
               "\"LAST")
      id<-which(indicator=="$DATA")
      temp.CTL[id]<-"$DATA boot.csv"
      id<-which(indicator=="$TABLE")
      temp.CTL<-temp.CTL[1:(id[1]-1)]
      id<-which(indicator=="$THETA")
      temp.CTL<-c(temp.CTL[1:(id-1)],line2,
                   temp.CTL[id:length(temp.CTL)])
      Boot.CTL<<-temp.CTL
   }
##
   show.BTsummary<-function()
   {  Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,mean))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,sd))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,min))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,function(x){quantile(x,probs=0.025)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,function(x){quantile(x,probs=0.25)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,function(x){quantile(x,probs=0.5)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,function(x){quantile(x,probs=0.75)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,function(x){quantile(x,probs=0.975)}))  
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep,2,max))
      Boot.summary<-t(Boot.summary)
      colnames(Boot.summary)<-c("Est","mean","sd","min","2.5%","25%","50%","75%","97.5%","max")
      o.id<-NULL
      for(i in 1:N.eta)
      for(j in i:N.eta)
        o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*(N.eta-1)+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
      paste("OMEGA(",o.id[,1],",",o.id[,2],")",sep=""),  
      paste("SIGMA(",1:N.eps,",",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(rownames(s.Boot.summary),round(s.Boot.summary,3))
      colnames(s.Boot.summary)<-c("Parameter",colnames(Boot.summary))
      win<-gwindow("Bootstrap Summary",width=600,height=300)
      gtable(s.Boot.summary,cont=win)
   }

##
   Boot.try<-function()
   {  ID.list<-as.numeric(names(table(D.data[,ID.id])))
      Boot.tot<-NULL
      win<-gwindow(paste("Bootstrap Progress : B=",B,sep=""),width=300,height=50)
      Boot.progress<-gslider(from=0,to=B,by=1,value=0,cont=win)
 
      for(b in 1:B)
      {  svalue(Boot.progress)<-b
         Boot.sample.id<-sample(ID.list,length(ID.list),replace=T)
         Boot.sample<-NULL
         for(i in 1:length(Boot.sample.id))
         {  id.temp<-which(D.data[,ID.id]==Boot.sample.id[i])
            id.temp<-D.data[id.temp,]
            id.temp$ID<-i
            Boot.sample<-rbind(Boot.sample,id.temp)
         }
         write.table(Boot.sample,"boot.csv",quote=FALSE,row.names=FALSE,col.names=FALSE,na=".",sep=",")
         system("nmfe6 boot.ctl boot.lst")
         system("nonmem.exe")
         THETA.t<-read.table("fort.53")
         OMEGA.t<-read.table("fort.54")
         SIGMA.t<-read.table("fort.55")
         tot<-c(unlist(THETA.t),unlist(OMEGA.t),unlist(SIGMA.t))
         Boot.tot<-rbind(Boot.tot,tot)
      }
      dispose(win) 
      Boot.keep<<-Boot.tot

      BootPlot<-function(h,...)
     {  if(svalue(Par.list)=="THETA")
        {  par(mfrow=c(1,N.theta))
           for(i in 1:N.theta)
           {  hist(Boot.tot[,i],main=paste("THETA(",i,")",sep=""),
                    xlab=paste("THETA(",i,")",sep=""))
              abline(v=THETA[i],col=2,lwd=2)
           }
        } else if(svalue(Par.list)=="OMEGA")
        {  par(mfrow=c(N.eta,N.eta))
           for(i in 1:N.eta)
           for(j in 1:N.eta)
           {  id<-N.theta+i+j
              hist(Boot.tot[,id],main=paste("OMEGA(",i,",",j,")",sep=""),
                     xlab=paste("OMEGA(",i,",",j,")",sep=""))
              abline(v=OMEGA[i,j],col=2,lwd=2)
           } 
        } else 
        {  par(mfrow=c(1,N.eps))
           for(i in 1:N.eps)
           {  id<-N.theta+N.eta*N.eta+i*i
              hist(Boot.tot[,id],main=paste("SIGMA(",i,",",i,")",sep=""),
                    xlab=paste("SIGMA(",i,",",i,")",sep=""))
              abline(v=SIGMA[i,i],col=2,lwd=2)
           }
        }
     }
     show.BTsummary()
     win<-gwindow("Bootstrap ")
     BigGroup<-ggroup(cont=win)
     group<-ggroup(horizontal=FALSE,cont=BigGroup)
     Par.list<-gdroplist(c("THETA","OMEGA","SIGMA"))
     Button<-gbutton("OK",handler=BootPlot)
     tmp<-gframe("Parameter",cont=group)
     add(tmp,Par.list)
     tmp<-gframe("Plot",cont=group)
     add(tmp,Button)
     add(BigGroup,ggraphics()) 
   }

##
   Boot.B.init<-function()
   {  setB<-function(h,...)
      {  B<<-as.numeric(svalue(Boot.N))
         dispose(win)
         Boot.try()
      }
      win<-gwindow("Bootstrap",width=100,height=20)
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
      tmp=gframe("Number of Bootstrap samples",container=group)
      Boot.label<-glabel("B")
      Boot.N<-gedit(" ",width=10)
      Button<-gbutton("OK",handler=setB)
      add(tmp,Boot.label)
      add(tmp,Boot.N)
      add(tmp,Button)
   }

##
   Boot<-function(h,...)
   {  Boot.ctl()
      write.table(Boot.CTL,"boot.ctl",quote=FALSE,row.names=FALSE,col.names=FALSE)
      Boot.B.init()
   }

#### -- VPC
   VPC.try<-function()
   {  temp<-strsplit(Current.CTL,split=" ")
      indicator<-NULL
      for(i in 1:length(temp))
        indicator<-rbind(indicator,temp[[i]][1])
      VPC.CTL<-Current.CTL[1:(which(indicator=="$EST" | indicator=="$ESTIMATION")-1)]
      temp.ctl1<-paste("$SIMULATION (20030521) ONLYSIM SUBPROBLEMS=",VPC.N,sep="")
      temp.ctl2<-"$TABLE ID TIME  FILE=VPC.SIM"
      VPC.CTL<-c(VPC.CTL,temp.ctl1,temp.ctl2)
      write.table(VPC.CTL,"VPC.CTL",quote=FALSE,row.names=FALSE,col.names=FALSE)
      system("nmfe6 VPC.CTL VPC.LST")
      system("nonmem.exe")

      sim.data<-readLines("VPC.SIM")
      colnames(D.data)<-Var.Name
      n.sim<-VPC.N
      n.data<-nrow(D.data)
      sim.data<-sim.data[-c(1,(1:(n.sim-1))*(n.data+2)+1,(1:(n.sim-1))*(n.data+2)+2)]
      temp<-unlist(strsplit(sim.data,split=" "))
      temp<-temp[temp!=""] 
      sim.name<-temp[1:6]
      sim.mat<-matrix(as.numeric(temp[-c(1:6)]),nrow=n.sim*n.data,byrow=T)
      colnames(sim.mat)<-sim.name
      sim.mat<-data.frame(sim.mat)

      sim.tot<-list()
      Quantile.tot<-NULL
      for(i in 1:n.data)
      {  temp.list<-list()
         temp.id<-which(sim.mat$ID==sim.mat$ID[i] & sim.mat$TIME==sim.mat$TIME[i])
         sort.DV<-sort(sim.mat$DV[temp.id])
         Q025<-sort.DV[round(n.sim*0.025)]
         Q05<-sort.DV[round(n.sim*0.05)]
         Q10<-sort.DV[round(n.sim*0.10)]
         Q25<-sort.DV[round(n.sim*0.25)]
         Q50<-sort.DV[round(n.sim*0.50)]
         Q75<-sort.DV[round(n.sim*0.75)]
         Q90<-sort.DV[round(n.sim*0.90)]    
         Q95<-sort.DV[round(n.sim*0.95)]
         Q975<-sort.DV[round(n.sim*0.975)]
         sim.tot[[i]]<-temp.list
         q.temp<-c(sim.mat$ID[i],sim.mat$TIME[i],Q025,Q05,Q10,Q25,
                   Q50,Q75,Q90,Q95,Q975,sim.mat$PRED[temp.id[1]])
         Quantile.tot<-rbind(Quantile.tot,q.temp)
      }
      colnames(Quantile.tot)<-c("ID","TIME","Q025","Q05","Q10","Q25","Q50","Q75","Q90","Q95","Q975","PRED")
      rownames(Quantile.tot)<-NULL
      Quantile.tot<-data.frame(Quantile.tot)
      Quantile.keep<<-Quantile.tot

      updatePlot<-function(h,...)
      {  CI.range<-svalue(CI.list)
         plot(sim.mat$TIME,sim.mat$DV,type='n',xlab="TIME",ylab="DV",ylim=c(0,max(Quantile.tot$Q975)))
         points(D.data$TIME,D.data$DV)
         temp3<-tapply(Quantile.tot$Q50,Quantile.tot$TIME,mean)
         lines(as.numeric(names(temp3)),temp3,lty=1)
         temp3<-tapply(Quantile.tot$Q25,Quantile.tot$TIME,mean)
         lines(as.numeric(names(temp3)),temp3,lty=2)
         temp3<-tapply(Quantile.tot$Q75,Quantile.tot$TIME,mean)
         lines(as.numeric(names(temp3)),temp3,lty=2)
         if(CI.range=="95%")
         {  temp1<-tapply(Quantile.tot$Q025,Quantile.tot$TIME,mean)
            lines(as.numeric(names(temp1)),temp1,lty=2,col=2)
            temp5<-tapply(Quantile.tot$Q975,Quantile.tot$TIME,mean)
            lines(as.numeric(names(temp5)),temp5,lty=2,col=2)
         } else if(CI.range=="90%")   
         {  temp1<-tapply(Quantile.tot$Q05,Quantile.tot$TIME,mean)
            lines(as.numeric(names(temp1)),temp1,lty=2,col=2)
            temp5<-tapply(Quantile.tot$Q95,Quantile.tot$TIME,mean)
            lines(as.numeric(names(temp5)),temp5,lty=2,col=2)
         } else if(CI.range=="80%")   
         {  temp1<-tapply(Quantile.tot$Q10,Quantile.tot$TIME,mean)
            lines(as.numeric(names(temp1)),temp1,lty=2,col=2)
            temp5<-tapply(Quantile.tot$Q90,Quantile.tot$TIME,mean)
            lines(as.numeric(names(temp5)),temp5,lty=2,col=2)
         }  
      }  
   
      win<-gwindow("Visual Predictive Check")
      BigGroup<-ggroup(cont=win)
      group<-ggroup(horizontal=FALSE,cont=BigGroup)
      CI.list<-gdroplist(c("95%","90%","80%"))
      Button<-gbutton("OK",handler=updatePlot)
      tmp<-gframe("CI",cont=group)
      add(tmp,CI.list)
      tmp<-gframe("plot",cont=group)
      add(tmp,Button)  
      add(BigGroup,ggraphics())
   
   # NPC
      time.list<-as.numeric(names(table(D.data[,TIME.id])))
      if(sum(Var.Prop.Keep=="MDV")==0)
      {  D.temp<-D.data
      } else
      {  D.temp<-D.data[(D.data[,which(Var.Prop.Keep=="MDV")]==0),] 
      }

      S1<-nrow(D.data)
      S2<-nrow(D.temp)
      S3<-VPC.N  
      tempMed<-tapply(Quantile.tot$Q50,Quantile.tot$TIME,mean)
      tempU<-tapply(Quantile.tot$Q975,Quantile.tot$TIME,mean)
      tempL<-tapply(Quantile.tot$Q025,Quantile.tot$TIME,mean)
 
      NPC.tot<-NULL
      for(i in 1:length(time.list))
      {  temp.id<-which(D.temp[,TIME.id]==time.list[i])
         S4<-sum(D.temp[temp.id,DV.id]>tempMed[i])
         S5<-sum(D.temp[temp.id,DV.id]<=tempMed[i])
         S6<-sum(D.temp[temp.id,DV.id]>tempU[i])
         S7<-sum(D.temp[temp.id,DV.id]<tempL[i])  
         NPC.tot<-rbind(NPC.tot,c(S4,S5,S6,S7))
      }
      NPC.sum<-apply(NPC.tot,2,sum)

      NPC.txt<-paste("---------------------------------------------\n")
      NPC.txt<-paste(NPC.txt,"Number of Records        : ",nrow(D.data),"\n",sep="")
      NPC.txt<-paste(NPC.txt,"Total observations       : ",nrow(D.temp),"\n",sep="")
      NPC.txt<-paste(NPC.txt,"Number of Iteration      : ",S3,"\n",sep="")
      NPC.txt<-paste(NPC.txt,"=============================================\n",sep="")
      NPC.txt<-paste(NPC.txt,"Points above the medians : ",NPC.sum[1],
                             "(",round(NPC.sum[1]/nrow(D.temp)*100,1),"%)","\n",sep="")
      NPC.txt<-paste(NPC.txt,"Points below the medians : ",NPC.sum[2],
                             "(",round(NPC.sum[2]/nrow(D.temp)*100,1),"%)","\n",sep="")
      NPC.txt<-paste(NPC.txt,"Ratio of points above to points below : ",
                                  round(NPC.sum[1]/NPC.sum[2],2),"\n",sep="")
      NPC.txt<-paste(NPC.txt,"=============================================\n",sep="")
      NPC.txt<-paste(NPC.txt,"***  95% Prediction Interval ***\n",sep="")
      NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
      NPC.txt<-paste(NPC.txt,"Points above 95% PI      : ",NPC.sum[3],
                             "(",round(NPC.sum[3]/nrow(D.temp)*100,1),"%)","\n",sep="")
      NPC.txt<-paste(NPC.txt,"Points below 95% PI      : ",NPC.sum[4],
                             "(",round(NPC.sum[4]/nrow(D.temp)*100,1),"%)","\n",sep="")
      NPC.txt<-paste(NPC.txt,"Ratio of points above to points below : ",
                                  round(NPC.sum[3]/NPC.sum[4],2),"\n",sep="")
      NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
      edit.win<-gwindow("Numerical Predictive Check")
      g<-ggroup(horizontal=FALSE,cont=edit.win)
      tmp<-gframe("NPC",container=g)
      a<-gtext(NPC.txt,width=410,height=310,font.attr=c(sizes="large",family="monospace"))
      add(tmp,a)
    }

##
   VPC.N.init<-function() 
   {  set.VPCN<-function(h,...)
      {  VPC.N<<-as.numeric(svalue(VPC.input))
         dispose(win)
         VPC.try()
      }
      win<-gwindow("Visual/Numerical Predictive Check",width=100,height=20)
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
      tmp=gframe("Number of simulated sample",container=group)
      VPC.label<-glabel("N")
      VPC.input<-gedit("1000",width=10)
      Button<-gbutton("OK",handler=set.VPCN)
      add(tmp,VPC.label)
      add(tmp,VPC.input)
      add(tmp,Button)
   }

##
   VPC<-function(h,...)
   {  VPC.N.init()
   }

# --- main part -----

  RunID<<-0
  Cur.Run<<-0
  INTER.flag<<-FALSE
  FO.flag<<-FALSE
  FOCE.flag<<-FALSE
  POSTHOC.flag<<-FALSE
  Current.CTL<<-NULL

  NONMEM.win<<-gwindow("GUI for NONMEM",width=750,height=300)
  menu.list<-list(File=list('Setup Working Directory'=list(handler=setupWD),
                            'Open Data File'=list(handler=OpenDataFile,icon="open"),
                            'Open Control File'=list(handler=OpenControlFile,icon="close"),
                            'Edit Control File'=list(handler=Editor),
                            'Quit'=list(handler=function(h,...) dispose(NONMEM.win),icon="quit")),    
                  EDA =list('V1 vs V2'=list(handler=XY.plot),
                            'DV vs TIME by ID'=list(handler=ID.plot),
                            'PK explorer'=list(handler=PK.plot),
                            'PD explorer'=list(handler=PD.plot)),
                  NONMEM=list('Run NONMEM'=list(handler=runNONMEM)),
                  'Output'=list('Parameters'=list(handler=show.parameters),
                                'G & H'=list(handler=show.GandH),
                                'Fitted and Residual'=list(handler=show.FittedResidual),                         
                                'LST file'=list(handler=show.LST)),
                  'Model Check'=list('DV vs PRED'=list(handler=DVvsPRED.plot),
                                          'DV vs RES'=list(handler=DVvsRES.plot),
                                          'TIME vs RES'=list(handler=TIMEvsRES.plot),
                                          'TIME vs DV and PRED'=list(handler=TIMEvsDVandPRED.plot),
                                          'EBE vs COV'=list(handler=EBEvsCOV.plot)),
                  'Model Evaluation'=list('Bootstraping'=list(handler=Boot),
                                          'Visual/Numerical Predictive Check'=list(handler=VPC))
                   )
  gmenu(menu.list,cont=NONMEM.win)
  table.name<-c("Run","Date","Time","MIN","COV","OFV","AIC","BIC",
                "ControlFile","DataFile","Model Description") 
  nonmem.run<<-matrix("",ncol=length(table.name),nrow=20)
  colnames(nonmem.run)<-table.name
  run.table<<-gtable(nonmem.run,chosencol=length(table.name),cont=NONMEM.win)
}

