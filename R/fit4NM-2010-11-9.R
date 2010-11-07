

fit4NM<-function()
{  options(guiToolkit="RGtk2")
   require(gWidgets)
   require(tcltk)
   require(tkrplot)
   require(RGtk2)
   require(gWidgetsRGtk2)
   require(cairoDevice)   
   
   dir.create("c:/fit4NM",showWarnings=FALSE)
   if(length(dir("c:/fit4NM",all.files=TRUE))>2)
   {   load("c:/fit4NM/.Rdata",.GlobalEnv)
   }
   ###################################
   # Set NONMEM Path-Default
   ###################################
   
   NMpath1<-function(h,...)
   {  Default.NMpath<<-gfile(text="Choose default NONMEM",type="open")
   }
   
   NMpathHelp1<-function(h,...)
   {  Default.Helppath<<-gfile(text="Choose default NONMEM help - index.htm",type="open")
   }
   
   ###################################
   # Set NONMEM Path-Alternative
   ###################################

   NMpath2<-function(h,...)
   {  Alternative.NMpath<<-gfile(text="Choose alternative NONMEM",type="open")
   }

   NMpathHelp2<-function(h,...)
   {  Alternative.Helppath<<-gfile(text="Choose alternative NONMEM help - index.htm",type="open")
   }

   ###################################
   # Set Editor path
   ###################################

   Editorpath<-function(h,...)
   {  Editor.path<<-gfile(text="Choose external editor",type="open")
   }
      
   ###################################
   # Save Configuration
   ###################################
 
   saveConfig<-function(h,...)
   {  dir.create("c:/fit4NM",showWarnings=FALSE)
      dir.list<-ls(pos=1)
      flag.D.H<-sum(dir.list=="Default.Helppath")
      flag.D.N<-sum(dir.list=="Default.NMpath")
      flag.A.H<-sum(dir.list=="Alternative.Helppath")
      flag.A.N<-sum(dir.list=="Alternative.NMpath")
      flag.E<-sum(dir.list=="Editor.path")
      save.list<-c(ifelse(flag.D.H==1,"Default.Helppath",NA),ifelse(flag.A.H==1,"Alternative.Helppath",NA),
                   ifelse(flag.D.N==1,"Default.NMpath",NA),ifelse(flag.A.N==1,"Alternative.NMpath",NA),
                   ifelse(flag.E==1,"Editor.path",NA))
      save.list<-save.list[!is.na(save.list)]
      save(list = save.list, file = "c:/fit4NM/.Rdata")
   }

   ###################################
   # Calculate elapsed time 
   ###################################  
    
   CalcTime<-function(h,...)
   {  openStandard<-function(h,...)
      {  standard.file<<-gfile(text="Choose standard time file",type="open")
         temp<-strsplit(standard.file,split="\\\\")[[1]]
         file.id<-temp[length(temp)]
         dir.name<-strsplit(standard.file,split=file.id)[[1]]
         setwd(dir.name)
         svalue(standard.t)<-standard.file
      }
      
      openIndiv1<-function(h,...)
      {  indiv.dir<<-gfile(text="Choose folder for individual data",type="selectdir")
         indiv.file<-dir(indiv.dir)
         for(i in 1:length(indiv.file))
         {  indiv.t[i]<-indiv.file[i]
         }
      }
  
      ElapseTime<-function(h,...)
      {  setTIME<-svalue(standard.t)
         setDIR<-indiv.dir 
         UNIT<-svalue(time.t)
         New.Time<-CalcTIME0(setTIME,setDIR,UNIT)
         temp<-gfile(text="Save calculated elapsed time as csv",
           type="save") #,filter=list("csv files"=list(patterns=c("*.csv"))))
         file.name<-strsplit(temp,split="\\.")[[1]][1]
         write.csv(New.Time,paste(file.name,".csv",sep=""),row.names=F,quote=F)
         dispose(timewin1)  
      }

      CalcTIME0<-function(setTIME,setDIR,UNIT)
      {  file.list<-dir(setDIR)
         setTIME<-read.csv(standard.file)
         tot.data<-NULL
         for(i in 1:length(file.list))
         {  data.t<-read.csv(paste(setDIR,"\\",file.list[i],sep=""))
            orig.colname<-colnames(data.t)
            colnames(data.t)<-toupper(orig.colname)
            DATETIME<-paste(data.t$DATE,data.t$TIME,sep=" ")
            data.temp<-strptime(DATETIME,"%Y-%m-%d %H:%M:%S")
            standard<-paste(as.character(setTIME$DATE0[setTIME$X.ID==data.t$X.ID[1]]),
                    as.character(setTIME$TIME0[setTIME$X.ID==data.t$X.ID[1]]),sep=" ")
            TIME<-as.numeric(difftime(strptime(data.temp,"%Y-%m-%d %H:%M:%S"),
                  strptime(standard,"%Y-%m-%d %H:%M:%S"), units=UNIT))
            data.t$TIME<-TIME
            data.t<-data.t[,-which(toupper(orig.colname)=="DATE")]
            colnames(data.t)<-orig.colname[-which(toupper(orig.colname)=="DATE")]
            tot.data<-rbind(tot.data,data.t)
         }
         temp<-colnames(tot.data)
         temp[temp=="X.ID"]<-"#ID"
         colnames(tot.data)<-temp
         return(tot.data)
      }

      timewin1<<-gwindow("Caclulate elapsed time")
      BBgroup<-ggroup(cont=timewin1,horizontal=FALSE)
      Bgroup1<-ggroup(cont=BBgroup,horizontal=TRUE)

      tmp<-gframe("",cont=BBgroup)
      standard.t<-gedit(" ",width=50)
      button1<-gbutton("Open reference time file #ID, DATE0 (yyyy-mm-dd), TIME0 (h:mm:ss)",handler=openStandard)
      add(tmp,button1)
      add(tmp,standard.t)

      tmp<-gframe("",cont=BBgroup)
      button2<-gbutton("Select data folder #ID, DATE (yyyy-mm-dd), TIME (h:mm:ss),... after data split by ID",handler=openIndiv1,width=20,height=10)
      add(tmp,button2)
      indiv.t<-gdroplist(c(" "),width=200,height=200)
      add(tmp,indiv.t)

      tmp<-gframe("",cont=BBgroup,height=50)
      button4<-gbutton("Select time unit",width=20,height=10)
      add(tmp,button4)
      time.t<-gdroplist(c("secs","mins","hours","days","weeks"),width=200,height=200)
      add(tmp,time.t)

      tmp<-gframe("",cont=BBgroup)
      button3<-gbutton("Calculate and save as csv",handler=ElapseTime,width=20,height=10)
      add(tmp,button3)
   }
   
   ###################################
   # Data Join
   ###################################
   DataJoinhandler<-function(h,...)
   {  DataJoin<-function(h,...)
      {  kk<-DJk
         tmp<-DataJoin.totDATA
         file.name<<-gfile(text="Data file",type="open")
         tclvalue(DataJoin.Name[[kk]])<<-file.name
         tmp[[kk]]<-read.csv(file.name,na.strings=".")
         DataJoin.totDATA<<-tmp
      }
      AddData<-function(h,...)
      {  k<-DJk+1
         DataJoin.Name[[k]]<<-tclVar("")
         DataJoinName[[k]]<<-tkentry(tt,width="60",textvariable=DataJoin.Name[[k]])
         tkgrid(tklabel(tt,text=" "),DataJoinName[[k]],tklabel(tt,text=" "))
         tkgrid(tt)
         DJk<<-k
      }

      JoinData<-function(h,...)
      {  merge.data<-DataJoin.totDATA[[1]]
            if(DJk>=2)
            {  for(i in 2:DJk)
               {  #merge.data<-merge(merge.data,DataJoin.totDATA[[i]],by=by.var,suffixes=c("",""))
                  merge.data<-merge(merge.data,DataJoin.totDATA[[i]],all=T)
               }
               temp<-colnames(merge.data)
               temp[temp=="X.ID"]<-"#ID"
               colnames(merge.data)<-temp
               file.new<-gfile(text="Save as csv",type="save")
               write.csv(merge.data,paste(file.new,".csv",sep=""),quote=FALSE,row.names=FALSE)
            } else
            {  warnings()
            }
            tkdestroy(Toptt)    
      }

      DataJoin.totDATA<<-list()
      Toptt<<-tktoplevel()
      tkwm.title(Toptt,"Data join")
      tt<-tkframe(Toptt)
      ttg<-tkframe(tt)
      tkgrid(ttg)
      OK.but2 <-tkbutton(tt,text="Select csv file",command=DataJoin)
      Add.but<-tkbutton(tt,text="Add more csv file",command=AddData)
      Join.but<-tkbutton(tt,text="Join",command=JoinData)
      tkgrid(tklabel(tt,text=""),
                    OK.but2,tklabel(tt,text=""),Add.but,tklabel(tt,text=""),Join.but)
      tkgrid(tt)
      DataJoin.Name<<-list()
      DataJoinName<<-list()
      DJk<<-1
      DataJoin.Name[[DJk]]<-tclVar("")
      DataJoinName[[DJk]]<-tkentry(tt,width="60",textvariable=DataJoin.Name[[DJk]]) 

      tkgrid(tklabel(tt,text=" "),DataJoinName[[DJk]],tklabel(tt,text=" "),tklabel(tt,text=" "))
      tkgrid(tt)	
   }

   ###################################
   # Data split
   ###################################

   DataSplit<-function(h,...)
   {  splitD<-function(h,...)
      {  splitDIR<-gfile("Choose folder",type="selectdir")
         setwd(splitDIR)
         cond1<-svalue(VarList.g1)
         cond2<-svalue(VarList.g2)
         temp<- strsplit(whole.file,"\\\\")[[1]]
         temp<-temp[length(temp)]
         header<-strsplit(temp,"\\.")[[1]][1]
         if(cond2=="NONE")
         {  cond<-paste(cond1,as.character(whole.data[,cond1]),sep="_")
         } else
         {  cond<-paste(cond1,as.character(whole.data[,cond1]),cond2,as.character(whole.data[,cond2]),sep="_")
         }  
         sel.id<-names(table(cond))
     
         for(i in sel.id)
         {  select.id<-which(cond==i)
            select.data<-whole.data[select.id,]
            sel.filename<-paste(header,"_",i,".csv",sep="")
            temp<-colnames(select.data)
            temp[temp=="X.ID"]<-"#ID"
            colnames(select.data)<-temp
            write.csv(select.data,sel.filename,row.names=F)
         }  
      }

      whole.file<-gfile(text="Open control file",type="open")
      whole.data<-read.csv(whole.file)
      Var.Name<-colnames(whole.data)
      VarList.g1<-gdroplist(Var.Name)
      VarList.g2<-gdroplist(c("NONE",Var.Name))
      gDS<-gwindow("Data split",width=100)
      ggDS<-ggroup(horizontal=FALSE,cont=gDS)
      tmp<-gframe("Level 1",cont=ggDS)
      add(tmp,VarList.g1)
      tmp<-gframe("Level 2",cont=ggDS)
      add(tmp,VarList.g2)
      Button1<-gbutton("OK",handler=splitD)
      tmp<-gframe("Split",cont=ggDS)
      add(tmp,Button1)
   }
  
   ###################################
   # Create NONMEM Data
   ###################################
   
   DataPrep<-function(h,...)
   {  DemogOK <- function()
      {  fileName1<<-gfile(text="Choose demographic data file",type="open")
         tclvalue(DemogName)<-fileName1
         dir.name<-strsplit(fileName1,split="\\.")[[1]][1]
         temp<-strsplit(dir.name,"\\\\")[[1]]    
         dataname<-temp[length(temp)]
         dir.name<-strsplit(dir.name,dataname)[[1]]  
         setwd(dir.name)       
      }
      AdmOK <- function()
      {  fileName2<<-gfile(text="Choose dosing data file",type="open")
         tclvalue(AdmName)<-fileName2
      }
      DVOK <- function()
      {  fileName3<<-gfile(text="Choose DV data file",type="open")
         tclvalue(DVName)<-fileName3
      }
      IPKOK<-function()
      {  fileName5<<-gfile(text="Choose IPK data file",type="open")
         tclvalue(IPKName)<-fileName5
      }

      displayInTable <- function(tclarray,title="",height=-1,width=-1,nrow=-1,ncol=-1)
      {  tt <- tktoplevel()
  	     tclRequire("Tktable")
  	     tkwm.title(tt,title)
  	     table1 <- tkwidget(tt,"table",rows=nrow,cols=ncol,titlerows=1,titlecols=0,
             	height=height+1,width=width+1,xscrollcommand=function(...) tkset(xscr,...),
		     yscrollcommand=function(...) tkset(yscr,...))
  	     xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  	     yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))
  	     tkgrid(table1,yscr)
  	     tkgrid.configure(yscr,sticky="nsw")
  	     tkgrid(xscr,sticky="new")
  	     tkconfigure(table1,variable=tclarray,background="white",selectmode="extended")
  	     return (table1)
      }

      openSpread<-function()
      {  NM.data.temp<-matrix(as.character(NM.data),ncol=ncol(NM.data))
   	     NM.data.temp<-rbind(colnames(NM.data),NM.data.temp)
    	   tclArray1 <- tclArray()
    	   for(i in (1:nrow(NM.data.temp)))
    	      for(j in (1:ncol(NM.data.temp)))
       	       tclArray1[[i-1,j-1]] <- NM.data.temp[i,j]
	       table1 <- displayInTable(tclArray1,nrow=nrow(NM.data.temp),ncol=ncol(NM.data.temp))
      }

      Combine<-function()
      {    Demog<<-read.csv(fileName1)
    	   Adm<<-read.csv(fileName2)
    	   DV<<-read.csv(fileName3)
    	   fileName5<<-tclvalue(IPKName)
    	   if(fileName5!="") IPK<<-read.csv(fileName5)
    	   ID.list<-unique(Demog$X.ID)
    	   if(fileName5!="")
    	   {  Demog.temp<-matrix(0,nrow=length(ID.list),ncol=(ncol(Demog)+ncol(IPK)-1))
       	      colnames(Demog.temp)<-c("X.ID",colnames(Demog)[-1],colnames(IPK)[-1])
       	      Demog.temp<-data.frame(Demog.temp)
       	      Demog.temp[,1]<-ID.list
       	      for(i in ID.list)
                 Demog.temp[Demog.temp$X.ID==i,]<-c(Demog[Demog$X.ID==i,],
                                                    IPK[IPK$X.ID==i,-1])
       	      Demog<<-Demog.temp
    	   }
    	   tot.data<-NULL
    	   colname.final1<-c(colnames(Adm),"DV","MDV")
    	   colname.final2<-c(colnames(Adm),"DV","MDV",colnames(Demog)[-1])
    	   for(i in ID.list) 
    	   {  DV.temp<-DV[which(DV$X.ID==i),]
    	      MDV<-rep(10,nrow(DV.temp))
    	      DV.temp<-cbind(DV.temp,MDV)
    	      temp.list<-which(Adm$X.ID==i)
    	      temp<-Adm[temp.list,]
    	      MDV<-rep(1,nrow(temp))
    	      temp<-cbind(temp,MDV)
     	      temp<-merge(temp,DV.temp,all=TRUE)   	      
     	      temp$MDV[temp$MDV==10]<-0
     	      temp<-temp[,colname.final1]
     	      Demog.temp<-matrix(rep(Demog[which(Demog$X.ID==i),-1],nrow(temp)),nrow=nrow(temp),byrow=T)
    	      temp<-cbind(temp,Demog.temp)
              colnames(temp)<-colname.final2
              tot.data<-rbind(tot.data,temp)
    	  }
    	 tot.data<-as.matrix(tot.data)
    	 temp<-as.character(tot.data)
    	 temp[which(temp=="NA")]<-"."
    	 tot.data<-matrix(temp,ncol=ncol(tot.data))
    	 colname.final2[1]<-"#ID"
    	 tot.data<-rbind(colname.final2,tot.data)
    	 NM.data<<-tot.data
    	 openSpread()
      }
      
      Save<-function()
      {  fileName4<-tclvalue(tkgetSaveFile(filetypes="{{CSV Files} {.csv}}")) 
    	 fileName4<-paste(fileName4,".csv",sep="")
    	 write.table(NM.data,fileName4,sep=",",quote=FALSE,row.names=FALSE, col.names=FALSE)
    	 tkdestroy(Toptt)
      }

      Demog<<-NULL
      Adm<<-NULL
      DV<<-NULL
      IPK<<-NULL
      Toptt<<-tktoplevel()
      tkwm.title(Toptt,"NM data preparation for PREDPP")
      tt<-tkframe(Toptt)

      DemogName <- tclVar("")
      Demog.Name <-tkentry(tt,width="60",textvariable=DemogName)
      IPKName <- tclVar("")
      IPK.Name <-tkentry(tt,width="60",textvariable=IPKName)
      AdmName <- tclVar("")
      Adm.Name <-tkentry(tt,width="60",textvariable=AdmName)
      DVName <- tclVar("")
      DV.Name <-tkentry(tt,width="60",textvariable=DVName)

      OK.but1 <-tkbutton(tt,text="   Select   ",width=7,height=1,command=DemogOK)
      OK.but2 <-tkbutton(tt,text="   Select   ",width=7,height=1,command=AdmOK)
      OK.but3 <-tkbutton(tt,text="   Select   ",width=7,height=1,command=DVOK)
      OK.but4 <-tkbutton(tt,text="   Select   ",width=7,height=1,command=IPKOK)
      CombineOK<-tkbutton(tt,text="Combine",width=7,height=1,command=Combine)
      SaveOK<-tkbutton(tt,text="    Save   ",width=7,height=1,command=Save)

      tkgrid(tklabel(tt,text="Demographics (#ID, covariates)"),Demog.Name,OK.but1)
      tkgrid(tklabel(tt,text="IPK (#ID, IPK)"),IPK.Name,OK.but4)
      tkgrid(tklabel(tt,text="Dosing (#ID, TIME (elapse), AMT,RATE)"),Adm.Name,OK.but2)
      tkgrid(tklabel(tt,text="DV (#ID, TIME (elapse), DV)"),DV.Name,OK.but3)
      tkgrid(tklabel(tt,text=" "),tklabel(tt,text=" "),CombineOK)
      tkgrid(tklabel(tt,text=" "),tklabel(tt,text=" "),SaveOK)
      tkgrid(tt)
   }
   
   data.ID<-function(h,...)
   {  data.file<-gfile(text="Choose data file",type="open")
      DD.data<- read.csv(data.file,na.string=".")
      rep.n<-table(DD.data$X.ID)
      seq.id<-1:length(rep.n)
      new.id<-rep(seq.id,rep.n)
      new.data<-cbind(new.id,DD.data)
      colnames(new.data)[1:2]<-c("#ID","OID")
      dir.name<-strsplit(data.file,split="\\.")[[1]][1]
      temp<-strsplit(dir.name,"\\\\")[[1]]    
      dataname<-temp[length(temp)]
      dir.name<-strsplit(dir.name,dataname)[[1]]
      setwd(dir.name)   
      write.csv(new.data,paste(gfile(text="Save as csv",
                 type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
   }
   ###################################
   # Select Data File
   ###################################
   
   OpenEDAData<-function(h,...)
   {  EDAfileName<<-gfile(text="Choose EDA data file",type="open")
      EDA.data<<-read.csv(EDAfileName,na.string=".")
      Var.Name<<-colnames(EDA.data)
      dir.name<-strsplit(EDAfileName,split="\\.")[[1]][1]
      temp<-strsplit(dir.name,"\\\\")[[1]]    
      dataname<-temp[length(temp)]
      dir.name<-strsplit(dir.name,dataname)[[1]]
      setwd(dir.name)
   }

   ###################################
   # Summary Statistics-continuous
   ###################################
     
   Summary.stat<-function(h,...)
   {  calc.summary<-function()
      {  DA.data<-as.matrix(EDA.data[,Con.list])
         colnames(DA.data)<-Con.list
         summary.stat1<-rbind(apply(DA.data,2,function(x) mean(x,na.rm=T)),
         apply(DA.data,2,function(x) sd(x,na.rm=T)), 
         apply(DA.data,2,function(x) min(x,na.rm=T)),
         apply(DA.data,2,function(x) quantile(x,na.rm=T,probs=0.25)),
         apply(DA.data,2,function(x) quantile(x,na.rm=T,probs=0.5)),
         apply(DA.data,2,function(x) quantile(x,na.rm=T,probs=0.75)),
         apply(DA.data,2,function(x) max(x,na.rm=T)))
         summary.stat1<-t(summary.stat1)
         colnames(summary.stat1)<-c("Mean","SD","Mininum","Q1","Median","Q3","Maximum")
         summary.stat<-round(summary.stat1,3)
         summary.stat<-cbind(c(rownames(summary.stat1)),summary.stat)
         savesummarydata<-function(h,...)
         {  dir.name<-strsplit(EDAfileName,split="\\.")[[1]][1]
            temp<-strsplit(dir.name,"\\\\")[[1]]    
            dataname<-temp[length(temp)]
            dir.name<-strsplit(dir.name,dataname)[[1]]
            setwd(dir.name)
            write.csv(summary.stat,paste(gfile(text="Save as csv",
                 type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
         }
         summary.w<-gwindow("Summary statistics")
         gsummary<-ggroup(cont=summary.w,horizontal=F)
         summary.table<-gtable(summary.stat,do.subset=TRUE,cont=gsummary)
         size(summary.table)<-c(20,200)
         tmp<-gframe(cont=gsummary,spacing=10000)
         Button1<-gbutton("Save",handler=savesummarydata,spacing=2000)
         size(Button1)<-c(50,30)
         add(tmp,Button1)
      }

      saveCat<-function(h,...)
      {  Con.list<<-Var.Name[svalue(catcheck)]
         dispose(checkg)
         calc.summary()
      }

      Var.Name<-colnames(EDA.data)
      checkg<-gwindow("Select continuous variable for summary statistics")
      catcheck<-gcheckboxgroup(Var.Name,use.table=TRUE,cont=checkg)
      Button1<-gbutton("OK",type="OK",handler=saveCat,cont=checkg)
   }
   

 
   #########################################################
   # Summary Statistics-categorical-single level per person
   #########################################################

   Summary.cat<-function(h,...)
   {  saveCat<-function(h,...)
      {  Cat.list<<-Var.Name[svalue(catcheck)]
         dispose(checkg)
         n.table<-tapply(rep(1,nrow(EDA.data)),EDA.data$X.ID,sum)
         temp<-EDA.data[1,]
   
         for(i in 2:length(n.table)) 
            temp<-rbind(temp,c(EDA.data[sum(n.table[1:(i-1)])+1,]))

         cont.list<-list()
         for(i in 1:length(Cat.list))
         {  cont.list[[i]]<-table(temp[,Cat.list[i]])
         }

         names(cont.list)<-Cat.list

         p<-max(unlist(lapply(cont.list,length)))
         n<-length(Cat.list)*2
         Cat.summary<-matrix(" ",nrow=n,ncol=(p+1))
         colnames(Cat.summary)<-c("Variable",paste("level",1:p,sep=""))
         for(i in 1:length(Cat.list))
         {  Cat.summary[(i-1)*2+1,1]<-Cat.list[i]
            Cat.summary[(i-1)*2+1,2:(length(cont.list[[i]])+1)]<-names(cont.list[[i]])
            Cat.summary[(i-1)*2+2,2:(length(cont.list[[i]])+1)]<-cont.list[[i]]
         }
     
         savesummarycdata<-function(h,...)
         {  dir.name<-strsplit(EDAfileName,split="\\.")[[1]][1]
            temp<-strsplit(dir.name,"\\\\")[[1]]    
            dataname<-temp[length(temp)]
            dir.name<-strsplit(dir.name,dataname)[[1]]
            setwd(dir.name)
            write.csv(Cat.summary,paste(gfile(text="Save as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
         }

         summary.cw<-gwindow("Summary Categorical variable Statistics")
         gsummary<-ggroup(cont=summary.cw,horizontal=F)
         summary.table<-gtable(Cat.summary,do.subset=TRUE,cont=gsummary)
         size(summary.table)<-c(20,200)
         tmp<-gframe(cont=gsummary,spacing=10000)
         Button1<-gbutton("Save",handler=savesummarycdata,spacing=2000)
         size(Button1)<-c(50,30)
         add(tmp,Button1)
      }
      Var.Name<-colnames(EDA.data)
      checkg<-gwindow("Select categorical variable for summary statistics")
      catcheck<-gcheckboxgroup(Var.Name,use.table=TRUE,cont=checkg)
      Button1<-gbutton("OK",type="OK",handler=saveCat,cont=checkg)
   }

   #########################################################
   # Summary Statistics-categorical-multiple levels per person
   #########################################################


   Summary.cat1<-function(h,...)
   {  saveCat<-function(h,...)
      {  Cat.list<<-Var.Name[svalue(catcheck)]
         dispose(checkg)
         n.table<-tapply(rep(1,nrow(EDA.data)),EDA.data$X.ID,sum)
         temp<-EDA.data
         cont.list<-list()
         for(i in 1:length(Cat.list))
         {   IDs<-EDA.data$X.ID
             X<-temp[,Cat.list[i]]
             temp.A<-paste(IDs,X,sep="-")
             cont.list[[i]]<- table(X[!duplicated(temp.A)])
         }

         names(cont.list)<-Cat.list
         p<-max(unlist(lapply(cont.list,length)))
         n<-length(Cat.list)*2
         Cat.summary<-matrix(" ",nrow=n,ncol=(p+1))
         colnames(Cat.summary)<-c("Variable",paste("level",1:p,sep=""))
         for(i in 1:length(Cat.list))
         {  Cat.summary[(i-1)*2+1,1]<-Cat.list[i]
            Cat.summary[(i-1)*2+1,2:(length(cont.list[[i]])+1)]<-names(cont.list[[i]])
            Cat.summary[(i-1)*2+2,2:(length(cont.list[[i]])+1)]<-cont.list[[i]]
         }
     
         savesummarycdata<-function(h,...)
         {  dir.name<-strsplit(EDAfileName,split="\\.")[[1]][1]
            temp<-strsplit(dir.name,"\\\\")[[1]]    
            dataname<-temp[length(temp)]
            dir.name<-strsplit(dir.name,dataname)[[1]]
            setwd(dir.name)
            write.csv(Cat.summary,paste(gfile(text="Save as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
         }

         summary.cw<-gwindow("Summary Categorical variable Statistics")
         gsummary<-ggroup(cont=summary.cw,horizontal=F)
         summary.table<-gtable(Cat.summary,do.subset=TRUE,cont=gsummary)
         size(summary.table)<-c(20,200)
         tmp<-gframe(cont=gsummary,spacing=10000)
         Button1<-gbutton("Save",handler=savesummarycdata,spacing=2000)
         size(Button1)<-c(50,30)
         add(tmp,Button1)
      }
      Var.Name<-colnames(EDA.data)
      checkg<-gwindow("Select categorical variable for summary statistics")
      catcheck<-gcheckboxgroup(Var.Name,use.table=TRUE,cont=checkg)
      Button1<-gbutton("OK",type="OK",handler=saveCat,cont=checkg)
   }
   
   ###################################
   # XY plot
   ###################################

   XY.plot<-function(h,...)
   {  D.data<-EDA.data
      ID.id<-NULL
      if(sum(colnames(D.data)=="X.ID")!=0)
         ID.id<-which(colnames(D.data)=="X.ID")
      updatePlot<-function(h,...)
      {  condX.V <-svalue(VarList.X,index=T)
         condY.V<-svalue(VarList.Y,index=T)
         select.data<-D.data
         if(!is.na(condX.V)& !is.na(condY.V))
         {  X<-select.data[,condX.V]
            Y<-select.data[,condY.V]
            dev.set(which=Dev.XYplot)
            plot(X,Y,xlab=Var.Name[condX.V],ylab=Var.Name[condY.V])
         }
      }
      saveData<-function(h,...)
      {  condX.V <-svalue(VarList.X,index=T)
         condY.V<-svalue(VarList.Y,index=T)
         select.data<-D.data[,c(ID.id,condX.V,condY.V)]
         if(is.null(ID.id))
         {  colnames(select.data)<-Var.Name[c(condX.V,condY.V)]
         } else
         {  colnames(select.data)<-Var.Name[c(ID.id,condX.V,condY.V)]
         }
         write.csv(select.data,paste(gfile(text="Save as csv",
             type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)          
      }
      VarList.X<-gdroplist(Var.Name)
      VarList.Y<-gdroplist(Var.Name)
      Button1<-gbutton("OK",handler=updatePlot)
      Button2<-gbutton("Save",handler=saveData)
    
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
      Dev.XYplot<-dev.cur()
   }

  
   ###################################
   # DV vs TIME by ID
   ###################################

   ID.plot<-function(h,...)
   {  D.data<-EDA.data
      ID.id<-NULL
      if(sum(colnames(D.data)=="X.ID")!=0)
         ID.id<-which(colnames(D.data)=="X.ID")
      TIME.id<-which(tolower(colnames(D.data))=="time")
      DV.id<-which(tolower(colnames(D.data))=="dv")
       
      if(is.null(ID.id))  
      {  gconfirm("Need ID information",icon="warning")
      } else
      {  ID<-sort(unique(D.data[,ID.id]))
         ID<-matrix(ID,nrow=length(ID))
         colnames(ID)<-c("ID")
         updatePlot<-function(h,...) 
         {  dev.set(which=Dev.IDplot)
            plot(D.data[,TIME.id],D.data[,DV.id],type='n',xlab="TIME",ylab="DV")
            id<-names(table(D.data[,ID.id]))
            for(i in 1:length(id))
            {  data<-D.data[which(D.data[,ID.id]==as.numeric(id[i])),]
               data<-data[!is.na(data[,DV.id]),]
               lines(data[,TIME.id],data[,DV.id],lty=3)
            }
            select.id<-svalue(IDlist)
            for(kk in 1:length(select.id))
            {  data<-D.data[which(D.data[,ID.id]==select.id[kk]),]
               data<-data[!is.na(data[,DV.id]),]
               lines(data[,TIME.id],data[,DV.id],col=2,lwd=2)
            }   
         }
   
         savedata<-function(h,...)
         {  header<-strsplit(EDAfileName,"\\.")[[1]]
            select.id<-svalue(IDlist)
            sel.data<-NULL
            for(i in select.id)
            {  sel.id<-which(EDA.data$X.ID==i)
               sel.data<-rbind(sel.data,EDA.data[sel.id,]) 
            }   
            write.csv(sel.data,paste(gfile(text="Save as csv",
               type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
         }
         IDlist <- gtable(ID,multiple=T,handler=updatePlot) 
         window <- gwindow("DV vs TIME by ID")
         Biggroup<-ggroup(cont=window,horizontal=TRUE)
         group<-ggroup(cont=Biggroup,horizontal=FALSE)
         tmp<-gframe("ID",container=group)
         size(IDlist)<-c(100,200)
         add(tmp,IDlist)
         button1<-gbutton("OK",handler=updatePlot)
         tmp<-gframe("Display selected IDs",container=group)
         add(tmp,button1)
         button2<-gbutton("Save",handler=savedata)     
         tmp<-gframe("Save selected IDs",container=group)
         add(tmp,button2)
         add(Biggroup,tmp)
         add(Biggroup, ggraphics())
         Dev.IDplot<-dev.cur()
         par(mfrow=c(1,1))
         plot(D.data[,TIME.id],D.data[,DV.id],type='n',xlab="TIME",ylab="DV")
         id<-names(table(D.data[,ID.id]))
         for(i in 1:length(id))
         {  data<-D.data[which(D.data[,ID.id]==as.numeric(id[i])),]
            data<-data[!is.na(data[,DV.id]),]
            lines(data[,TIME.id],data[,DV.id])
         }
      }
   }

   ###################################
   # DV vs TIME by ID and Covariates
   ###################################
   IDCOV.plot<-function(h,...)
   {  D.data<-EDA.data
      ID.id<-NULL
      if(sum(colnames(D.data)=="X.ID")!=0)
         ID.id<-which(colnames(D.data)=="X.ID")
      TIME.id<-which(tolower(colnames(D.data))=="time")
      DV.id<-which(tolower(colnames(D.data))=="dv")
      Var.Name<-colnames(D.data)
      cov.list<-1:ncol(D.data)
      COV.data<-as.matrix(D.data[,c(ID.id,cov.list)])
      colnames(COV.data)<-c("ID",Var.Name[cov.list])
      sample.data<-D.data
      sample.data[is.na(sample.data)]<-"NA"
      colnames(sample.data)<-Var.Name

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
         if(cond1.V!="NONE     ")
         {  if(!is.na(cond1.F))
            {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]>=cond1.F)
               select.data<-select.data[selected.id,]
            }
            if(!is.na(cond1.T))
            {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]<=cond1.T)
               select.data<-select.data[selected.id,]
            }
         }
         if(cond2.V!="NONE     ")
         {  if(!is.na(cond2.F))
            {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]>=cond2.F)
               select.data<-select.data[selected.id,]
            }
            if(!is.na(cond2.T))
            {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]<=cond2.T)
               select.data<-select.data[selected.id,]
            }
         }
         if(nrow(select.data)==0)
         {  gmessage("No data!",title="Error",icon="error") 
         } else  if(svalue(VarType.g1)!="Categorical-multiple levels per person")
         {  id<-names(table(select.data[,1]))
            data<-D.data[which(D.data[,ID.id]==id[1]),c(ID.id,DV.id,TIME.id)]
               data<-data[!is.na(data[,2]),]
               dev.set(which=Dev.IDCOVplot)
               plot(data[,3],data[,2],type='l',xlab="TIME",ylab="DV",
                  xlim=range(D.data[,TIME.id],na.rm=T),ylim=range(D.data[,DV.id],na.rm=T))
               if(length(id)!=1)
                  for(i in 2:length(id))
                  {  data<-D.data[which(D.data[,ID.id]==id[i]),c(ID.id,DV.id,TIME.id)]
                     data<-data[!is.na(data[,2]),]
                     lines(data[,3],data[,2])
                  }
         } else
         {  id<-names(table(select.data[,1]))
            data.D<-EDA.data
            data.D<-data.D[!is.na(data.D[,"DV"]),]
            dev.set(which=Dev.IDCOVplot)
            plot(data.D[,"TIME"],data.D[,"DV"],type='n',xlab="TIME",ylab="DV",
                  xlim=range(data.D[,"TIME"],na.rm=T),ylim=range(data.D[,"DV"],na.rm=T))
            for(i in 1:length(id))
            {  data<-select.data[which(select.data[,"X.ID"]==id[i]),c(ID.id,DV.id,TIME.id)]
               data<-data[!is.na(data[,"DV"]),]
               lines(data[,"TIME"],data[,"DV"])
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
         if(cond1.V!="NONE")
         {  if(!is.na(cond1.F))
            {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]>=cond1.F)
               select.data<-select.data[selected.id,]
            }
            if(!is.na(cond1.T))
            {  selected.id<-which(select.data[,which(Var.Name==cond1.V)]<=cond1.T)
               select.data<-select.data[selected.id,]
            }
         }
         if(cond2.V!="NONE")
         {  if(!is.na(cond2.F))
            {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]>=cond2.F)
               select.data<-select.data[selected.id,]
            }
            if(!is.na(cond2.T))
            {  selected.id<-which(select.data[,which(Var.Name==cond2.V)]<=cond2.T)
               select.data<-select.data[selected.id,]
            }
         }
         if(nrow(select.data)==0)
         {  gmessage("No data!",title="Error",icon="error") 
         } else  if(svalue(VarType.g1)!="Categorical-multiple levels per person")
         {  id<-names(table(select.data[,1]))
            data<-NULL
            for(i in 1:length(id))
               data<-rbind(data,D.data[which(D.data[,ID.id]==id[i]),])
            write.csv(data,paste(gfile(text="Save as csv",
               type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
         } else
         {  write.csv(select.data,paste(gfile(text="Save as csv",
               type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)

         }      
      }

      From.label.1<-glabel("From")
      To.label.1<-glabel("To")
      VarList.g1<-gdroplist(c("NONE     ",Var.Name))
      VarType.g1<-gradio(c("Categorical-single level per person","Categorical-multiple levels per person","Continuous"))
      From.id.g1<-gedit(" ",width=10)
      To.id.g1<-gedit(" ",width=10)
      From.label.2<-glabel("From")
      To.label.2<-glabel("To")
      VarList.g2<-gdroplist(c("NONE     ",Var.Name))
      VarType.g2<-gradio(c("Categorical-single level per person","Categorical-multiple levels per person","Continuous"))
      From.id.g2<-gedit(" ",width=10)
      To.id.g2<-gedit(" ",width=10)
      Button1<-gbutton("OK",handler=updatePlot)
      Button2<-gbutton("Save",handler=saveData)
      win<-gwindow("DV vs TIME by covariates")
      BigGroup<-ggroup(cont=win)
      group<-ggroup(horizontal=FALSE,cont=BigGroup)
      tmp<-gframe("Covariate 1 : ",container=group)
      add(tmp,VarList.g1)
      add(tmp,VarType.g1)
      tmp<-gframe("Select  : ",container=group)
      add(tmp,From.label.1);add(tmp,From.id.g1)
      add(tmp,To.label.1);add(tmp,To.id.g1)
      tmp<-gframe("Covariate 2 : ",container=group)
      add(tmp,VarList.g2)
      add(tmp,VarType.g2)
      tmp<-gframe("Select :",container=group)
      add(tmp,From.label.2);add(tmp,From.id.g2)
      add(tmp,To.label.2);add(tmp,To.id.g2)
      tmp<-gframe("Plot",container=group)
      add(tmp,Button1,expand=TRUE)
      tmp<-gframe("Save",container=group)
      add(tmp,Button2,expand=TRUE)   
      add(BigGroup,ggraphics())
      Dev.IDCOVplot<-dev.cur()
   }

   ###################################
   # COV vs COV
   ###################################
   COVvsCOV.plot<-function(h,...)
   {  CovPlot<-function()
      {  updatePlot<-function(h,...)
         { 
           ID.id<-NULL
           Var.Name<-Cov.list
           if(sum(colnames(EDA.data)=="X.ID")!=0)
            ID.id<-which(colnames(EDA.data)=="X.ID")

           D.data<-EDA.data
         DD1<-svalue(VarList.cond1)
         DD1.F<-svalue(From.id.cond1)
         DD1.T<-svalue(To.id.cond1)
         DD2<-svalue(VarList.cond2)
         DD2.F<-svalue(From.id.cond2)
         DD2.T<-svalue(To.id.cond2)

         if(DD1!="NONE     ")
         {  if(DD2!="NONE     ")
            {  if(DD1.F==" " | DD2.F==" " | DD1.T ==" " |DD2.T==" ")
               { gmessage("Enter select condition",icon="error")
               } else
               {  sel.id<-which(D.data[,DD1]>=as.numeric(svalue(DD1.F)) & D.data[,DD1]<=as.numeric(svalue(DD1.T)) &
                              D.data[,DD2]>=as.numeric(svalue(DD2.F)) & D.data[,DD2]<=as.numeric(svalue(DD2.T)) )
               }
            } else
            {  sel.id<-which(D.data[,DD1]>=as.numeric(svalue(DD1.F)) & D.data[,DD1]<=as.numeric(svalue(DD1.T)))
            } 
         } else
         { sel.id<-1:nrow(D.data)
         }

         temp.data<-EDA.data[sel.id,]
         n.table<-tapply(rep(1,nrow(temp.data)),temp.data$X.ID,sum)
         temp<-apply(temp.data[1:n.table[1],],2,function(x) median(x,na.rm=T))
      
         for(i in 2:length(n.table)) 
             temp<-rbind(temp,apply(temp.data[(sum(n.table[1:(i-1)])+1):sum(n.table[1:i]),],2,function(x) median(x,na.rm=T)))

         D.data<-temp[,c("X.ID",Cov.list)]
         DD<<-D.data

      
         ID.id<-NULL
         Var.Name11<-colnames(D.data)
         if(sum(colnames(D.data)=="X.ID")!=0)
            ID.id<-which(colnames(D.data)=="X.ID")
         condX.V <-svalue(VarList.X)
         condY.V<-svalue(VarList.Y)
         select.data<-D.data
            if(!is.na(condX.V)& !is.na(condY.V))
            {   X<-select.data[,condX.V]; if(svalue(VarType.g1)=="Categorical") X<-factor(X)
                Y<-select.data[,condY.V]; if(svalue(VarType.g2)=="Categorical") Y<-factor(Y)
                dev.set(which=Dev.COVvsCOVplot)  
                plot(X,Y,xlab=condX.V,ylab=condY.V)
                if(svalue(VarType.g1)!="Categorical" & svalue(VarType.g2)!="Categorical")lines(lowess(X,Y),col=2,lwd=2)
            } 
         }

         saveData<-function(h,...)
         {  condX.V <-svalue(VarList.X,index=T)+1
            condY.V<-svalue(VarList.Y,index=T)+1
            select.data<-D.data[,c(ID.id,condX.V,condY.V)]
            write.csv(select.data,paste(gfile(text="Save as csv",
                  type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
          
         }

         Var.Name<-Cov.list
         From.label.1<-glabel("From")
         To.label.1<-glabel("To")
         VarList.cond1<-gdroplist(c("NONE     ",Var.Name))
         From.id.cond1<-gedit(" ",width=10)
         To.id.cond1<-gedit(" ",width=10)

         From.label.2<-glabel("From")
         To.label.2<-glabel("To")
         VarList.cond2<-gdroplist(c("NONE     ",Var.Name))
         From.id.cond2<-gedit(" ",width=10)
         To.id.cond2<-gedit(" ",width=10)

         VarList.X<-gdroplist(Var.Name)
         VarType.g1<-gradio(c("Categorical","Continuous"))
         VarList.Y<-gdroplist(Var.Name)
         VarType.g2<-gradio(c("Categorical","Continuous"))
         Button1<-gbutton("OK",handler=updatePlot)
         Button2<-gbutton("SAVE",handler=saveData)
    
         win<-gwindow("Covariate vs covariate plot")
         BigGroup<-ggroup(cont=win)
         group<-ggroup(horizontal=FALSE,cont=BigGroup)
         tmp<-gframe(" Select ",container=group)
         add(tmp,VarList.cond1)
         add(tmp,From.label.1);add(tmp,From.id.cond1)
         add(tmp,To.label.1);add(tmp,To.id.cond1)
         tmp<-gframe(" Select ",container=group)
         add(tmp,VarList.cond2)
         add(tmp,From.label.2);add(tmp,From.id.cond2)
         add(tmp,To.label.2);add(tmp,To.id.cond2)

         tmp<-gframe(" Covariate 1 : X-axis",container=group)
         add(tmp,VarList.X)
         add(tmp,VarType.g1)
         tmp<-gframe(" Covariate 2 : Y-axis",container=group)
         add(tmp,VarList.Y)
         add(tmp,VarType.g2)

         tmp<-gframe("Plot",container=group)
         add(tmp,Button1,expand=TRUE)
         tmp<-gframe("Save",container=group)
         add(tmp,Button2,expand=TRUE)
         add(BigGroup,ggraphics())
         Dev.COVvsCOVplot<-dev.cur()
      }

      saveCov<-function(h,...)
      {  Cov.list<<-Var.Name1[svalue(groupcheck)]
         dispose(checkg)
         CovPlot()
      }

      Var.Name1<-colnames(EDA.data)
      checkg<-gwindow("Covariate selection",width=200,height=400)
      groupcheck<-gcheckboxgroup(Var.Name1,cont=checkg,use.table=TRUE)
      Button1<-gbutton("OK",type="OK",handler=saveCov,cont=checkg)
   }

   ###################################
   # Edit with default Editor

   ###################################

   EditEditor<-function(h,...)
   {  file.ctl<-gfile(text="Choose NONMEM control file",type="open")
      D.temp<-readLines(file.ctl)
      Current.CTL<<-D.temp
      NONMEM.CTL<<-D.temp
      if(is.null(Current.CTL))
      {  edit.txt<-" "
      } else
      {  edit.txt<-Current.CTL[1]
         for(i in 2:length(Current.CTL))
            edit.txt<-paste(edit.txt,Current.CTL[i],sep="\n")
      }
      dir.name<-strsplit(file.ctl,split="\\.")[[1]][1]
      temp<-strsplit(file.ctl,"\\\\")[[1]]
      file.name<-temp[length(temp)]    

      setwd(strsplit(file.ctl,split=file.name)[[1]][1])

      edit.win<-gwindow(temp[length(temp)])
      g<-ggroup(horizontal=FALSE,cont=edit.win)
     	tmp<-gframe("Editor",container=g)
      a<-gtext(edit.txt,width=600,height=300,
                font.attr=c(sizes="large",family="monospace"))
      add(tmp,a)
      save.b<-gbutton("Save",handler=function(h,...){ 
               write.table(svalue(a),file.ctl,quote=FALSE,row.names=FALSE,col.names=FALSE)})
      saveas.b<-gbutton("Save as",handler=function(h,...){ 
                 file.new<-gfile(text="Save as",type="save")
                 write.table(svalue(a),file.new,quote=FALSE,row.names=FALSE,col.names=FALSE)
                 dispose(edit.win)
                 Editor1(file.new)})
      gb<-ggroup(horizontal=TRUE,cont=g)
      tmp<-gframe("",container=gb)  
      add(tmp,save.b)
      tmp<-gframe("",container=gb)  
      add(tmp,saveas.b)
   }

   ###################################
   # Edit with external Editor
   ###################################

   ExternalEditor<-function(h,...)
   {  control.path<-gfile(text="Choose control file",type="open")
      temp<-strsplit( Editor.path,split="\\\\")[[1]]
      Editor.name<-temp[length(temp)]
      Editor.path.t<-strsplit(Editor.path,split=Editor.name)[[1]]
      setwd(Editor.path.t)
      system(paste(Editor.name,control.path),invisible=F,wait=F)
   }


   ###################################
   # Notes before run
   ###################################
  
   BeforeRun<-function(h,...)
   {  gmessage("*** Notes before run ***
           \nThe file name of a control stream should be runnumber.ctl.
           \nID should be #ID.
           \nIndividual predictions = IPRED  
           \nIndividual weighted residuals = IWRES 
           \nSemicolons used for comments in a NONMEM control stream should be preceded by one space as follows.
           \n  $THETA ; #8     (O)
           \n  $THETA; #8      (X)
           \n$TABLE statements to generate runnumber.eta (ID, ETA(1), ETA(2), ...) and runnumber.par (ID, V, CL, ...) should be included in a control stream.
           \nPrior to the beginning of a run,
           \n        $TABLE statements to generate runnumber.noh is inserted automatically in a control stream.
           \n        Control and data input files are copied automatically to a runnumber subfolder. 
           \n        The data input file copied to a runnumber subfolder is duplicated automatically to runnumber.csv.
           \nThe number of data records in several ouput files should be equal to that of data input file (ACCEPT argument in $DATA is not allowed).
           \nModel description should be written in English and should not include comma(,) separators",cont=T)
   }


    VPCNote<-function(h,...)
   {  gmessage("*** Notes before model evaluations  ***
                \nRadomization test, predictive checks and bootstrap should be conducted within a runnumber subfolder.
                \nRandomization test permutes the median value of a time invariant covariate across every time point in an individual.
                \nSimilary, the median value of a time varying covariate in an individual is permuted. 
                \nThe result of randomization test for time varying covariates should not be adopted. 
                \nFor, predictive checks!
                \n     Please copy the original control file to other folder.
                \n     Rename it to anyname.ctl and move it to runnumber subfolder.             
                \n     All model parameters (THETA, ETA, EPSILON) in anyname.ctl should be replaced by final estimates.          
                \n     Otherwise, leave it as was in the original control file.                
                \n     After starting predictive checks, $EST, $COV, $TABLE blocks are removed and $TABLE block for PC.sim is inserted automatically.                   
                \n     Press \"calculate predictive checks\" button for every stratum when you use stratification.
                \n     All the variables used for stratification should be present at every time point. Please remember this, especially for the stratification based on the dosing data which are not present at every time point in data input file.
                \nFor the menus of open randomization test result, predictive checks with PC.sim and summary data from joined bootstrap raw data file.
                \n     All the conditions but seed number, under which the results of randomization test, predictive checks and bootstrap were obtained, should not be changed.
                \n     Each result file of the randomization test performed in separate PC contains information of the covariate model at the record of prob 0. When you join these result files, leave only one prob 0 data record.    ",cont=TRUE,width=500)   
   }  
   ###################################
   # NONMEM Run-From Editor
   ###################################

   Editor<-function(h,...)
   {  file.ctl<<-gfile(text="Choose NONMEM control file",type="open")
      Editor1<-function(file.ctl)
      {	 D.temp<-readLines(file.ctl)
      	 Current.CTL<-D.temp
      	 NONMEM.CTL<-D.temp
  	     if(is.null(Current.CTL))
      	 {  edit.txt<-" "
      	 } else
      	 {  edit.txt<-Current.CTL[1]
            for(i in 2:length(Current.CTL))
               edit.txt<-paste(edit.txt,Current.CTL[i],sep="\n")
      	 }
         dir.name<-strsplit(file.ctl,split="\\.")[[1]][1]
         temp<-strsplit(file.ctl,"\\\\")[[1]]
         file.name<-temp[length(temp)]    
         setwd(strsplit(file.ctl,split=file.name)[[1]][1])

      	 edit.win<-gwindow(temp[length(temp)])
      	 g<-ggroup(horizontal=FALSE,cont=edit.win)
     	   tmp<-gframe("Editor",container=g)
      	 a<-gtext(edit.txt,width=600,height=300,
                font.attr=c(sizes="large",family="monospace"))
      	 add(tmp,a)
      	 save.b<-gbutton("Save",handler=function(h,...){ 
            	write.table(svalue(a),file.ctl,quote=FALSE,row.names=FALSE,col.names=FALSE)})
      	 saveas.b<-gbutton("Save as",handler=function(h,...){ 
             	file.new<-gfile(text="Save As",type="save")
             	write.table(svalue(a),file.new,quote=FALSE,row.names=FALSE,col.names=FALSE)
             	dispose(edit.win)
             	Editor1(file.new)
             	file.ctl<<-file.new})
      	 gb<-ggroup(horizontal=TRUE,cont=g)
      	 tmp<-gframe("",container=gb)  
      	 add(tmp,save.b)
      	 tmp<-gframe("",container=gb)  
      	 add(tmp,saveas.b)
     	 RunNM.1<-gbutton("Run default NONMEM",handler=function(h,...){ 
                dir.create(dir.name,showWarnings=F)
                file.copy(file.ctl,dir.name,overwrite=T)
      	        temp<-strsplit(data.file,"\\\\")[[1]]
                data.name<-temp[length(temp)] 
                temp<-strsplit(dir.name,split="\\\\")[[1]]
             	file.id<-temp[length(temp)]
                file.copy(data.file,dir.name,overwrite=T)
                file.copy(data.file,paste(dir.name,"\\",file.id,".csv",sep=""),overwrite=T)
                
                NONMEM.command<-paste(Default.NMpath, file.name, paste(strsplit(file.name,split="\\.")[[1]][1],"res",sep="."),
                                     paste(">",strsplit(file.name,split="\\.")[[1]][1],".console",sep=""))
                setwd(dir.name)
                param.num<-as.numeric(svalue(num.param))
                Description<-svalue(model.description)
                tt<-strsplit(Description,"\n")[[1]]
                Description<-""
                for(i in 1:length(tt))
                   Description<-paste(Description,tt[i])

                temp<-strsplit(Default.NMpath,split="\\\\")[[1]]
                NMversion<-ifelse(temp[length(temp)]=="nmfe7.bat","NM7","NM6")
                write.table(NMversion,"NM.version")

                RemakeCTL(paste(dir.name,"\\",file.id,".ctl",sep=""))
                system(NONMEM.command,wait=F) 
          	OpenNMConsole(file.id,paste(dir.name,"\\",file.id,".console",sep=""))
                alarm()
                if(sum(dir()==paste(file.id,".ETA",sep=""))!=0)
             	{  TOT.temp<-TOT.RUN
                   TOT.temp$num<-TOT.temp$num+1
                   TOT.temp$data<-rbind(TOT.temp$data,c(file.id,dir.name,svalue(parent)))
                   colnames(TOT.temp$data)<-c("ID","path","parents")
                   TOT.RUN<<-TOT.temp
                   OpenResult(file.id,paste(dir.name,"\\",file.id,".res",sep=""))
                   D.LST<-readLines(paste(dir.name,"\\",file.id,".res",sep=""))
                   ETA<-read.table(paste(dir.name,"\\",file.id,".ETA",sep=""),skip=1,header=T)
                   temp.ETA<-colnames(ETA)
                   ET.id<-NULL
                   for(i in 1:length(temp.ETA))
                   {  if(length(strsplit(temp.ETA[i],"ET")[[1]])>1)
                         ET.id<-c(ET.id,i)
                   }
                   ETA<-ETA[,ET.id]              
        data.tempt<-read.csv(data.file,na.string=".")   
         if(sum(toupper(colnames(data.tempt))=="MDV")!=0)
         {  data.i<-which(toupper(colnames(data.tempt))=="MDV")
            temp<-data.tempt[,data.i]
            temp<-temp[temp==0]
            data.n<-length(temp)         
         } else
         {  data.n<-nrow(data.tempt)
         }
                   ShowResult1(D.LST,param.num,data.n,Description,ETA,file.id,dir.name)
                } else
                { 
                   gconfirm(paste("fitting failure : ",file.id,".ctl ",sep=""),icon="error")
                  
                }  
                })
                
      	 RunNM.2<-gbutton("Run alternative NONMEM",handler=function(h,...){ 
                dir.create(dir.name,showWarnings=F)
                file.copy(file.ctl,dir.name,overwrite=T)
      	        temp<-strsplit(data.file,"\\\\")[[1]]
                data.name<-temp[length(temp)] 
                temp<-strsplit(dir.name,split="\\\\")[[1]]
            	file.id<-temp[length(temp)]
                file.copy(data.file,dir.name,overwrite=T)
                file.copy(data.file,paste(dir.name,"\\",file.id,".csv",sep=""),overwrite=T)
                
                NONMEM.command<-paste(Alternative.NMpath, file.name, paste(strsplit(file.name,split="\\.")[[1]][1],"res",sep="."),
                                     paste(">",strsplit(file.name,split="\\.")[[1]][1],".console",sep=""))
                setwd(dir.name)
                param.num<-as.numeric(svalue(num.param))
                Description<-svalue(model.description)
                tt<-strsplit(Description,"\n")[[1]]
                Description<-""
                for(i in 1:length(tt))
                   Description<-paste(Description,tt[i])
                
                temp<-strsplit(Alternative.NMpath,split="\\\\")[[1]]
                NMversion<-ifelse(temp[length(temp)]=="nmfe7.bat","NM7","NM6")
                write.table(NMversion,"NM.version")

                RemakeCTL(paste(dir.name,"\\",file.id,".ctl",sep=""))
                system(NONMEM.command,wait=F) 
             	OpenNMConsole(file.id,paste(dir.name,"\\",file.id,".console",sep=""))
                alarm()
                
                if(sum(dir()==paste(file.id,".ETA",sep=""))!=0)
             	{                 
             	  TOT.temp<-TOT.RUN
                TOT.temp$num<-TOT.temp$num+1
                TOT.temp$data<-rbind(TOT.temp$data,c(file.id,dir.name,svalue(parent)))
                colnames(TOT.temp$data)<-c("ID","path","parents")
                TOT.RUN<<-TOT.temp
                OpenResult(file.id,paste(dir.name,"\\",file.id,".res",sep=""))
                D.LST<-readLines(paste(dir.name,"\\",file.id,".res",sep=""))
                ETA<-read.table(paste(dir.name,"\\",file.id,".ETA",sep=""),skip=1,header=T)
                temp.ETA<-colnames(ETA)
                ET.id<-NULL
                for(i in 1:length(temp.ETA))
                {  if(length(strsplit(temp.ETA[i],"ET")[[1]])>1)
                      ET.id<-c(ET.id,i)
                }
                ETA<-ETA[,ET.id]                              
        data.tempt<-read.csv(data.file,na.string=".")   
         if(sum(toupper(colnames(data.tempt))=="MDV")!=0)
         {  data.i<-which(toupper(colnames(data.tempt))=="MDV")
            temp<-data.tempt[,data.i]
            temp<-temp[temp==0]
            data.n<-length(temp)         
         } else
         {  data.n<-nrow(data.tempt)
         }
                ShowResult1(D.LST,param.num,data.n,Description,ETA,file.id,dir.name)
                } else
                { 
                   gconfirm(paste("NONMEM failure : ",file.id,".ctl ",sep=""),icon="error")
                  
                }  
                
                })           
         openrundata<-function(h,...)
         {  data.file<<-gfile(text="Choose data file",type="open")
            svalue(data.set)<-data.file
         }

         grunNM<-ggroup(horizontal=F,cont=g) 
         tmp<-gframe("Model description",cont=grunNM)
         model.description<-gtext(" ",width=250,height=20)
         add(tmp,model.description)

         tmp<-gframe("Choose parents",cont=grunNM)
         item<-unique(c("ROOT",TOT.RUN$data[,"ID"]))
         parent<-gdroplist(item)
         add(tmp,parent)
         
         tmp<-gframe("Number of parameters\n # of theta + # of omega and sigma(except fixed)",cont=grunNM)
         num.param<-gtext("",width=100,height=15)
         add(tmp,num.param)
         
         tmp<-gframe("NONMEM data file",cont=grunNM)
         data.set<-gtext(" ",width=400,height=30)
         add(tmp,data.set)
         open.but<-gbutton("Open",handler=openrundata)
         add(tmp,open.but)
                         
     	  gc<-ggroup(horizontal=TRUE,cont=g)
      	 tmp<-gframe("",container=gc)  
      	 add(tmp,RunNM.1)
      	 tmp<-gframe("",container=gc)  
      	 add(tmp,RunNM.2)
      }
      Editor1(file.ctl)
   }

  
   ###################################
   # Result after NM run #1
   ###################################

   CatchNMversion<-function(D.LST)
   {  D.temp<-matrix(D.LST)
      indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
      NM.id<- which(indicator=="1NONLINEAR")
      temp<- strsplit(D.LST[NM.id],split=" ")[[1]]
      NMversion<-ifelse(temp[which(temp=="VERSION")+1]=="VI",6,7)
      return(NMversion)
   }

   ShowResult1<-function(D.LST,param.num,data.n,Description,ETA,file.id,dir.name)
   {  RunID<-TOT.RUN$data[TOT.RUN$num,1]
      n.lst<-length(D.LST)
      Date<-D.LST[n.lst-1]
      Time<-D.LST[n.lst]

      D.temp<-matrix(D.LST)
      indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
      min.id<- which(indicator=="0MINIMIZATION")
      min.id<-min.id[length(min.id)]
      Min<-strsplit(D.LST[min.id],split=" ")[[1]][2]

      indicator<-apply(D.temp,1,function(x) strsplit(x,split=":")[[1]][1])

      cond.id<-grep(" EIGENVALUES ",D.LST)
      if(length(cond.id)!=0)
      {  
         flag<-T
         id.current<-cond.id+5
         cond.line<-0
         while(flag)
         {  ttemp<-D.LST[id.current]
            flag<-ttemp!=" "
            if(flag)
            {  cond.line<-cond.line+1
               id.current<-id.current+1
            }
         }
         temp<-NULL
         for(i in 1:cond.line)
            temp<-c(temp,strsplit(D.LST[cond.id+5+cond.line+i],split=" ")[[1]])
         temp<-as.numeric(temp[temp!=""&temp!="+"])
         cond.num<-round(max(temp)/min(temp),3)        
      } else
      {  cond.num<-NA
      }   
      
      obj.id<-which(indicator==" #OBJV")
      if(length(obj.id)!=0)
      { obj.id<-obj.id[length(obj.id)]
      } else
      { obj.id<-9+which(indicator==" ********************                           MINIMUM VALUE OF OBJECTIVE FUNCTION                  ********************" )
      } 
      temp<-strsplit(D.LST[obj.id],split=" ")[[1]]
      temp<-temp[3:(length(temp)-3)]
      temp<-as.numeric(temp[temp!=""])
      Obj<-temp[!is.na(temp)]
      AIC<-Obj+2*param.num
      AICc<-round(Obj+2*param.num+2*param.num*(param.num+1)/(data.n-param.num-1),3)
      SBC<-round(Obj+param.num*log(data.n),3)
      parent<-TOT.RUN$data[TOT.RUN$num,"parents"]

# choose THETA,seTHETA,OMEGA,seOMEGA,SIGMA,seSIGMA

      final.start.id<-grep("FINAL PARAMETER ESTIMATE",D.LST)
      final.start.id<-final.start.id[length(final.start.id)]
      Result.LST<-D.LST[final.start.id:length(D.LST)]

      theta.id<-grep("THETA",Result.LST)
      theta.line<-0
      theta.flag<-TRUE
      while(theta.flag)
      {  if(Result.LST[theta.id[1]+3+theta.line]!=" ")
         {  theta.line<-theta.line+1
         } else
         {  theta.flag<-FALSE
         }  
      }
      temp<-NULL
      for(i in 1:theta.line)
         temp<-c(temp,unlist(strsplit(Result.LST[theta.id[1]+3+theta.line+i],split=" ")))
      temp<-as.numeric(temp[temp!=""])
      THETA<-temp
      
      seTHETA<-rep(NA,length(THETA))  
      if(length(theta.id)!=1)
      {  temp<-NULL
         for(i in 1:theta.line)
            temp<-c(temp,unlist(strsplit(Result.LST[theta.id[2]+3+theta.line+i],split=" ")))
         temp<-as.numeric(temp[temp!=""])
         seTHETA<-temp
      } 

      omega.id<-grep("OMEGA",Result.LST)
      omega.line<-0
      omega.flag<-TRUE
      while(omega.flag)
      {  if(Result.LST[omega.id[1]+3+omega.line]!=" ")
         {  omega.line<-omega.line+1
         } else
         {  omega.flag<-FALSE
         }  
      }
      temp<-NULL
      for(i in 1:omega.line)
         temp<-c(temp,unlist(strsplit(Result.LST[omega.id[1]+2+i],split=" ")))
      temp<-temp[temp!=""]

      N.eta<-length(temp)
      OMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
      seOMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
      omega.name<-NULL
      id.current<-omega.id[1]+4+omega.line  
      id.current1<-omega.id[2]+4+omega.line  
      
      for(i in 1:N.eta)
      {  temp<-NULL;temp1<-NULL
         flag<-TRUE
         while(flag)
         {  id.current<-id.current+1;id.current1<-id.current1+1
            flag<-Result.LST[id.current]!=" "
            if(flag)
            {  temp<-c(temp,unlist(strsplit(Result.LST[id.current],split="+ ")))
               temp1<-c(temp1,unlist(strsplit(Result.LST[id.current1],split="+ ")))            
            }
         }
         temp<-temp[temp!=""];temp1<-temp1[temp1!=""]
         temp<-temp[temp!="+"] ;temp1<-temp1[temp1!="+"] 
         temp1[ temp1=="........."]<-NA 
         
         for(j in 1:N.eta)
         {  OMEGA[i,j]<-as.numeric(temp[j])
            seOMEGA[i,j]<-as.numeric(temp1[j])
            omega.name<-c(omega.name,paste("OMEGA(",i,"/",j,")",sep=""))
         }  
         id.current<-id.current+1 
         id.current1<-id.current1+1              
      }     

      sigma.id<-grep("SIGMA",Result.LST)
      temp<-unlist(strsplit(Result.LST[sigma.id[1]+6],split="  "))
      temp<-temp[temp!=""]; temp<-temp[temp!="+"]
      SIGMA<-as.numeric(temp)
      seSIGMA<-rep(NA,length(SIGMA))
      if(length(theta.id)!=1)
      {  temp<-unlist(strsplit(Result.LST[sigma.id[2]+6],split="  "))
         temp<-temp[temp!=""]; temp<-temp[temp!="+"]
         seSIGMA<-as.numeric(temp)      
      } 

      names.est<-c(paste("TH",1:length(THETA),sep=""),omega.name,paste("SIGMA",1:length(SIGMA)))      
      EST<-c(THETA,OMEGA,SIGMA)
      SE<-c(seTHETA,seOMEGA,seSIGMA)

      RSE<-round(SE/EST*100,4)
      Lower<-round(EST-1.96*SE,4)
      Upper<-round(EST+1.96*SE,4)

      if(sum(!is.na(seTHETA))==0)
      {  COV<-"NONE"
         cond.num<-NA
      } else
      {  COV<-"OK"
      }

      temp<-c(RunID,Date,Time,Min,COV,Obj,AIC,AICc,SBC,cond.num,parent,Description,param.num)
      if(TOT.RUN$num>2)
      {  run.table[]<-rbind(run.table[],temp)
      } else
      {  run.table[][TOT.RUN$num,]<-temp
      }

      se.ETA<-apply(ETA,2,sd) 
      shrinkage.ETA<-matrix(NA,ncol=N.eta,nrow=N.eta)
      diag(shrinkage.ETA)<-(1-se.ETA/sqrt(diag(OMEGA)))*100
      shrinkage<-c(rep("NA",length(THETA)),round(shrinkage.ETA,3),rep("NA",length(SIGMA)))

      CV.ETA<-matrix(NA,ncol=N.eta,nrow=N.eta)
      diag(CV.ETA)<-sqrt(diag(OMEGA))*100
      CV<-c(rep("NA",length(THETA)),round(CV.ETA,3),rep("NA",length(SIGMA)))
      
      tot.res<-cbind(names.est,EST,SE,RSE,Lower,Upper,shrinkage,CV)
      colnames(tot.res)<-c("Parameters","Estimates","SE","%RSE","Lower","Upper","%Shrinkage","%CV")
      tot.res[is.na(tot.res)| is.nan(tot.res) | tot.res=="NA"| tot.res=="NaN"]<-" "
      tot.res<-tot.res[-which(apply(tot.res,1,function(x) sum(x==" "))==7),]
      gtable(tot.res, cont=gwindow(paste(file.id,".sum",sep="")),do.subset=TRUE,width=150)
      write.csv(tot.res,paste(dir.name,"\\",file.id,".sum",sep=""),quote=F)
      write.csv(tot.res,paste(dir.name,"\\",file.id,".sum.csv",sep=""),quote=F)      
   }
   
 
   ###################################
   # External Run
   ###################################   

   ExternalRun<-function(h,...)
   {  opendata<-function(h,...)
      {  data.file<<-gfile(text="Open data file",type="open")
         svalue(data.t)<-data.file      
      }

      openControl<-function(h,...)
      {  control.file<<-gfile(text="Open control file (runnumber.ctl)",type="open")
         svalue(control.t)<-control.file
         dir.name<-strsplit(control.file,split="\\.")[[1]][1]
         svalue(dir.t)<-dir.name

         temp<-strsplit(dir.name,split="\\\\")[[1]]
         file.id<-temp[length(temp)]
         setwd(strsplit(dir.name,split=file.id)[[1]])
       }

      openEdtRun<-function(h,...)
      {  dir.name<-strsplit(control.file,split="\\.")[[1]][1]
         dir.create(dir.name,showWarnings=F)
         temp<-strsplit(control.file,split="\\\\")[[1]]
         file.id<-temp[length(temp)]
         file.id1<-strsplit(file.id,split="\\.")[[1]][1]
         setwd(dir.name)
         file.copy(control.file,dir.name,overwrite=T)
         file.copy(data.file,dir.name,overwrite=T)
         file.copy(data.file,paste(dir.name,"\\",file.id1,".csv",sep=""),overwrite=T)
         
         temp<-strsplit(Default.NMpath,split="\\\\")[[1]]
         NMversion<-ifelse(temp[length(temp)]=="nmfe7.bat","NM7","NM6")
         write.table(NMversion,"NM.version")
         RemakeCTL(file.id)

         temp<-strsplit( Editor.path,split="\\\\")[[1]]
         Editor.name<-temp[length(temp)]
         Editor.path.t<-strsplit(Editor.path,split=Editor.name)[[1]]
         setwd(Editor.path.t)
         system(paste(Editor.name,paste(dir.name,"\\",file.id,sep="")),wait=F)
         setwd(dir.name)
      }
 
      Outerwin<-gwindow("Run from external editor")
      Bgroup<-ggroup(cont=Outerwin,horizontal=TRUE)
      BBgroup<-ggroup(cont=Bgroup,horizontal=FALSE)
 
      tmp<-gframe("",cont=BBgroup)
      control.t<-gedit("",width=50)
      button1<-gbutton("Open control file",handler=openControl)
      add(tmp,button1)
      add(tmp,control.t)

      tmp<-gframe("",cont=BBgroup)
      button2<-gbutton("Open data files",handler=opendata,width=20,height=10)
      data.t<-gedit(" ",width=50)
      add(tmp,button2)
      add(tmp,data.t) 
      tmp<-gframe("NONMEM run number directory",cont=BBgroup)
      dir.t<-gedit(" ",width=50)
      add(tmp,button2)
      add(tmp,dir.t) 
 
      tmp<-gframe("",cont=BBgroup)
      button3<-gbutton("Open external editor for run",handler=openEdtRun,width=20,height=10)
      add(tmp,button3)   
   }


   ###################################
   # NONMEM Run-Direct Run
   ###################################

   DirectRun<-function(h,...)
   {  RunNONMEM<-function(id)
      {  print(id)
         i<-id#i<-which(DirectRunNum==id)
         data.file<-tclvalue(DataFile.Name[[i]])
         file.ctl<-tclvalue(ControlFile.Name[[i]])
         param.num<-as.numeric(tclvalue(Param.Num[[i]]))
         Description<-tclvalue(Description.N[[i]])
         parents<-toupper(tclvalue(Parent.Num[[i]]) )
         dir.name<-strsplit(file.ctl,split="\\.")[[1]][1]
         temp<-strsplit(file.ctl,"\\\\")[[1]]
         file.name<-temp[length(temp)]              
         dir.create(dir.name,showWarnings=F)
         file.copy(file.ctl,paste(dir.name,file.name,sep="\\"),overwrite=T)
         temp<-strsplit(data.file,"\\\\")[[1]]
         data.name<-temp[length(temp)] 
         file.id<-strsplit(data.name,"\\.")[[1]][1]
         file.copy(data.file,paste(dir.name,data.name,sep="\\"),overwrite=T)
         file.copy(data.file,paste(dir.name,"\\",file.id,".csv",sep=""),overwrite=T)

         NONMEM.command<-paste(Default.NMpath, file.name, paste(strsplit(file.name,split="\\.")[[1]][1],"res",sep="."),
                               paste(">",strsplit(file.name,split="\\.")[[1]][1],".console",sep=""))
         setwd(dir.name)
         temp<-strsplit(Default.NMpath,split="\\\\")[[1]]
         NMversion<-ifelse(temp[length(temp)]=="nmfe7.bat","NM7","NM6")
         write.table(NMversion,"NM.version")

         RemakeCTL(paste(dir.name,file.name,sep="\\"))
         system(NONMEM.command,invisible=F,show.output.on.console=F,wait=F) 
         OpenNMConsole(file.id,paste(dir.name,"\\",file.id,".console",sep=""))
         alarm()
         if(sum(dir()==paste(file.id,".ETA",sep=""))!=0)
         {                    
         temp<-strsplit(dir.name,split="\\\\")[[1]]
         file.id<-temp[length(temp)]
         TOT.temp<-TOT.RUN
         TOT.temp$num<-TOT.temp$num+1
         TOT.temp$data<-rbind(TOT.temp$data,c(file.id,dir.name,parents))
         colnames(TOT.temp$data)<-c("ID","path","parents")
         TOT.RUN<<-TOT.temp

         OpenResult(file.id,paste(dir.name,"\\",file.id,".res",sep=""))
         D.LST<-readLines(paste(dir.name,"\\",file.id,".res",sep=""))
         ETA<-read.table(paste(dir.name,"\\",file.id,".ETA",sep=""),skip=1,header=T)
         temp.ETA<-colnames(ETA)
         ET.id<-NULL
         for(i in 1:length(temp.ETA))
         {  if(length(strsplit(temp.ETA[i],"ET")[[1]])>1)
               ET.id<-c(ET.id,i)
         }
         ETA<-ETA[,ET.id]           
         data.tempt<-read.csv(data.file,na.string=".")   
         if(sum(toupper(colnames(data.tempt))=="MDV")!=0)
         {  data.i<-which(toupper(colnames(data.tempt))=="MDV")
            temp<-data.tempt[,data.i]
            temp<-temp[temp==0]
            data.n<-length(temp)         
         } else
         {  data.n<-nrow(data.tempt)
         }
         
         ShowResult1(D.LST,param.num=param.num,data.n,Description=Description,ETA,file.id,dir.name)               
                } else
                { 
                   gconfirm(paste("fitting failure : ",file.id,".ctl ",sep=""),icon="error")
                  
                }  

      }

#      SimRun<-function()
#      {  runA<-function(h,...)
#         {   for(i in 1:length(a))
#             { id<-which(DirectRunNum==svalue(a)[i])
#               RunNONMEM(id)
#             }  
#         }
#         checkg<-gwindow("Multiple Runs",width=300,height=20*k)
#         gcont<-ggroup(cont=checkg,horizontal=F)
 #        a<-gcheckboxgroup(as.character(DirectRunNum),cont=gcont)
#         a.button<-gbutton("OK",cont=gcont,handler=runA)
        
         
         
#         ,handler=function(h,...)
#                          {  id<-svalue(h$obj); print(as.character(DirectRunNum[i]))
#                             RunNONMEM(id)  
#                          } ,use.table=TRUE)
#               
#         for(i in 1:k)
#         {  gcheckbox(as.character(DirectRunNum[i]),cont=gcont,handler=function(h,...)
#                          {  id<-svalue(h$obj); print(as.character(DirectRunNum[i]))
#                             RunNONMEM(id)  
#                          } ,use.table=TRUE)
#         }
#      }

      SeqRun<-function()
      {  for(i in 1:k)
         {  data.file<-tclvalue(DataFile.Name[[i]])
            file.ctl<-tclvalue(ControlFile.Name[[i]])
            param.num<-as.numeric(tclvalue(Param.Num[[i]]))
            Description<-tclvalue(Description.N[[i]])
            parents<-toupper(tclvalue(Parent.Num[[i]]) )

            dir.name<-strsplit(file.ctl,split="\\.")[[1]][1]
            temp<-strsplit(file.ctl,"\\\\")[[1]]
            file.name<-temp[length(temp)]              
            dir.create(dir.name,showWarnings=F)
            file.copy(file.ctl,paste(dir.name,file.name,sep="\\"),overwrite=T)
            temp<-strsplit(data.file,"\\\\")[[1]]
            data.name<-temp[length(temp)] 
            file.copy(data.file,paste(dir.name,data.name,sep="\\"),overwrite=T)
            file.id<-strsplit(file.name,split="\\.")[[1]][1]
            file.copy(data.file,paste(dir.name,"\\",file.id,".csv",sep=""),overwrite=T)

            NONMEM.command<-paste(Default.NMpath, paste(file.id,".ctl",sep=""), paste(file.id,"res",sep="."),
                                     paste(">",file.id,".console",sep=""))
            setwd(dir.name)
            temp<-strsplit(Default.NMpath,split="\\\\")[[1]]
            NMversion<-ifelse(temp[length(temp)]=="nmfe7.bat","NM7","NM6")
            write.table(NMversion,"NM.version")

            RemakeCTL(paste(dir.name,file.name,sep="\\"))
            system(NONMEM.command,invisible=F,show.output.on.console=F,wait=F)
            OpenNMConsole(file.id,paste(dir.name,"\\",file.id,".console",sep=""))           
            alarm()
         if(sum(dir()==paste(file.id,".ETA",sep=""))!=0)
         {                          
            temp<-strsplit(dir.name,split="\\\\")[[1]]
            file.id<-temp[length(temp)]
 
            TOT.temp<-TOT.RUN
            TOT.temp$num<-TOT.temp$num+1
            TOT.temp$data<-rbind(TOT.temp$data,c(file.id,dir.name,parents))
            colnames(TOT.temp$data)<-c("ID","path","parents")
            TOT.RUN<<-TOT.temp

            OpenResult(file.id,paste(dir.name,"\\",file.id,".res",sep=""))      
            D.LST<-readLines(paste(dir.name,"\\",file.id,".res",sep=""))
            ETA<-read.table(paste(dir.name,"\\",file.id,".ETA",sep=""),skip=1,header=T)
            temp.ETA<-colnames(ETA)
            ET.id<-NULL
            for(i in 1:length(temp.ETA))
            {  if(length(strsplit(temp.ETA[i],"ET")[[1]])>1)
                  ET.id<-c(ET.id,i)
            }
            ETA<-ETA[,ET.id]              
        data.tempt<-read.csv(data.file,na.string=".")   
         if(sum(toupper(colnames(data.tempt))=="MDV")!=0)
         {  data.i<-which(toupper(colnames(data.tempt))=="MDV")
            temp<-data.tempt[,data.i]
            temp<-temp[temp==0]
            data.n<-length(temp)         
         } else
         {  data.n<-nrow(data.tempt)
         }
            ShowResult1(D.LST,param.num=param.num,data.n,Description=Description,ETA,file.id,dir.name)                 
                } else
                { 
                   gconfirm(paste("fitting failure : ",file.id,".ctl ",sep=""),icon="error")
                  
                }  

         }
      }

      Add<-function()
      {  AddLine(k+1)
      }

      AddLine<-function(id)
      {  k<<-id
         ControlFile.Name[[id]]<<-tclVar("")
         ControlFileName[[id]]<<-tkentry(tt,width="20",textvariable=ControlFile.Name[[id]])
         DataFile.Name[[id]]<<-tclVar("")
         DataFileName[[id]]<<-tkentry(tt,width="20",textvariable=DataFile.Name[[id]])
         Run.Num[[id]]<<-tclVar("")
         RunNum[[id]]<<-tkentry(tt,width="15",textvariable=Run.Num[[id]])
         Description.N[[id]]<<-tclVar("")
         DescriptionN[[id]]<<-tkentry(tt,width="60",textvariable=Description.N[[id]])
         Param.Num[[id]]<<-tclVar("")
         ParamNum[[id]]<<-tkentry(tt,width="15",textvariable=Param.Num[[id]])
         Parent.Num[[id]]<<-tclVar("")
         ParentNum[[id]]<<-tkentry(tt,width="15",textvariable=Parent.Num[[id]])
         tkgrid(ControlFileName[[id]],RunNum[[id]],tklabel(tt,text=" "),ParentNum[[id]],
                   tklabel(tt,text=" "),ParamNum[[id]], tklabel(tt,text=" "),
                      DescriptionN[[id]],DataFileName[[id]],tklabel(tt,text=" "))
         tkgrid(tt)
      }

      ConFile<-function()
      {  kk<-k
         tclvalue(ControlFile.Name[[kk]])<<-gfile(text="Open control file (runnumber.ctl)",type="open")
         file.ctl<-tclvalue(ControlFile.Name[[kk]])
         temp<-strsplit(file.ctl,"\\\\")[[1]]
         RunNumber<-strsplit(tolower(temp[length(temp)]),split="\\.ctl")[[1]][1]  
         tclvalue(Run.Num[[kk]])<<-RunNumber
         DirectRunNum<<-c(DirectRunNum,RunNumber)
      }

      DataFile<-function()
      {  kk<-k
         file.ctl<-tclvalue(ControlFile.Name[[kk]])
         temp<-strsplit(file.ctl,"\\\\")[[1]]
         file.name<-temp[length(temp)]
         dir.name<-strsplit(file.ctl,split=file.name)[[1]] 
         setwd(dir.name)
         tclvalue(DataFile.Name[[kk]])<<-gfile(text="Data file",type="open")
      }
      
      SaveRun<-function(h,...)
      {  TOT.table<-NULL
         for(i in 1:k)
         {  data.file<-tclvalue(DataFile.Name[[i]])
            file.ctl<-tclvalue(ControlFile.Name[[i]])
            param.num<-tclvalue(Param.Num[[i]])
            Description<-tclvalue(Description.N[[i]])
            parents<-toupper(tclvalue(Parent.Num[[i]]))
            Runnum<-tclvalue(Run.Num[[i]])
            TOT.table<-rbind(TOT.table,c(file.ctl,Runnum,parents,param.num,Description,data.file))
          }  
          colnames(TOT.table)<-c("ControlFile","Runnumber","Parents","paramnum","Description","DataFile")
          run.file<-paste(gfile(text="Save as csv",type="save"),".csv",sep="")
          write.csv(TOT.table,run.file)
      }
      
      DirectRunNum<<-NULL
      Toptt<<-tktoplevel()
      tkwm.title(Toptt,"Direct run")
      tt<-tkframe(Toptt)
      ttg<-tkframe(tt)
      OK.but3 <-tkbutton(ttg,text="Sequential runs",command=SeqRun)
      OK.but4 <-tkbutton(ttg,text="Save as csv",command=SaveRun)
      tkgrid(tklabel(ttg,text=""),tklabel(ttg,text=""),tklabel(ttg,text=""),
                     OK.but3,tklabel(ttg,text=""),OK.but4)
      tkgrid(ttg)
      OK.but1 <-tkbutton(tt,text="Control file (runnumber.ctl)",command=ConFile)
      OK.but2 <-tkbutton(tt,text="Data file",command=DataFile)
      Add.but<-tkbutton(tt,text="Add",command=Add)
      
      tkgrid(OK.but1,tklabel(tt,text="Run number"),tklabel(tt,text=""),
                    tklabel(tt,text="Parents"),tklabel(tt,text=""),
                    tklabel(tt,text="# of parameters"),tklabel(tt,text=""),tklabel(tt,text="Description (English only)"),OK.but2,Add.but)

      tkgrid(tt)
      ControlFile.Name<<-list()
      ControlFileName<<-list()
      DataFile.Name<<-list()
      DataFileName<<-list()
      RunNum<<-list()
      Run.Num<<-list()
      DescriptionN<<-list()
      Description.N<<-list()
      ParamNum<<-list()
      Param.Num<<-list()      
      ParentNum<<-list()
      Parent.Num<<-list()
            
      k<<-1
      ControlFile.Name[[k]]<<-tclVar("")
      ControlFileName[[k]]<<-tkentry(tt,width="20",textvariable=ControlFile.Name[[k]])
      DataFile.Name[[k]]<<-tclVar("")
      DataFileName[[k]]<<-tkentry(tt,width="20",textvariable=DataFile.Name[[k]]) 
      Run.Num[[k]]<<-tclVar("")
      RunNum[[k]]<<-tkentry(tt,width="15",textvariable=Run.Num[[k]])
      Description.N[[k]]<<-tclVar("")
      DescriptionN[[k]]<<-tkentry(tt,width="60",textvariable=Description.N[[k]])
      Param.Num[[k]]<<-tclVar("")
      ParamNum[[k]]<<-tkentry(tt,width="15",textvariable=Param.Num[[k]])
      Parent.Num[[k]]<<-tclVar("")
      ParentNum[[k]]<<-tkentry(tt,width="15",textvariable=Parent.Num[[k]])

      tkgrid(ControlFileName[[k]],RunNum[[k]],tklabel(tt,text=" "),ParentNum[[k]],
                   tklabel(tt,text=" "),ParamNum[[k]], tklabel(tt,text=" "),
                      DescriptionN[[k]],DataFileName[[k]],tklabel(tt,text=" "))
      tkgrid(tt)	
   }
   
  
   ###################################
   # open nonmem console
   ###################################

   OpenNMConsole<-function(NonmemRunID,filename)
   {  for(k in 1:10000)
      { n<-1+1
      }
      NonmemConsole<-gwindow(as.character(NonmemRunID))
      a<-gtext("",cont=NonmemConsole,font.attr=c(family="korea1deb"))
      diff.t<-TRUE
      old<-0
      console.text<-readLines(filename)
      svalue(a)<-console.text
      while(diff.t)
      {  new<-length(console.text)
         if(new!=old & new!=0) add(a,console.text[(old+1):new])
         old<-new
         diff.t<-ifelse(new>2, console.text[new-1]!="OUTPUT",TRUE)
         console.text<-readLines(filename)
      } 
   } 

    
   ###################################
   # open Result file
   ###################################   

   OpenResult<-function(NonmemRunID,filename)
   {  NonmemRes<-gwindow(paste(as.character(NonmemRunID),".res",sep=""))
      a<-gtext("",cont=NonmemRes,font.attr=c(family="korea1deb"),width=700)
      old<-0
      console.text<-readLines(filename)
      svalue(a)<-console.text
   }  
   
   ###################################
   # add $table in control file
   ###################################

   RemakeCTL<-function(file.name)
   {  
      D.CTL<-readLines(file.name)
      temp<-strsplit(file.name,split="\\\\")[[1]]
      file.id<-strsplit(temp[length(temp)],split="\\.")[[1]][1]
      D.temp<-matrix(D.CTL)
      NMversion<-read.table("NM.version")
      indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
      if(sum(indicator==";runnumber.ctl",na.rm=T)==0)
      {  table.id<-which(tolower(indicator)=="$table")[1]
         add.CTL1<-";runnumber.ctl"
         if(NMversion=="NM7")
         {  add.CTL2<-paste("$TABLE ID TIME DV IPRED IRES IWRES NPRED NRES NWRES PREDI RESI WRESI", 
                   " CPRED CRES CWRES CPREDI CRESI CWRESI EPRED ERES EWRES NPDE FILE=",file.id,".NOH NOPRINT ONEHEADER",sep="")
         } else
         {  add.CTL2<-paste("$TABLE ID TIME DV IPRED IRES IWRES PRED RES WRES FILE=",file.id,".NOH NOPRINT ONEHEADER",sep="")
         }                    
         D.new<-c(D.CTL[1:(table.id-1)],add.CTL1,add.CTL2,D.CTL[table.id:length(D.CTL)])
      } else
      {  D.new<-D.CTL
      }  
      write.table(D.new,file.name,quote=FALSE,row.names=FALSE,col.names=FALSE)
   }

   ###################################
   # Select output data
   ###################################   
   outputselect<-function(h,...)
   {  selRUNnum<-function(h,...)
      {  selectIDs<-function(h,...)
         {  D.data<-Orig.Data
            DD1<-svalue(VarList.cond1)
            DD1.F<-svalue(From.id.cond1)
            DD1.T<-svalue(To.id.cond1)
            DD2<-svalue(VarList.cond2)
            DD2.F<-svalue(From.id.cond2)
            DD2.T<-svalue(To.id.cond2)
            temp.i<-which(toupper(colnames(Orig.Data))=="MDV")
            if(DD1!="NONE     ")
            {  if(DD2!="NONE     ")
               {  if(DD1.F==" " | DD2.F==" " | DD1.T ==" " |DD2.T==" ")
                  {  gmessage("Enter select condition",icon="error")
                  } else
                  {  
                     if(length(temp.i)!=0)
                     {  sel.id<-which(D.data[,DD1]>=as.numeric(svalue(DD1.F)) & D.data[,DD1]<=as.numeric(svalue(DD1.T)) &
                              D.data[,DD2]>=as.numeric(svalue(DD2.F)) & D.data[,DD2]<=as.numeric(svalue(DD2.T))&D.data[,temp.i]==0 )
                     } else
                     {  sel.id<-which(D.data[,DD1]>=as.numeric(svalue(DD1.F)) & D.data[,DD1]<=as.numeric(svalue(DD1.T)) &
                              D.data[,DD2]>=as.numeric(svalue(DD2.F)) & D.data[,DD2]<=as.numeric(svalue(DD2.T)) )
                     }                              
                  }
               } else
               {  if(length(temp.i)!=0)
                  { sel.id<-which(D.data[,DD1]>=as.numeric(svalue(DD1.F)) & D.data[,DD1]<=as.numeric(svalue(DD1.T))&D.data[,temp.i]==0)
                  } else
                  { sel.id<-which(D.data[,DD1]>=as.numeric(svalue(DD1.F)) & D.data[,DD1]<=as.numeric(svalue(DD1.T)))
                  }                     
               } 
            } else
            { if(length(temp.i)!=0)
              {  sel.id<-(1:nrow(D.data))[D.data[,temp.i]==0]
              } else
              {  sel.id<-(1:nrow(D.data))
              }               
            }
            SEL.ID<<-sel.id
            TEMP<-read.table(paste(file.id,".noh",sep=""),skip=1,header=T)   
            if(length(sel.id)!=nrow(D.data))
            {  OUTPUT.file<<- paste(gfile(text="Save selected data as csv",
                  type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep="")        
               write.csv(TEMP[SEL.ID,],OUTPUT.file,row.names=F,quote=F)     
            } else
            {  OUTPUT.file<<-paste(svalue(id.sel),".noh",sep="")
            }  
            dispose(win)
         }
         file.id<-svalue(id.sel)
         FILE.ID<<-file.id
         runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==file.id),"path"]
         setwd(runnum.path)
         Orig.Data<-read.csv(paste(file.id,".csv",sep=""))
         Var.Name<-colnames(Orig.Data)
         temp.id<-which(toupper(Var.Name)=="MDV")
         if(length(temp.id)!=0)
           Var.Name<-Var.Name[-temp.id]
         From.label.1<-glabel("From")
         To.label.1<-glabel("To")
         VarList.cond1<-gdroplist(c("NONE     ",Var.Name))
         From.id.cond1<-gedit(" ",width=10)
         To.id.cond1<-gedit(" ",width=10)
         From.label.2<-glabel("From")
         To.label.2<-glabel("To")
         VarList.cond2<-gdroplist(c("NONE     ",Var.Name))
         From.id.cond2<-gedit(" ",width=10)
         To.id.cond2<-gedit(" ",width=10)
         VarList.X<-gdroplist(Var.Name)
         VarType.g1<-gradio(c("Categorical","Continuous"))
         VarList.Y<-gdroplist(Var.Name)
         VarType.g2<-gradio(c("Categorical","Continuous"))
         Button1<-gbutton("OK",handler=selectIDs)

         tmp<-gframe(" Select level 1 ",container=group)
         add(tmp,VarList.cond1)
         add(tmp,From.label.1);add(tmp,From.id.cond1)
         add(tmp,To.label.1);add(tmp,To.id.cond1)
         tmp<-gframe(" Select level 2",container=group)
         add(tmp,VarList.cond2)
         add(tmp,From.label.2);add(tmp,From.id.cond2)
         add(tmp,To.label.2);add(tmp,To.id.cond2)
         tmp<-gframe("  ",container=group)
         add(tmp,Button1)
      }
      Button<-gbutton("OK",handler=selRUNnum)
      runid.list<-unique(TOT.RUN$data[,"ID"])
      id.sel<-gdroplist(runid.list)

      win<-gwindow("Select output data")
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
      tmp<-gframe("Run number",container=group)
      add(tmp,id.sel)
      add(tmp,Button)
   }
   
   ###################################
   # Explore OUTPUT
   ################################### 

   PRED.list.7<<-c("PRED","IPRED", "NPRED" ,"PREDI","CPRED","CPREDI","EPRED")
   RES.list.7<<-c("RES","IRES","WRES","IWRES","NWRES","NRES","RESI","WRESI","CRES",
               "CWRES","CRESI","CWRESI","ERES","EWRES","NPDE")
   PRED.list.6<<-c("PRED","IPRE")
   RES.list.6<<-c("RES","WRES","IWRE","IRES")
   ###################################
   # postXY plot
   ###################################

   postXY.plot<-function(h,...)
   {  runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
      datafile.name1<-paste(FILE.ID,".csv",sep="")
      D.data1<-read.csv(datafile.name1,na.string=".")
      colnames(D.data1)<-paste(colnames(D.data1),"-EDA  ",sep="")
      datafile.name2<-paste(FILE.ID,".noh",sep="")
      D.data2<-read.table(datafile.name2,skip=1,header=T)
      D.data<-cbind(D.data2,D.data1)[SEL.ID,]
      
      Var.Name.post<-colnames(D.data)
      updatePlot<-function(h,...)
      {  condX.V <-svalue(VarList.X,index=T)
         condY.V<-svalue(VarList.Y,index=T)
         print(condX.V)
         print(condY.V)
         select.data<-D.data
         if(!is.na(condX.V)& !is.na(condY.V))
         {  X<-select.data[,condX.V]
            Y<-select.data[,condY.V]
            dev.set(which=Dev.XYplot)
            plot(X,Y,xlab=Var.Name.post[condX.V],ylab=Var.Name.post[condY.V])
            lines(lowess(X,Y),col=2,lwd=2)                
         }
      }
      saveData<-function(h,...)
      {  condX.V <-svalue(VarList.X,index=T)
         condY.V<-svalue(VarList.Y,index=T)
         select.data<-D.data[,c(ID.id,condX.V,condY.V)]
         if(is.null(ID.id))
         {  colnames(select.data)<-Var.Name.post[c(condX.V,condY.V)]
         } else
         {  colnames(select.data)<-Var.Name.post[ c(ID,id,condX.V,condY.V)]                                                                                                                                            
         }
         write.csv(select.data,paste(gfile(text="Save as csv",
             type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)          
      }
      VarList.X<-gdroplist(Var.Name.post)
      VarList.Y<-gdroplist(Var.Name.post)
      Button1<-gbutton("OK",handler=updatePlot)
      Button2<-gbutton("Save",handler=saveData)
    
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
      Dev.XYplot<-dev.cur()
   }
   ###################################
   # DV vs PRED
   ################################### 

   DVvsPRED.plot<-function(h,...)
   {   runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
       outfile.name<-paste(FILE.ID,".noh",sep="")
       Output.Table<-read.table(outfile.name,skip=1,header=T)
       select.data<-Output.Table[SEL.ID,]
       ID<-as.character(sort(unique(select.data$ID)))
       ID<-matrix(ID,nrow=length(ID))
       colnames(ID)<-c("ID")

      updatePlot<-function(h,...)
      {       condX.V <-"DV"
         condY.V<-svalue(PRED.sel)
         X<-select.data[,3]
         del.id<-which(X==0)
         if(length(del.id)!=0)
             X<-X[-del.id]
         if(is.null(X))
           X<-rep(NA,nrow(select.data)-length(del.id))
         if(length(del.id)!=0)
         {  Y<-select.data[-del.id,condY.V]
         } else
         {  Y<-select.data[,condY.V]
         }         
           
         if(is.null(Y))
           Y<-rep(NA,nrow(select.data)-length(del.id))
         plot(X,Y,xlab=condX.V,ylab=condY.V)
         abline(a=0,b=1,col=2)        
      }
      
      updatePlot1<-function(h,...)
      {  condX.V <-"DV"
         condY.V<-svalue(PRED.sel)
         X<-select.data[,3]
         del.id<-which(X==0)
         if(length(del.id)!=0)
            X<-X[-del.id]
         if(is.null(X))
           X<-rep(NA,nrow(select.data)-length(del.id))
         if(length(del.id)!=0)
         {  Y<-select.data[-del.id,condY.V]
         } else
         {  Y<-select.data[,condY.V]
         }         
         if(is.null(Y))
           Y<-rep(NA,nrow(select.data)-length(del.id))
         plot(X,Y,xlab=condX.V,ylab=condY.V)
         abline(a=0,b=1,col=2)    
         sel.id<-which(select.data$ID==svalue(IDlist))
         X<-select.data[sel.id,3]
         del.id<-which(X==0)
         if(length(del.id)!=0)
            X<-X[-del.id]
         if(is.null(X))
           X<-rep(NA,nrow(select.data)-length(del.id))
         Y<-select.data[sel.id,condY.V]
         if(length(del.id)!=0)
            Y<-Y[-del.id]
         points(X,Y,col=2,pch=16)        
      }
      
      Button<-gbutton("OK",handler=updatePlot)
      win<-gwindow(paste("DV vs Predictions plot (",OUTPUT.file,")",sep=""))
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
      tmp=gframe(" Y variable",container=group)
      if(read.table("NM.version")=="NM6")
      {  PRED.list<-PRED.list.6
      } else
      {  PRED.list<-PRED.list.7
      }
      PRED.sel<-gdroplist(PRED.list)
      add(tmp,PRED.sel)

      tmp=gframe("Plot",container=group)
      add(tmp,Button,expand=TRUE)
      tmp<-gframe("ID",container=group)
      IDlist <- gtable(ID,multiple=T,handler=updatePlot1) 
      size(IDlist)<-c(100,200)
      add(tmp,IDlist)

      add(BigGroup,ggraphics())
   }

   ###################################
   # DV vs RES
   ################################### 

   DVvsRES.plot<-function(h,...)
   {  runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
      outfile.name<-paste(runnum.path,"\\",FILE.ID,".noh",sep="")
      Output.Table<-read.table(outfile.name,skip=1,header=T)
      select.data<-Output.Table[SEL.ID,]
      ID<-as.character(sort(unique(select.data$ID)))
      ID<-matrix(ID,nrow=length(ID))
      colnames(ID)<-c("ID")
    
      updatePlot<-function(h,...)
      {  condX.V <-"DV"
         condY.V<-svalue(RES.sel)
         X<-select.data[,3]
         del.id<-which(X==0)
         if(length(del.id)!=0)
            X<-X[-del.id]
         if(is.null(X))
            X<-rep(NA,nrow(select.data)-length(del.id))
         if(length(del.id)!=0)
         {  Y<-select.data[-del.id,condY.V]
         } else
         {  Y<-select.data[,condY.V]
         }         
         if(is.null(Y))
            Y<-rep(NA,nrow(select.data)-length(del.id))
         
         par(mfrow=c(1,1))
         plot(X,Y,xlab=condX.V,ylab=condY.V)
         abline(h=0,col=2)
         WRES.list<-c("WRES","IWRES","NWRES","WRESI","CWRES","CRESI","CWRESI","EWRES","NPDE")
         if(sum(WRES.list==condY.V)!=0)
         {  abline(h=2,col=2,lty=2)
            abline(h=-2,col=2,lty=2)
         }        
      }
      
      updatePlot1<-function(h,...)
      {  condX.V <-"DV"
         condY.V<-svalue(RES.sel)
         X<-select.data[,3]
         del.id<-which(X==0)
         if(length(del.id)!=0)
            X<-X[-del.id]
         if(is.null(X))
            X<-rep(NA,nrow(select.data)-length(del.id))
         if(length(del.id)!=0)
         {  Y<-select.data[-del.id,condY.V]
         } else
         {  Y<-select.data[,condY.V]
         }
         if(is.null(Y))
            Y<-rep(NA,nrow(select.data)-length(del.id))
         
         par(mfrow=c(1,1))
         plot(X,Y,xlab=condX.V,ylab=condY.V)
         abline(h=0,col=2)
         WRES.list<-c("WRES","IWRES","NWRES","WRESI","CWRES","CRESI","CWRESI","EWRES","NPDE")
         if(sum(WRES.list==condY.V)!=0)
         {  abline(h=2,col=2,lty=2)
            abline(h=-2,col=2,lty=2)
         }   
          sel.id<-which(select.data$ID==svalue(IDlist))
         X<-select.data[sel.id,3]
         del.id<-which(X==0)
         if(length(del.id)!=0)
             X<-X[-del.id]
         if(is.null(X))
           X<-rep(NA,nrow(select.data)-length(del.id))
         Y<-select.data[sel.id,condY.V]
         if(length(del.id)!=0)
            Y<-Y[-del.id]
         points(X,Y,col=2,pch=16)        
              
      }

      Button<-gbutton("OK",handler=updatePlot)
      win<-gwindow(paste("DV vs Residuals plot (",OUTPUT.file,")",sep=""))
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)

      tmp=gframe(" Y variable",container=group)
      if(read.table("NM.version")=="NM6")
      {  RES.list<-RES.list.6
      } else
      {  RES.list<-RES.list.7
      }

      RES.sel<-gdroplist(RES.list)
      add(tmp,RES.sel)
      tmp=gframe("Plot",container=group)
      add(tmp,Button,expand=TRUE)
      tmp<-gframe("ID",container=group)
      IDlist <- gtable(ID,multiple=T,handler=updatePlot1) 
      size(IDlist)<-c(100,200)
      add(tmp,IDlist)
      add(BigGroup,ggraphics())
   }

   ###################################
   # TIME vs RES
   ################################### 

   TIMEvsRES.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
         outfile.name<-paste(runnum.path,"\\",FILE.ID,".NOH",sep="")
         Output.Table<-read.table(outfile.name,skip=1,header=T)
         condX.V <-"TIME"
         condY.V<-svalue(RES.sel)
   
         select.data<-Output.Table[SEL.ID,]
         del.id<-which(select.data[,3]==0)
         if(length(del.id)!=0)
         {   X<-select.data[-del.id,condX.V]
         } else
         {   X<-select.data[,condX.V]
         }          
         if(is.null(X))
           X<-rep(NA,nrow(select.data)-length(del.id))
         if(length(del.id)!=0)
         {  Y<-select.data[-del.id,condY.V]
         } else
         {  Y<-select.data[,condY.V]
         }          
         if(is.null(Y))
           Y<-rep(NA,nrow(select.data)-length(del.id))

         plot(X,Y,xlab=condX.V,ylab=condY.V,type='n')
         id<-as.numeric(names(table(Output.Table$ID)))
         for(i in 1:length(id))
         {  if(length(del.id)!=0)
            {  X.data<-X[which(select.data[-del.id,"ID"]==id[i])]
               Y.data<-Y[which(select.data[-del.id,"ID"]==id[i])]
            } else
            {  X.data<-X[which(select.data[,"ID"]==id[i])]
               Y.data<-Y[which(select.data[,"ID"]==id[i])]
            }               
            lines(X.data,Y.data,lty=2)
         }
         abline(h=0,col=2)
         
         WRES.list<-c("WRES","IWRES","NWRES","WRESI","CWRES","CRESI","CWRESI","EWRES","NPDE")

         if(sum(WRES.list==condY.V)!=0)
         {  abline(h=2,col=2,lty=2)
            abline(h=-2,col=2,lty=2)
         }
       }

      Button<-gbutton("OK",handler=updatePlot)

      win<-gwindow(paste("TIME vs Residuals plot (",OUTPUT.file,")",sep=""))
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)

      tmp=gframe(" Y variable",container=group)
      if(read.table("NM.version")=="NM6")
      {  RES.list<-RES.list.6
      } else
      {  RES.list<-RES.list.7
      }
      RES.sel<-gdroplist(RES.list)
      add(tmp,RES.sel)

      tmp=gframe("Plot",container=group)
      add(tmp,Button,expand=TRUE)
      add(BigGroup,ggraphics())
   }

   ###################################
   # TIME vs DV and PRED
   ################################### 

   TIMEvsDVandPRED.plot<-function(h,...)
   { 
      updatePlot<-function(h,...) 
      {  
         runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
         outfile.name<-paste(runnum.path,"\\",FILE.ID,".noh",sep="")
         Output.Table<-read.table(outfile.name,skip=1,header=T)
         D.data<-read.csv(paste(FILE.ID,".csv",sep=""))
         condX.V <-"TIME"
         select.data<-Output.Table[SEL.ID,]
         condY.V<-svalue(PRED.sel)
         del.id<-which(select.data[,3]==0)
         if(length(del.id)!=0)
         {  X<-select.data[-del.id,condX.V]
            Y<-select.data[-del.id,3]
            ID<-select.data[-del.id,1]
            Y1<-select.data[-del.id,condY.V]  
         } else
         {  X<-select.data[,condX.V]
            Y<-select.data[,3]
            ID<-select.data[,1]
            Y1<-select.data[,condY.V]  
         }   
         plot(X,Y,type='n',xlab=condX.V,ylab=condY.V,
            ylim=range(c(Y,Y1)))#,xlim=range(Output.Table[,condX.V]),ylim=range(Output.Table[,condY.V]))
         id.list<-unique(Output.Table$ID)
         
         for(i in id.list)
         {  sel.data.id<-which(ID==i)
            lines(X[sel.data.id],Y[sel.data.id],lwd=0.1,lty=2,col="grey")
         }
         
         for(i in id.list)
         {  sel.data.id<-which(ID==i)
            lines(X[sel.data.id],Y1[sel.data.id],lwd=0.5,lty=1,col="grey50")
         }
      }
      Button<-gbutton("OK",handler=updatePlot)

      win<-gwindow(paste("Predictions and DV vs TIME plot (",OUTPUT.file,")",sep=""))
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
 
      tmp=gframe(" Y variable",container=group)
      if(read.table("NM.version")=="NM6")
      {  PRED.list<-PRED.list.6
      } else
      {  PRED.list<-PRED.list.7
      }

      PRED.sel<-gdroplist(PRED.list)
      add(tmp,PRED.sel)

      tmp=gframe("Plot",container=group)
      add(tmp,Button,expand=TRUE)
      add(BigGroup,ggraphics())
   }
    
   ###################################
   # EBE vs COV plot
   ###################################

   EBEvsCOV.plot<-function(h,...)
   {  updatePlot<-function(h,...)
      {  runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
         outfile.name<-paste(runnum.path,"\\",FILE.ID,".noh",sep="")
         Output.Table<-read.table(outfile.name,skip=1,header=T)
         condX.V <-svalue(EBE.sel)
         condY.V<-svalue(COV.sel)
         X<-TOT.data[,condX.V]
         Y<-TOT.data[,condY.V]
         plot(Y,X,xlab=condY.V,ylab=condX.V)
         lines(lowess(Y,X),col=2,lwd=2)        
      }
      savePlot<-function(h,...)
      {  condX.V <-svalue(EBE.sel)
         condY.V<-svalue(COV.sel)
         write.csv(T.data[,c("X.ID",condX.V,condY.V)],paste(gfile(text="Save selected data as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
      }     
      
      runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
      runnum.path<-runnum.path[length(runnum.path)]
      outfile.name<-paste(runnum.path,"\\",FILE.ID,".noh",sep="")
      datafile.name<-paste(runnum.path,"\\",FILE.ID,".csv",sep="")
      EBEfile.name<-paste(runnum.path,"\\",FILE.ID,".eta",sep="")
      PARfile.name<-paste(runnum.path,"\\",FILE.ID,".par",sep="")
    
      Orig.data<-read.csv(datafile.name,na.string=".")
      EBE.data<-read.table(EBEfile.name,skip=1,header=T)
      PAR.data<-read.table(PARfile.name,skip=1,header=T)   
      if(nrow(EBE.data)!=nrow(PAR.data))
      {  tot.data<-merge(EBE.data,PAR.data,all=T)
      } else
      {  tot.data<-cbind(EBE.data,PAR.data[,-1])
      }   
      EBE.data<-merge(EBE.data,PAR.data,all=T)
      colnames(EBE.data)[1]<-"X.ID"
      Var.Name<-colnames(Orig.data)
      EBE.name<-colnames(EBE.data);#EBE.name<-EBE.name[EBE.name!="X.ID"]
      if(nrow(EBE.data)!=nrow(Orig.data))
      {  tot.data<-merge(Orig.data,EBE.data,all=T)
      } else
      {  tot.data<-cbind(Orig.data,EBE.data[,-1])
      }      
      
      TOT.data<-tot.data[SEL.ID,]
      T.data<<-TOT.data
      Button2<-gbutton("OK",handler=updatePlot)
      Button3<-gbutton("OK",handler=savePlot)

      win<-gwindow("Parameter vs covariate")
      BigGroup<-ggroup(cont=win)
      group=ggroup(horizontal=FALSE,cont=BigGroup)
      tmp=gframe(" Parameter",container=group)
      EBE.sel<-gdroplist(EBE.name)
      add(tmp,EBE.sel)
 
      tmp=gframe(" Covariate",container=group)
      COV.sel<-gdroplist(Var.Name)
      add(tmp,COV.sel)

      tmp=gframe("Plot",container=group)
      add(tmp,Button2,expand=TRUE)
 
      tmp=gframe("Save",container=group)
      add(tmp,Button3,expand=TRUE)     
      add(BigGroup,ggraphics())
   }
    
   ###################################
   # Add table
   ###################################    
   AddRunTable<-function(h,...)
   {  AddTable<-function(h,...)
      {  for(ii in 1:kk)
         {  dir.name<-tclvalue(ControlFile.Dir[[ii]])
            file.id<-tclvalue(Run.Num[[ii]])
            param.num<-as.numeric(tclvalue(Param.Num[[ii]]))
            Description<-tclvalue(Description.N[[ii]])
            TOT.temp<-TOT.RUN
            TOT.temp$num<-TOT.temp$num+1
            TOT.temp$data<-rbind(TOT.temp$data,c(file.id,dir.name,toupper(tclvalue(Parent.Num[[ii]]))))
            colnames(TOT.temp$data)<-c("ID","path","parents")
            TOT.RUN<<-TOT.temp

            D.LST<-readLines(paste(dir.name,"\\",file.id,".res",sep=""))
            ETA<-read.table(paste(dir.name,"\\",file.id,".ETA",sep=""),skip=1,header=T)
            temp.ETA<-colnames(ETA)
            ET.id<-NULL
            for(i in 1:length(temp.ETA))
            {  if(length(strsplit(temp.ETA[i],"ET")[[1]])>1)
                  ET.id<-c(ET.id,i)
            }
            ETA<-ETA[,ET.id]              
            data.file<-paste(dir.name,"\\",file.id,".csv",sep="")
        data.tempt<-read.csv(data.file,na.string=".")   
         if(sum(toupper(colnames(data.tempt))=="MDV")!=0)
         {  data.i<-which(toupper(colnames(data.tempt))=="MDV")
            temp<-data.tempt[,data.i]
            temp<-temp[temp==0]
            data.n<-length(temp)         
         } else
         {  data.n<-nrow(data.tempt)
         }

            RunID<-TOT.RUN$data[TOT.RUN$num,1]
            n.lst<-length(D.LST)
            Date<-D.LST[n.lst-1]
            Time<-D.LST[n.lst]

            D.temp<-matrix(D.LST)
            indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
            min.id<- which(indicator=="0MINIMIZATION")
            min.id<-min.id[length(min.id)]
            Min<-strsplit(D.LST[min.id],split=" ")[[1]][2]

            indicator<-apply(D.temp,1,function(x) strsplit(x,split=":")[[1]][1])
            cond.id<-grep(" EIGENVALUES ",D.LST)
            if(length(cond.id)!=0)
            {  flag<-T
               id.current<-cond.id+5
               cond.line<-0
               while(flag)
               {  ttemp<-D.LST[id.current]
                  flag<-ttemp!=" "
                  if(flag)
                  {  cond.line<-cond.line+1
                     id.current<-id.current+1
                  }
               }
               temp<-NULL
               for(i in 1:cond.line)
                  temp<-c(temp,strsplit(D.LST[cond.id+5+cond.line+i],split=" ")[[1]])
               temp<-as.numeric(temp[temp!=""&temp!="+"])
               cond.num<-round(max(temp)/min(temp),3)        
            } else
            {  cond.num<-NA
            }
            
            obj.id<-which(indicator==" #OBJV")
            if(length(obj.id)!=0)
            {  obj.id<-obj.id[length(obj.id)]
            } else
            {  obj.id<-9+which(indicator==" ********************                           MINIMUM VALUE OF OBJECTIVE FUNCTION                  ********************" )
            } 
            temp<-strsplit(D.LST[obj.id],split=" ")[[1]]
            temp<-temp[3:(length(temp)-3)]
            temp<-as.numeric(temp[temp!=""])
            Obj<-temp[!is.na(temp)]
            AIC<-Obj+2*param.num
            AICc<-round(Obj+2*param.num+2*param.num*(param.num+1)/(data.n-param.num-1),3)
            SBC<-round(Obj+param.num*log(data.n),3)
            parent<-TOT.RUN$data[TOT.RUN$num,"parents"]

            final.start.id<-grep("FINAL PARAMETER ESTIMATE",D.LST)
            final.start.id<-final.start.id[length(final.start.id)]
            Result.LST<-D.LST[final.start.id:length(D.LST)]

            theta.id<-grep("THETA",Result.LST)
            theta.line<-0
            theta.flag<-T
            while(theta.flag)
            {  if(Result.LST[theta.id[1]+3+theta.line]!=" ")
               {  theta.line<-theta.line+1
               } else
               {  theta.flag<-FALSE
               }  
            }
            temp<-NULL
            for(i in 1:theta.line)
              temp<-c(temp,unlist(strsplit(Result.LST[theta.id[1]+3+theta.line+i],split=" ")))
            temp<-as.numeric(temp[temp!=""])
            THETA<-temp
      
            seTHETA<-rep(NA,length(THETA))  
            if(length(theta.id)!=1)
            {  temp<-NULL
               for(i in 1:theta.line)
                  temp<-c(temp,unlist(strsplit(Result.LST[theta.id[2]+3+theta.line+i],split=" ")))
               temp<-as.numeric(temp[temp!=""])
               seTHETA<-temp
            }     

            if(sum(!is.na(seTHETA))==0)
            {  COV<-"NONE"
               cond.num<-NA
            } else
            {  COV<-"OK"
            }

            temp<-c(RunID,Date,Time,Min,COV,Obj,AIC,AICc,SBC,cond.num,parent,Description,param.num)

            if(TOT.RUN$num>2)
            {  run.table[]<-rbind(run.table[],temp)
            } else
            {  run.table[][TOT.RUN$num,]<-temp
            }
         } 
         tkdestroy(Toptt)
      }

      Add<-function()
      {  AddLine(kk+1)
      }

      AddLine<-function(id)
      {  kk<<-id
         ControlFile.Dir[[id]]<<-tclVar("")
         ControlFileDir[[id]]<<-tkentry(tt,width="20",textvariable=ControlFile.Dir[[id]])
         Run.Num[[id]]<<-tclVar("")
         RunNum[[id]]<<-tkentry(tt,width="15",textvariable=Run.Num[[id]])
         Description.N[[id]]<<-tclVar("")
         DescriptionN[[id]]<<-tkentry(tt,width="60",textvariable=Description.N[[id]])
         Param.Num[[id]]<<-tclVar("")
         ParamNum[[id]]<<-tkentry(tt,width="15",textvariable=Param.Num[[id]])
         Parent.Num[[id]]<<-tclVar("")
         ParentNum[[id]]<<-tkentry(tt,width="15",textvariable=Parent.Num[[id]])

         tkgrid(ControlFileDir[[id]],RunNum[[id]],tklabel(tt,text=" "),ParentNum[[id]],
                   tklabel(tt,text=" "),ParamNum[[id]], tklabel(tt,text=" "),
                      DescriptionN[[id]],tklabel(tt,text=" "))
         tkgrid(tt)
      }

      ConFile<-function()
      {  kkk<-kk
         tclvalue(ControlFile.Dir[[kkk]])<<-gfile(text="Choose run number subfolder",type="selectdir")
         file.ctl<-tclvalue(ControlFile.Dir[[kkk]])
         temp<-strsplit(file.ctl,"\\\\")[[1]]
         RunNumber<-temp[length(temp)]
         tclvalue(Run.Num[[kkk]])<<-RunNumber
         setwd(strsplit(file.ctl,split=RunNumber)[[1]])
      }

      Toptt<<-tktoplevel()
      tkwm.title(Toptt,"Make run table from runnumber subfolder")
      tt<-tkframe(Toptt)
      ttg<-tkframe(tt)
      OK.but3 <-tkbutton(ttg,text="OK",command=AddTable)

      tkgrid(tklabel(ttg,text=""),tklabel(ttg,text=""),tklabel(ttg,text=""),
                     OK.but3,tklabel(ttg,text=""))
      tkgrid(ttg)
      OK.but1 <-tkbutton(tt,text="Run number subfolder",command=ConFile)
      Add.but<-tkbutton(tt,text="Add",command=Add)
      
      tkgrid(OK.but1,tklabel(tt,text="Run number"),tklabel(tt,text=""),
                    tklabel(tt,text="Parents"),tklabel(tt,text=""),
                    tklabel(tt,text="# of parameters"),tklabel(tt,text=""),tklabel(tt,text="Description (English only)"),Add.but)

      tkgrid(tt)
      ControlFile.Dir<<-list()
      ControlFileDir<<-list()
      RunNum<<-list()
      Run.Num<<-list()
      DescriptionN<<-list()
      Description.N<<-list()
      ParamNum<<-list()
      Param.Num<<-list()      
      ParentNum<<-list()
      Parent.Num<<-list()
            
      kk<<-1
      ControlFile.Dir[[kk]]<<-tclVar("")
      ControlFileDir[[kk]]<<-tkentry(tt,width="20",textvariable=ControlFile.Dir[[kk]])
      Run.Num[[kk]]<<-tclVar("")
      RunNum[[kk]]<<-tkentry(tt,width="15",textvariable=Run.Num[[kk]])
      Description.N[[kk]]<<-tclVar("")
      DescriptionN[[kk]]<<-tkentry(tt,width="60",textvariable=Description.N[[kk]])
      Param.Num[[kk]]<<-tclVar("")
      ParamNum[[kk]]<<-tkentry(tt,width="15",textvariable=Param.Num[[kk]])
      Parent.Num[[kk]]<<-tclVar("")
      ParentNum[[kk]]<<-tkentry(tt,width="15",textvariable=Parent.Num[[kk]])

      tkgrid(ControlFileDir[[kk]],RunNum[[kk]],tklabel(tt,text=" "),ParentNum[[kk]],
                   tklabel(tt,text=" "),ParamNum[[kk]], tklabel(tt,text=" "),
                      DescriptionN[[kk]],tklabel(tt,text=" "))
      tkgrid(tt)
   }

   
   ###################################
   # save RUNtable
   ###################################   

   saveRUNTABLE.handler<-function(h,...)
   {  runtable<-run.table[]
      runtable<-runtable[runtable$Run!="",]
      temp.table<-cbind(runtable,TOT.RUN$data[,2])
      colnames(temp.table)<-c(colnames(runtable),"Path")
      write.csv(temp.table,paste(gfile(text="Save run table as csv",
           type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F,quote=F)
   }
   
   loadRUNTABLE.handler<-function(h,...)
   {  run.filename<-gfile("Open run table file",type="open")
      run.f<-read.csv(run.filename)

      for(ii in 1:nrow(run.f))
      {  temp<-as.character(run.f[ii,1:13])
         for(j in c(2,3,4,5,11,12))
            temp[j]<-as.character(run.f[ii,j])
         if(TOT.RUN$num>2)
         {  run.table[]<-rbind(run.table[],temp)
         } else
         {  run.table[][TOT.RUN$num+ii,]<-temp
         }
      }    
      TOT.temp<-TOT.RUN
      TOT.temp$data<-cbind(run.f$Run.number,as.character(run.f$Path),as.character(run.f$Parents))          
      colnames(TOT.temp$data)<-c("ID","path","parents")
      TOT.temp$data<-rbind(TOT.RUN$data,TOT.temp$data)
      TOT.temp$num<-TOT.RUN$num+nrow(run.f)   
      TOT.RUN<<-TOT.temp
      
    }
   
   
   ###################################
   # Model Tree
   ###################################   

   Tree.handler<-function(h,...)
   {  Tree.make<-function(Tree.data,Root.name)
      {  id.tree<-Tree.data[,1]
         parent.tree<-Tree.data[,2]
         Tree.struct<-list()
         Tree.struct$ROOT<-NULL
         Tree.struct$Child<-list()
         for(i in 1:nrow(Tree.data))
         {  if(parent.tree[i]==Root.name)
            {  Tree.struct$ROOT<-c(Tree.struct$ROOT,id.tree[i])
            }
         }
         if(!is.null(Tree.struct$ROOT))
         {  for(j in 1:length(Tree.struct$ROOT))
            {  Tree.struct$Child[[j]]<-id.tree[which(parent.tree==Tree.struct$ROOT[j])]
               names(Tree.struct$Child)[j]<-paste(Root.name,Tree.struct$ROOT[j],sep="-")
            }
            return(Tree.struct)
         } else
         {  return(NULL)
         }
      }
 
      expandTree<-function(Tree.data,Root.list,TREE.display)
      {  empty.col<-ncol(TREE.display)
         Tree.Flag<-FALSE
         for(i in 1:length(Root.list))
         {  temp<-Tree.make(Tree.data,Root.list[i])

            if(length(temp$ROOT)!=0)
            {  empty.matrix<-matrix(0,nrow=nrow(TREE.display)+length(temp$ROOT)-1,ncol=empty.col+1)
               ind.i<-which(TREE.display[,ncol(empty.matrix)-1]==Root.list[i])
               empty.matrix[1:ind.i,1:ncol(TREE.display)]<-TREE.display[1:ind.i,]
               empty.matrix[ind.i:(ind.i+length(temp$ROOT)-1),empty.col+1]<-temp$ROOT
               if(i != length(Root.list))
                  empty.matrix[(ind.i+length(temp$ROOT)):nrow(empty.matrix),1:ncol(TREE.display)]<-TREE.display[(ind.i+1):nrow(TREE.display),]
               TREE.display<-empty.matrix
               Tree.Flag<-TRUE
            }
          
         }
         TREE<-list(Display=TREE.display,Flag=Tree.Flag)
         return(TREE)  
      }   
 

      Tree.data <- as.matrix(cbind(as.character(TOT.RUN$data[,1]),as.character(TOT.RUN$data[,3])))
      colnames(Tree.data)<-c("ID","parents")      
      Tree.data<-matrix(c(Tree.data[!duplicated(Tree.data[,"ID"],fromLast=T),]),ncol=2)
       
      temp<-Tree.make(Tree.data,"ROOT")
      Root.list<-temp$ROOT    
      TREE.display<-matrix(Root.list,ncol=1)      
      flag<-T
      while(flag)
      {  TREE<-expandTree(Tree.data,Root.list,TREE.display)
         TREE.display<-TREE$Display
         Root.list<-TREE.display[,ncol(TREE.display)]
         flag<-TREE$Flag
      }   
      A<-TREE.display
      colnames(A)<-c("ROOT",rep(" ",ncol(A)-1))
      MT<-gwindow("Model tree")
      gtable(A,cont=MT)
   }

   ###################################
   # openXpose
   ###################################   
   
   OpenXpose<-function(h,...)
   {  setwd("c:/fit4NM")
      system("Rgui")
   } 


   ###################################
   # Randomization Test 
   ###################################  

   RandomTest<-function(h,...)
   {  openControl<-function(h,...)
      {  control.file<-gfile(text="Open control file",type="open")
         current.ctl<<-readLines(control.file)
         svalue(control.t)<-control.file
         temp<-strsplit(control.file,split="\\\\")[[1]]
         Random.RUN<-temp[length(temp)]
         setwd(strsplit(control.file,split=Random.RUN)[[1]])
         Random.RUN<<-strsplit(Random.RUN,split="\\.")[[1]][1]
         file.id<<-strsplit(Random.RUN,split="\\.")[[1]][1]
      }
 
      randomsave<-function(h,...)
      {  file.name<-paste(gfile(text="Save as csv",
                 type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep="")
         write.csv(RAN.result,file.name,row.names=F)
         svalue(edit5)<-file.name                 
      }

      randomopen<-function(h,...)
      {  file.name<-gfile(text="Open randomization result file", type="open",filter=list("csv files"=list(patterns=c("*.csv"))))
         temp<-read.csv(file.name)
         sort.id<-sort.list(as.numeric(as.character(temp$Delta[-1])),decreasing=T)
         temp<-temp[c(1,sort.id+1),]
         n.row<-nrow(temp)-1
         temp$Prob[-1]<-1:n.row
         temp$Quantile[-1]<-c(1:n.row)/n.row
         RAN.result<<-temp
         svalue(edit5)<-file.name
         Rplot()
      }
      
      opendata<-function(h,...)
      {  data.file<<-gfile(text="Open data file",type="open")
         D.data<<-read.csv(data.file,na.string=".")
         svalue(data.t)<-data.file
         Var.list<-colnames(D.data)
         tmp<-gframe("Covariates",cont=BBgroup)
         cov.t<<-gdroplist(Var.list)
         add(tmp,cov.t)
         tmp<-gframe("OBJ for reference model",cont=BBgroup)
         base.obj.t<<-gedit(" ",width=50)
         add(tmp,base.obj.t)
         tmp<-gframe("# of iterations / Seed number",cont=BBgroup)
         RT.label<-glabel("# of replicates")
         iteration.n<<-gedit("1000",width=10)
         add(tmp,RT.label)
         add(tmp,iteration.n)
         Seed.label<-glabel("Seed number")
         Seed.input.RT<<-gedit("0",width=10)
         add(tmp,Seed.label)
         add(tmp,Seed.input.RT) 
         tmp<-gframe("Randomization test",cont=BBgroup)
         button3<-gbutton("Start",handler=randomstart,width=20,height=10)
         add(tmp,button3)
         tmp<-gframe("Save randomization test result",cont=BBgroup)
         button4<-gbutton("Save",handler=randomsave,width=20,height=10)
         add(tmp,button4)
          tmp<-gframe("Open randomization test result",cont=BBgroup)
         button5<-gbutton("Open",handler=randomopen,width=20,height=10)
         edit5<<-gedit("",width=50)
         add(tmp,button5)
         add(tmp,edit5)         
         
      }

      randomstart<-function(h,...)
      {  cov.T<-svalue(cov.t)
         print(cov.T)
         temp.CTL<-current.ctl
         temp<-strsplit(temp.CTL,split=" ")
         indicator<-NULL
         for(i in 1:length(temp))
            indicator<-rbind(indicator,temp[[i]][1])
         id<-which(indicator=="$DATA")
         temp.CTL[id]<-"$DATA RT.csv"
         write.table(temp.CTL,"RT.ctl",quote=FALSE,row.names=FALSE,col.names=FALSE)
         id.table<-table(D.data$X.ID)
         COV<-NULL
         for(i in 1:length(id.table))
         {  start.id<-ifelse(i==1,1, (id.table[1:(i-1)]+1))
            COV<-c(COV,median(D.data[start.id:sum(id.table[1:i]),cov.T],na.rm=T))
         }
         print(COV)
         iteration.N<-as.numeric(svalue(iteration.n))            
         RT.win<-gwindow("Randomization test progress",width=300,height=50)
         RT.progress<-gslider(from=0,to=iteration.N,by=1,value=0,cont=RT.win)
         svalue(RT.progress)<-0

         random.table<-NULL     
         seed<-as.numeric(svalue(Seed.input.RT))
         set.seed(seed)
         for( k in 0:iteration.N)
         {  svalue(RT.progress)<-k
            D.data.t<-D.data
            if(k !=0)
            { 
               COV.random<-sample(COV,replace=F)
               print(COV.random)
               cov.random<-COV.random[D.data.t$X.ID]
               D.data.t[,cov.T]<-cov.random
               colnames(D.data.t)[1]<-"#ID"
               write.csv(D.data.t,"RT.csv",quote=FALSE,row.names=FALSE,na=".")
            } else
            {  colnames(D.data.t)[1]<-"#ID"
               write.csv(D.data.t,"RT.csv",quote=FALSE,row.names=FALSE,na=".")
            }
            random.command<-paste(Default.NMpath," RT.ctl RT.res")
            system(random.command,invisible=F,show.output.on.console=F)    

            D.LST<-readLines("RT.res")
            D.temp<-matrix(D.LST)
            indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
            min.id<- which(indicator=="0MINIMIZATION")
            min.id<-min.id[length(min.id)]
            Min<-strsplit(D.LST[min.id],split=" ")[[1]][2]

            indicator<-apply(D.temp,1,function(x) strsplit(x,split=":")[[1]][1])
            temp<-strsplit(D.LST[length(D.LST)-4],split=" ")[[1]]
            temp<-as.numeric(temp[temp!=""&temp!="+"])
            cond.num<-round(max(temp)/min(temp),3)
            obj.id<-which(indicator==" #OBJV")
            if(length(obj.id)!=0)
            {  obj.id<-obj.id[length(obj.id)]
            } else
            { obj.id<-9+which(indicator==" ********************                           MINIMUM VALUE OF OBJECTIVE FUNCTION                  ********************" )
            } 
            temp<-strsplit(D.LST[obj.id],split=" ")[[1]]
            temp<-temp[3:(length(temp)-3)]
            temp<-as.numeric(temp[temp!=""])
            Obj<-temp[!is.na(temp)]
            
            final.start.id<-grep("FINAL PARAMETER ESTIMATE",D.LST)
            final.start.id<-final.start.id[length(final.start.id)]
            Result.LST<-D.LST[final.start.id:length(D.LST)]

            theta.id<-grep("THETA",Result.LST)
            theta.line<-0
            theta.flag<-T
            while(theta.flag)
            { if(Result.LST[theta.id[1]+3+theta.line]!=" ")
               {  theta.line<-theta.line+1
               } else
               {  theta.flag<-FALSE
               }  
            }
            temp<-NULL
            for(i in 1:theta.line)
              temp<-c(temp,unlist(strsplit(Result.LST[theta.id[1]+3+theta.line+i],split=" ")))
            temp<-as.numeric(temp[temp!=""])
            THETA<-temp
      
            seTHETA<-rep(NA,length(THETA))  
            if(length(theta.id)!=1)
            {  temp<-NULL
               for(i in 1:theta.line)
                  temp<-c(temp,unlist(strsplit(Result.LST[theta.id[2]+3+theta.line+i],split=" ")))
               temp<-as.numeric(temp[temp!=""])
               seTHETA<-temp
            } 
      
            omega.id<-grep("OMEGA",Result.LST)
            omega.line<-0
            omega.flag<-T
            while(omega.flag)
            {  if(Result.LST[omega.id[1]+3+omega.line]!=" ")
               {  omega.line<-omega.line+1
               } else
               {  omega.flag<-FALSE
               }  
            }
            temp<-NULL
            for(i in 1:omega.line)
               temp<-c(temp,unlist(strsplit(Result.LST[omega.id[1]+2+i],split=" ")))
            temp<-temp[temp!=""]

            N.eta<-length(temp)
            OMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
            seOMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
            omega.name<-NULL
            id.current<-omega.id[1]+4+omega.line  
            id.current1<-omega.id[2]+4+omega.line  
      
            for(i in 1:N.eta)
            {  temp<-NULL;temp1<-NULL
               flag<-T
               while(flag)
               {  id.current<-id.current+1;id.current1<-id.current1+1
                  flag<-Result.LST[id.current]!=" "
                  if(flag)
                  {  temp<-c(temp,unlist(strsplit(Result.LST[id.current],split="+ ")))
                     temp1<-c(temp1,unlist(strsplit(Result.LST[id.current1],split="+ ")))            
                  }
               }
               temp<-temp[temp!=""];temp1<-temp1[temp1!=""]
               temp<-temp[temp!="+"] ;temp1<-temp1[temp1!="+"] 
               temp1[ temp1=="........."]<-NA 
        
               for(j in 1:N.eta)
               {  OMEGA[i,j]<-as.numeric(temp[j])
                  seOMEGA[i,j]<-as.numeric(temp1[j])
                  omega.name<-c(omega.name,paste("OMEGA(",i,"/",j,")",sep=""))
               }  
               id.current<-id.current+1 
               id.current1<-id.current1+1              
            } 

            sigma.id<-grep("SIGMA",Result.LST)
            temp<-unlist(strsplit(Result.LST[sigma.id[1]+6],split="  "))
            temp<-temp[temp!=""]; temp<-temp[temp!="+"]
            SIGMA<-as.numeric(temp)
            seSIGMA<-rep(NA,length(SIGMA))
            if(length(theta.id)!=1)
            {  temp<-unlist(strsplit(Result.LST[sigma.id[2]+6],split="  "))
               temp<-temp[temp!=""]; temp<-temp[temp!="+"]
               seSIGMA<-as.numeric(temp)      
            } 

            names.est<-c(paste("TH",1:length(THETA),sep=""),omega.name,paste("SIGMA",1:length(SIGMA)))      
            EST<-c(THETA,OMEGA,SIGMA)
            SE<-c(seTHETA,seOMEGA,seSIGMA)
            Prob<-k
            if(sum(!is.na(seTHETA))==0)
            {  COV.s<-"NONE"
               cond.num<-NA
            } else
            {  COV.s<-"OK"
            }

            Delta<-as.numeric(svalue(base.obj.t))-Obj
            temp<-c(Prob,Delta,Obj,Min,COV.s,EST,SE)
            random.table<-rbind(random.table,temp)
#            colnames(random.table)<-c("Prob","Delta","Obj","Min","Covariance",names.est,paste(names.est,":se",sep=""))            
#            random.table<-data.frame(random.table)
#            sort.id<-sort.list(as.numeric(as.character(random.table$Delta[-1])),decreasing=T)
#            random.T<-random.table[-1,]
#            random.T<-rbind(random.table[1,],random.T[sort.id,])
#            RAN.result<<- random.T                       
         }  
         dispose(RT.win)
         colnames(random.table)<-c("Prob","Delta","Obj","Min","Covariance",names.est,paste(names.est,":se",sep=""))
         random.table<-data.frame(random.table)
         sort.id<-sort.list(as.numeric(as.character(random.table$Delta[-1])),decreasing=T)
         random.T<-random.table[-1,]
         random.T<-rbind(random.table[1,],random.T[sort.id,])
         Quantile<-(0:iteration.N)/iteration.N
         random.T<-cbind(random.T[,1:3],Quantile,random.T[,-c(1:3)])
         Quantile<-Quantile[-1]
         data.delta<-as.numeric(as.character(random.table[1,2]))
         Delta<-as.numeric(as.character(random.T[-1,2]))
         plot(as.numeric(Delta),1-Quantile,type='n',xlim=c(min(Delta,0,data.delta),max(data.delta,Delta,0)*1.3),
              ylim=c(0,1),xlab="Delta : OBJ(Reference-test with permuted data)",
               main=paste(file.id,":",cov.T),               
                sub="Green line = OBJ(Reference-test with original data)",ylab="Quantile",axes=F,col="grey10")
         axis(1)
         for(i in 1:length(Delta))
            lines(c(0,Delta[i]),c(1-Quantile[i],1-Quantile[i]),col="grey")
         cutoff.id<-max(round(0.05*iteration.N),1)
         lines(c(0,Delta[cutoff.id]),c(1-Quantile[cutoff.id],1-Quantile[cutoff.id]),col=2,lwd=2)
         text(max(Delta),1-Quantile[cutoff.id],round(Delta[cutoff.id],3),cex=0.6)
         text(Delta[cutoff.id]/2,1-Quantile[cutoff.id],"5%",cex=0.6)
         data.quantile<-1-sum(Delta>data.delta)/iteration.N
         lines(c(0,data.delta),c(data.quantile,data.quantile),col=3,lwd=2)   
         text(data.delta*1.1,data.quantile,round(data.delta,3),cex=0.6)        
         RAN.result<<- random.T  
         tempR<<-random.table   
      }

      Rplot<-function()
      {  random.T<-RAN.result[-1,]
         data.delta<-as.numeric(as.character(RAN.result[1,2]))
         Delta<-as.numeric(as.character(random.T[,2]))
         iteration.N<-as.numeric(svalue(iteration.n))
         cov.T<-svalue(cov.t)
         Quantile<-(1:iteration.N)/iteration.N
     
         plot(as.numeric(Delta),1-Quantile,type='n',xlim=c(min(Delta,0,data.delta),max(data.delta,Delta,0)*1.3),
              ylim=c(0,1),xlab="Delta : OBJ(Reference-test with permuted data)",
               main=paste(file.id,":",cov.T),               
                sub="Green line = OBJ(Reference-test with original data)",ylab="Quantile",axes=F,col="grey10")
         axis(1)
         for(i in 1:length(Delta))
            lines(c(0,Delta[i]),c(1-Quantile[i],1-Quantile[i]),col="grey")
         cutoff.id<-max(round(0.05*iteration.N),1)
         lines(c(0,Delta[cutoff.id]),c(1-Quantile[cutoff.id],1-Quantile[cutoff.id]),col=2,lwd=2)
         text(max(Delta),1-Quantile[cutoff.id],round(Delta[cutoff.id],3),cex=0.6)
         text(Delta[cutoff.id]/2,1-Quantile[cutoff.id],"5%",cex=0.6)
         data.quantile<-1-sum(Delta>data.delta)/iteration.N
         lines(c(0,data.delta),c(data.quantile,data.quantile),col=3,lwd=2)   
         text(data.delta*1.1,data.quantile,round(data.delta,3),cex=0.6)        
      }
      
      timewin<-gwindow("Randomization test")
      Bgroup<-ggroup(cont=timewin,horizontal=TRUE)
      BBgroup<-ggroup(cont=Bgroup,horizontal=FALSE)
 
      tmp<-gframe("Find run number subfolder",cont=BBgroup)
      control.t<-gedit("",width=50)
      button1<-gbutton("Open control file",handler=openControl)
      add(tmp,button1)
      add(tmp,control.t)

      tmp<-gframe("Find run number subfolder",cont=BBgroup)
      button2<-gbutton("Open data files",handler=opendata,width=20,height=10)
      data.t<-gedit(" ",width=50)
      add(tmp,button2)
      add(tmp,data.t) 
      add(Bgroup,ggraphics())
   }
   
 TIMEvsDVandPREDID.plot<-function(h,...)
 {    runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
      outfile.name<-paste(runnum.path,"\\",FILE.ID,".noh",sep="")
      Output.Table<-read.table(outfile.name,skip=1,header=T,na.string=".")
      D.data<-Output.Table[SEL.ID,]
      n<-nrow(D.data)
      p<-names(table(D.data$ID))

      updatePlot1<-function(h,...) 
      {  select.pred<-svalue(PRED.sel)
         select.id<-svalue(IDlist)
            id.list<-which(D.data$ID==select.id)
                if(length(id.list)!=0)
                {  y.lim<-range(D.data$DV[id.list],D.data$IPRED[id.list],D.data[id.list,select.pred],na.rm=T)
                   plot(D.data$TIME[id.list],D.data[id.list,"DV"],ylim=y.lim,col="grey10",cex=1.2,xlab="TIME",ylab="",main=paste("ID",select.id,sep=" "))
                   lines(D.data$TIME[id.list],D.data[id.list,"DV"],lwd=1)
                   lines(D.data$TIME[id.list],D.data[id.list,select.pred],col=4,lwd=1)
                   lines(D.data$TIME[id.list],D.data[id.list,"IPRED"],col=2,lwd=1)
                }
      }
      
      ID<-as.character(sort(unique(D.data$ID)))
      ID<-matrix(ID,nrow=length(ID))
      colnames(ID)<-c("ID")

      window<-gwindow("Predictions and DV vs TIME by ID plot")
      Biggroup<-ggroup(cont=window)
      group=ggroup(horizontal=FALSE,cont=Biggroup)
      tmp<-gframe(" Y variable",container=group)
      if(read.table("NM.version")=="NM6")
      {  PRED.list<-PRED.list.6
      } else
      {  PRED.list<-PRED.list.7
      }
      PRED.list<-PRED.list[PRED.list!="IPRED"]
      PRED.sel<-gdroplist(PRED.list)
      add(tmp,PRED.sel)
      tmp<-gframe("ID",container=group)
      IDlist <- gtable(ID,multiple=T,handler=updatePlot1) 
      size(IDlist)<-c(100,200)
      add(tmp,IDlist)
      tmp<-gframe("DV   : black",container=group)
      tmp<-gframe("IPRED : red",container=group)
      tmp<-gframe("Selected prediction : blue",container=group)   
      add(Biggroup,tmp)
      add(Biggroup, ggraphics())       
}
   
# TIMEvsDVandPREDID.plot<-function(h,...)
# {    updatePlot<-function(h,...) 
#      {  select.pred<-svalue(PRED.sel)
#         nb<-gnotebook()
#         runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
#         outfile.name<-paste(runnum.path,"\\",FILE.ID,".noh",sep="")
#         D.data<-read.table(outfile.name,skip=1,header=T,na.string=".")
#         D.data<-read.csv(paste(FILE.ID,".csv",sep=""),na.string=".")
#         n<-nrow(D.data)
#         p<-names(table(D.data$ID))
#         p.n<-(length(p)+2)/4

#         for(i in 1:p.n)
#         {  ggraphics(container=nb,label=paste("plot",i,sep=" "))
#            add(BigGroup,nb)
#            par(mfrow=c(2,2))
#            for(j in 1:4)
#            {   id.list<-which(D.data$ID==(i-1)*4+j)
#                if(length(id.list)!=0)
#                {  y.lim<-range(D.data$DV[id.list],D.data$IPRED[id.list],D.data[id.list,select.pred],na.rm=T)
#                   plot(D.data$TIME[id.list],D.data[id.list,"DV"],ylim=y.lim,col="grey10",cex=1.2,xlab="TIME",ylab="",main=paste("ID",(i-1)*4+j,sep=" "))
#                   lines(D.data$TIME[id.list],D.data[id.list,"DV"],lwd=1)
#                   lines(D.data$TIME[id.list],D.data[id.list,select.pred],col=2,lwd=1)
#                   lines(D.data$TIME[id.list],D.data[id.list,"IPRED"],col=4,lwd=1)
#                }
#            }
#        }   
#  ggraphics(container=nb,label=paste("plot",i,sep=" "))
#            add(BigGroup,nb)
#            i<-1
#            id.list<-which(D.data$ID==p[i])
#                if(length(id.list)!=0)
#                {  y.lim<-range(D.data$DV[id.list],D.data$IPRED[id.list],D.data[id.list,select.pred],na.rm=T)
#                   plot(D.data$TIME[id.list],D.data[id.list,"DV"],ylim=y.lim,col="grey10",cex=1.2,xlab="TIME",ylab="",main=paste("ID",p[i],sep=" "))
#                   lines(D.data$TIME[id.list],D.data[id.list,"DV"],lwd=1)
#                   lines(D.data$TIME[id.list],D.data[id.list,select.pred],col=2,lwd=1)
#                   lines(D.data$TIME[id.list],D.data[id.list,"IPRED"],col=4,lwd=1)
#                }
      
#         for(i in 1:length(p))
#         {  ggraphics(container=nb,label=paste("plot",i,sep=" "))
#            add(BigGroup,nb)
#            id.list<-which(D.data$ID==p[i])
#                if(length(id.list)!=0)
#                {  y.lim<-range(D.data$DV[id.list],D.data$IPRED[id.list],D.data[id.list,select.pred],na.rm=T)
#                   plot(D.data$TIME[id.list],D.data[id.list,"DV"],ylim=y.lim,col="grey10",cex=1.2,xlab="TIME",ylab="",main=paste("ID",p[i],sep=" "))
#                   lines(D.data$TIME[id.list],D.data[id.list,"DV"],lwd=1)
#                   lines(D.data$TIME[id.list],D.data[id.list,select.pred],col=2,lwd=1)
#                   lines(D.data$TIME[id.list],D.data[id.list,"IPRED"],col=4,lwd=1)
#                }
#       }        

#      }

#      window<-gwindow("Predictions and DV vs TIME by ID plot")
#      BigGroup<-ggroup(cont=window)
#      group=ggroup(horizontal=FALSE,cont=BigGroup)
#      tmp<-gframe(" Y variable",container=group)
#      if(read.table("NM.version")=="NM6")
#      {  PRED.list<-PRED.list.6
#      } else
#      {  PRED.list<-PRED.list.7
#      }
#      PRED.list<-PRED.list[PRED.list!="IPRED"]
#      PRED.sel<-gdroplist(PRED.list)
#      add(tmp,PRED.sel)
#      Button<-gbutton("OK",handler=updatePlot)

#      tmp<-gframe("Plot",container=group)
#      add(tmp,Button,expand=TRUE)
# }    

   ###################################
   # Visual/Numerical Predictive Check
   ###################################
      CalcNPC1<-function(h,...)
      {  
         temp.id<-which(TOT.RUN$data[,"ID"]==VPC.RUN)
         temp.id<-temp.id[length(temp.id)]
         current.dir<-TOT.RUN$data[temp.id,"path"]
#         current.result<-paste(current.dir,"\\",VPC.RUN,".noh",sep="")
         VPC.N<-as.numeric(svalue(N.g1))
         L1<-svalue(VarList.g1)
         L2<-svalue(VarList.g2)
         L3<-svalue(VarList.g3)
        
         if(L1!="NONE")
         {  L1.d<-as.numeric(svalue(id.g1))
            if(L2!="NONE")
            {  L2.d<-as.numeric(svalue(id.g2))
               if(L3!="NONE")
               {  L3.d<-as.numeric(svalue(id.g3))              
                  sel.id<-which(D.data[,L1]==L1.d & D.data[,L2]!=L2.d&D.data[,L3]==L3.d)
               } else
               {  sel.id<-which(D.data[,L1]==L1.d & D.data[,L2]!=L2.d)
               }    
            } else if(L3!="NONE")
            {  L3.d<-as.numeric(svalue(id.g3))              
               sel.id<-which(D.data[,L1]==L1.d & D.data[,L3]==L3.d)
            } else
            {  sel.id<-which(D.data[,L1]==L1.d)
            }  
         } else if(L3!="NONE")
         {  L3.d<-as.numeric(svalue(id.g3))              
            sel.id<-which(D.data[,L3]==L3.d)
         } else
         {  sel.id<-1:nrow(D.data)
         }  
 

         sim.data<-readLines("PC.SIM")

#         result.data<-read.table(current.result,skip=1,header=T)
         n.sim<-VPC.N
         n.data<-nrow(D.data)
         sim.data<-sim.data[-c(1,(1:(n.sim-1))*(n.data+2)+1,(1:(n.sim-1))*(n.data+2)+2)]
         temp<-unlist(strsplit(sim.data,split=" "))
         temp<-temp[temp!=""] 
         sim.name<-temp[1:3]
         sim.mat<-matrix(as.numeric(unlist(temp[-c(1:3)])),nrow=n.sim*n.data,byrow=T)
         colnames(sim.mat)<-sim.name
         sim.mat<-data.frame(sim.mat)

         sim.tot<-list()
         Quantile.tot1<-NULL
         for(j in 1:length(sel.id))
         {  i<-sel.id[j]
            temp.list<-list()
            temp.id<-which(sim.mat$ID==sim.mat$ID[i] & sim.mat$TIME==sim.mat$TIME[i])
            sort.DV<-sort(sim.mat$DV[temp.id])
            Q025<-sort.DV[max(round(n.sim*0.025),1)]
            Q05<-sort.DV[max(round(n.sim*0.05),1)]
            Q10<-sort.DV[max(round(n.sim*0.10),1)]
            Q25<-sort.DV[max(round(n.sim*0.25),1)]
            Q50<-sort.DV[round(n.sim*0.50)]
            Q75<-sort.DV[round(n.sim*0.75)]
            Q90<-sort.DV[round(n.sim*0.90)]   
            Q95<-sort.DV[round(n.sim*0.95)]
            Q975<-sort.DV[round(n.sim*0.975)]
            sim.tot[[i]]<-temp.list
            q.temp<-c(sim.mat$ID[i],sim.mat$TIME[i],Q025,Q05,Q10,Q25,
                   Q50,Q75,Q90,Q95,Q975)
            Quantile.tot1<-rbind(Quantile.tot1,q.temp)
         }
         
         DV.data<-read.csv(paste(VPC.RUN,".csv",sep=""),na.string=".")
         if(L1!="NONE")
         {  L1.d<-as.numeric(svalue(id.g1))
            if(L2!="NONE")
            {  L2.d<-as.numeric(svalue(id.g2))
               if(L3!="NONE")
               {  L3.d<-as.numeric(svalue(id.g3))              
                  sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L2]!=L2.d&DV.data[,L3]==L3.d)
               } else
               {  sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L2]!=L2.d)
               }    
            } else if(L3!="NONE")
            {  L3.d<-as.numeric(svalue(id.g3))              
               sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L3]==L3.d)
            } else
            {  sel.id<-which(DV.data[,L1]==L1.d)
            }  
         } else if(L3!="NONE")
         {  L3.d<-as.numeric(svalue(id.g3))              
            sel.id<-which(DV.data[,L3]==L3.d)
         } else
         {  sel.id<-1:nrow(DV.data)
         }  

         DV.data<-DV.data[sel.id,]
         colnames(DV.data)[1]<-"ID"               

         colnames(Quantile.tot1)<-c("ID","TIME","Q025","Q05","Q10","Q25","Q50","Q75","Q90","Q95","Q975")
         if(nrow(Quantile.tot1)!=nrow(DV.data))
         {  Quantile.tot<-merge(Quantile.tot1,DV.data,all=TRUE) 
         } else
         {  del.id<-c(which(colnames(DV.data)=="ID"),which(colnames(DV.data)=="TIME"))
            Quantile.tot<-cbind(Quantile.tot1,DV.data[,-del.id]) 
         }    
               
         colnames(Quantile.tot)<-c("ID","TIME","Q025","Q05","Q10","Q25","Q50","Q75","Q90","Q95","Q975",colnames(DV.data)[-c(1:2)])
         rownames(Quantile.tot)<-NULL
         Quantile.tot<-data.frame(Quantile.tot)
         Quantile.keep<<-Quantile.tot
         alarm()
      }

      CalcNPC2<-function(h,...)
      {  VPC.N<-as.numeric(svalue(N.g1))
         temp.id<-which(TOT.RUN$data[,"ID"]==VPC.RUN)
         temp.id<-temp.id[length(temp.id)]
         current.dir<-TOT.RUN$data[temp.id,"path"]
 #        current.result<-paste(current.dir,"\\",VPC.RUN,".noh",sep="")

         VPC.win<-gwindow("Predictive checks progress",width=300,height=50)
         VPC.progress<-gslider(from=0,to=100,by=1,value=0,cont=VPC.win)
         svalue(VPC.progress)<-0
#         result.data<-read.table(current.result,skip=1,header=T)
         n.sim<-VPC.N
         n.data<-nrow(D.data)

         L1<-svalue(VarList.g1)
         L2<-svalue(VarList.g2)
         L3<-svalue(VarList.g3)

         if(L1!="NONE")
         {  L1.d<-as.numeric(svalue(id.g1))
            if(L2!="NONE")
            {  L2.d<-as.numeric(svalue(id.g2))
               if(L3!="NONE")
               {  L3.d<-as.numeric(svalue(id.g3))              
                  sel.id<-which(D.data[,L1]==L1.d & D.data[,L2]!=L2.d&D.data[,L3]==L3.d)
               } else
               {  sel.id<-which(D.data[,L1]==L1.d & D.data[,L2]!=L2.d)
               }    
            } else if(L3!="NONE")
            {  L3.d<-as.numeric(svalue(id.g3))              
               sel.id<-which(D.data[,L1]==L1.d & D.data[,L3]==L3.d)
            } else
            {  sel.id<-which(D.data[,L1]==L1.d)
            }  
         } else if(L3!="NONE")
         {  L3.d<-as.numeric(svalue(id.g3))              
            sel.id<-which(D.data[,L3]==L3.d)
         } else
         {  sel.id<-1:nrow(D.data)
         }  
         print(length(sel.id))
         TIME.id<-as.numeric(as.character(names(table(round(D.data[sel.id,"TIME"],3)))))
         print(length(TIME.id))
         Quantile.tot1<-NULL
         NN<-0
         for(i in 1:length(TIME.id))
         {  b<-round(i/length(TIME.id)*100)
            svalue(VPC.progress)<-b

            ID.list<-sel.id[which(round(D.data[sel.id,"TIME"],3)==TIME.id[i])]; NN<-NN+length(ID.list)
            con<-file("PC.SIM","r")
            TT<-list()
            temp<-scan(con,skip=1+ID.list[1],nlines=1,quiet=TRUE)
            if(length(ID.list)>1)
            {  for(ki in 2:length(ID.list))
                  temp<-rbind(temp,scan(con,skip=ID.list[ki]-ID.list[ki-1]-1,nlines=1,quiet=TRUE))
            }
            for(k in 2:n.sim)
            {  temp<-rbind(temp,scan(con,skip=(n.data+1)-ID.list[length(ID.list)]+ID.list[1],nlines=1,quiet=TRUE))
               if(length(ID.list)>1)
               {  for(ki in 2:length(ID.list))
                   temp<-rbind(temp,scan(con,skip=ID.list[ki]-ID.list[ki-1]-1,nlines=1,quiet=TRUE))
               }
            }

            close(con)
            sort.DV<-sort(unlist(temp[,3]))
            n.DV<-length(sort.DV)
            Q025<-sort.DV[max(round(n.DV*0.025),1)]
            Q05<-sort.DV[max(round(n.DV*0.05),1)]
            Q10<-sort.DV[max(round(n.DV*0.10),1)]
            Q25<-sort.DV[max(round(n.DV*0.25),1)]
            Q50<-sort.DV[round(n.DV*0.50)]
            Q75<-sort.DV[round(n.DV*0.75)]
            Q90<-sort.DV[round(n.DV*0.90)]   
            Q95<-sort.DV[round(n.DV*0.95)]
            Q975<-sort.DV[round(n.DV*0.975)]
            N<-length(ID.list)
            q.temp<-c(TIME.id[i],N,Q025,Q05,Q10,Q25,
                     Q50,Q75,Q90,Q95,Q975)
            Quantile.tot1<-rbind(Quantile.tot1,unlist(q.temp))            
         }

         dispose(VPC.win)
         colnames(Quantile.tot1)<-c("TIME","N","Q025","Q05","Q10","Q25","Q50","Q75","Q90","Q95","Q975")
         Quantile.keep<<-data.frame(Quantile.tot1)
         alarm()
      }

      updatePlot<-function(h,...)
      {  L1<-svalue(VarList.g1)
         L2<-svalue(VarList.g2)
         L3<-svalue(VarList.g3)
         Quantile.tot<-Quantile.keep

         DV.data<-read.csv(paste(VPC.RUN,".csv",sep=""),na.string=".")
         if(L1!="NONE")
         {  L1.d<-as.numeric(svalue(id.g1))
            if(L2!="NONE")
            {  L2.d<-as.numeric(svalue(id.g2))
               if(L3!="NONE")
               {  L3.d<-as.numeric(svalue(id.g3))              
                  sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L2]!=L2.d&DV.data[,L3]==L3.d)
               } else
               {  sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L2]!=L2.d)
               }    
            } else if(L3!="NONE")
            {  L3.d<-as.numeric(svalue(id.g3))              
               sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L3]==L3.d)
            } else
            {  sel.id<-which(DV.data[,L1]==L1.d)
            }  
         } else if(L3!="NONE")
         {  L3.d<-as.numeric(svalue(id.g3))              
            sel.id<-which(DV.data[,L3]==L3.d)
         } else
         {  sel.id<-1:nrow(DV.data)
         }  
         DV.data<-DV.data[sel.id,]

         CI.range<-svalue(CI.list)
         plot(DV.data$TIME,DV.data$DV,type='n',
                   xlab="TIME",ylab="DV",ylim=c(min(c(unlist(Quantile.tot[,3:11]),DV.data$DV),na.rm=T),max(c(unlist(Quantile.tot[,3:11]),DV.data$DV),na.rm=T)))
         points(DV.data$TIME,DV.data$DV)
         if(CI.range=="95%")
         {  lines(Quantile.tot$TIME,Quantile.tot$Q025,lty=2,col=2,lwd=2)
            lines(Quantile.tot$TIME,Quantile.tot$Q975,lty=2,col=2,lwd=2)
         } else if(CI.range=="90%")   
         {  lines(Quantile.tot$TIME,Quantile.tot$Q05,lty=2,col=2,lwd=2)
            lines(Quantile.tot$TIME,Quantile.tot$Q95,lty=2,col=2,lwd=2)
         } else if(CI.range=="80%")   
         {  lines(Quantile.tot$TIME,Quantile.tot$Q10,lty=2,col=2,lwd=2)
            lines(Quantile.tot$TIME,Quantile.tot$Q90,lty=2,col=2,lwd=2)
         }  

         VPC.data<<-Quantile.tot
      } 
    
     NumericalCheck<-function(h,...)
      {  Quantile.tot<-Quantile.keep
         L1<-svalue(VarList.g1)
         L2<-svalue(VarList.g2)
         L3<-svalue(VarList.g3)
         
         DV.data<-read.csv(paste(VPC.RUN,".csv",sep=""),na.string=".")

         if(L1!="NONE")
         {  L1.d<-as.numeric(svalue(id.g1))
            if(L2!="NONE")
            {  L2.d<-as.numeric(svalue(id.g2))
               if(L3!="NONE")
               {  L3.d<-as.numeric(svalue(id.g3))              
                  sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L2]!=L2.d&DV.data[,L3]==L3.d)
               } else
               {  sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L2]!=L2.d)
               }    
            } else if(L3!="NONE")
            {  L3.d<-as.numeric(svalue(id.g3))              
               sel.id<-which(DV.data[,L1]==L1.d & DV.data[,L3]==L3.d)
            } else
            {  sel.id<-which(DV.data[,L1]==L1.d)
            }  
         } else if(L3!="NONE")
         {  L3.d<-as.numeric(svalue(id.g3))              
            sel.id<-which(DV.data[,L3]==L3.d)
         } else
         {  sel.id<-1:nrow(DV.data)
         }  


         Dtemp.data<-DV.data[sel.id,]
  
         var.name<-tolower(colnames(Dtemp.data))
         time.id<-which(var.name=="time")
         DV.id<-which(var.name=="dv")
         if(sum(var.name=="mdv")==0)
         {  D.temp<-D.data
         } else
         {  D.temp<-D.data[(D.data[,which(var.name=="mdv")]==0),] 
         }
         
          time.list<-as.numeric(names(table(round(Dtemp.data[,time.id],3))))
   

         S1<-nrow(Dtemp.data)
         S2<-nrow(D.temp)
         S3<-VPC.N  
#         tempMed<-tapply(Quantile.tot$Q50,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
#         tempU<-tapply(Quantile.tot$Q975,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
#         tempL<-tapply(Quantile.tot$Q025,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
#         tempU1<-tapply(Quantile.tot$Q95,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
#         tempL1<-tapply(Quantile.tot$Q05,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
#         tempU2<-tapply(Quantile.tot$Q90,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
#         tempL2<-tapply(Quantile.tot$Q10,Quantile.tot$TIME,function(x) mean(x,na.rm=T))
         Quantile.tot$TIME<-round(Quantile.tot$TIME,3)
         Dtemp.data$TIME<-round(Dtemp.data$TIME,3)
         NPC.tot<-NULL
         for(i in 1:length(time.list))
         {  temp.id<-which(Dtemp.data[,time.id]==time.list[i])
            temp.id1<-which(Quantile.tot$TIME==time.list[i])        
            S4<-sum(Dtemp.data[temp.id,DV.id]>Quantile.tot$Q50[temp.id1],na.rm=T)
            S5<-sum(Dtemp.data[temp.id,DV.id]<=Quantile.tot$Q50[temp.id1],na.rm=T)
            S6<-sum(Dtemp.data[temp.id,DV.id]>Quantile.tot$Q975[temp.id1],na.rm=T)
            S7<-sum(Dtemp.data[temp.id,DV.id]<Quantile.tot$Q025[temp.id1],na.rm=T)  
            S8<-sum(Dtemp.data[temp.id,DV.id]>Quantile.tot$Q95[temp.id1],na.rm=T)
            S9<-sum(Dtemp.data[temp.id,DV.id]<Quantile.tot$Q05[temp.id1],na.rm=T)
            S10<-sum(Dtemp.data[temp.id,DV.id]>Quantile.tot$Q90[temp.id1],na.rm=T)
            S11<-sum(Dtemp.data[temp.id,DV.id]<Quantile.tot$Q10[temp.id1],na.rm=T)
            NPC.tot<-rbind(NPC.tot,c(S4,S5,S6,S7,S8,S9,S10,S11))
         }
         NPC.sum<-apply(NPC.tot,2,sum)

         NPC.txt<-paste("---------------------------------------------\n")
         NPC.txt<-paste(NPC.txt,"Number of records        : ",nrow(Dtemp.data),"\n",sep="")
         NPC.txt<-paste(NPC.txt,"Total observations       : ",nrow(D.temp),"\n",sep="")
         NPC.txt<-paste(NPC.txt,"Number of iteration      : ",S3,"\n",sep="")
         NPC.txt<-paste(NPC.txt,"=============================================\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points above the medians : ",NPC.sum[1],
                             "(",round(NPC.sum[1]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points below the medians : ",NPC.sum[2],
                             "(",round(NPC.sum[2]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Ratio of points above to points below : ",
                                  round(NPC.sum[1]/NPC.sum[2],2),"\n",sep="")
         NPC.txt<-paste(NPC.txt,"=============================================\n",sep="")
         NPC.txt<-paste(NPC.txt,"***  95% Prediction interval ***\n",sep="")
         NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points above 95% PI      : ",NPC.sum[3],
                             "(",round(NPC.sum[3]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points below 95% PI      : ",NPC.sum[4],
                             "(",round(NPC.sum[4]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Ratio of points above to points below : ",
                                  round(NPC.sum[3]/NPC.sum[4],2),"\n",sep="")
         NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
         NPC.txt<-paste(NPC.txt,"***  90% Prediction interval ***\n",sep="")
         NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points above 90% PI      : ",NPC.sum[5],
                             "(",round(NPC.sum[5]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points below 90% PI      : ",NPC.sum[6],
                             "(",round(NPC.sum[6]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Ratio of points above to points below : ",
                                  round(NPC.sum[5]/NPC.sum[6],2),"\n",sep="")
         NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
         NPC.txt<-paste(NPC.txt,"***  80% Prediction interval ***\n",sep="")
         NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points above 80% PI      : ",NPC.sum[7],
                             "(",round(NPC.sum[7]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Points below 80% PI      : ",NPC.sum[8],
                             "(",round(NPC.sum[8]/nrow(Dtemp.data)*100,1),"%)","\n",sep="")
         NPC.txt<-paste(NPC.txt,"Ratio of points above to points below : ",
                                  round(NPC.sum[7]/NPC.sum[8],2),"\n",sep="")
         NPC.txt<-paste(NPC.txt,"---------------------------------------------\n",sep="")
         edit.win<-gwindow("Predictive checks")
         g<-ggroup(horizontal=FALSE,cont=edit.win)
         tmp<-gframe("",container=g)
         a<-gtext(NPC.txt,width=410,height=310,font.attr=c(sizes="large",family="monospace"))
         add(tmp,a)
         NPC<<-NPC.txt
      }
     
      plot.save<-function(h,...)
      {  write.csv(VPC.data,paste(gfile(text="Save VPC data as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
      }

      NumericalCheck.save<-function(h,...)
      {  write.table(NPC,paste(gfile(text="Save predictive check result(name.txt)",
              type="save"),".txt",sep=""),row.names=F)
      }   

      openControl<-function(h,...)
      {  control.file<-gfile(text="Open control file",type="open")
         current.ctl<<-readLines(control.file)
         svalue(control.t)<-control.file
         temp<-strsplit(control.file,split="\\\\")[[1]]
         VPC.RUN<-temp[length(temp)]
         VPC.dir<<-strsplit(control.file,split=VPC.RUN)[[1]]
         setwd(VPC.dir)
         VPC.RUN<<-strsplit(VPC.RUN,split="\\.")[[1]][1]
      }
 
      opendata<-function(h,...)
      {  data.file<<-gfile(text="Open data file",type="open")
         D.data<<-read.csv(data.file,na.string=".")
         svalue(data.t)<-data.file
      }

      VPCwithFile<-function(h,...)
      {  VPC2GUI()
      }

      SavePC<-function(h,...)
      {  file.PC<<-gfile(text="Save Predictive check calculation as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv"))))
          write.csv(Quantile.keep,paste(file.PC,".csv",sep=""),row.names=F)
          svalue(edit14)<-paste(file.PC,".csv",sep="")       
      }
      
      OpenPC<-function(h,...)
      {  file.PC<<-gfile(text="Open predictive check calculation",
              type="open",filter=list("csv files"=list(patterns=c("*.csv"))))
          svalue(edit14)<-file.PC       
          Quantile.keep<<-read.csv(file.PC)
      }     
      openF<-function(h,...)
      {  VPC.dir<<-gfile("Select run number foler with PC.sim",type="selectdir")
         setwd(VPC.dir)
         svalue(dir.g1)<-VPC.dir
         temp<-strsplit(VPC.dir,split="\\\\")[[1]]
         VPC.RUN<<-temp[length(temp)]
         current.result<<-paste(VPC.RUN,".noh",sep="")
         D.data<-read.csv(paste(VPC.RUN,".csv",sep=""),na.string=".")
       
         Var.Name<-colnames(D.data)
         group1<<-ggroup(horizontal=TRUE,cont=group)   
         From.label.1<-glabel("")
         VarList.g1<<-gdroplist(c("MDV",Var.Name))
         VarList.g2<<-gdroplist(c("NONE",c("NONE",Var.Name)))
         id.g1<<-gedit("0",width=10)
         id.g2<<-gedit(" ",width=10)
         label.g1<<-glabel("Include")
         label.g2<<-glabel("Exclude")
      
         tmp<-gframe("Select data",container=group1)
         add(tmp,label.g1)
         add(tmp,VarList.g1)
         add(tmp,id.g1)
         add(tmp,label.g2)
         add(tmp,VarList.g2)
         add(tmp,id.g2)

         id.g3<<-gedit(" ",width=10)
         VarList.g3<<-gdroplist(c("NONE",c("NONE",Var.Name)))
         tmp<-gframe("Stratification",container=group)
         add(tmp,VarList.g3)
         add(tmp,id.g3)
   
#         Button1<<-gbutton("Small data",handler=CalcNPC1)
         tmp<-gframe("Calculate predictive checks",cont=group)
#         add(tmp,Button1)

         Button2<<-gbutton("OK",handler=CalcNPC2)
         add(tmp,Button2)
         
         tmp<-gframe("Save predictive checks calculation",cont=group)

         Button13<<-gbutton("OK",handler=SavePC)
         add(tmp,Button13)

         tmp<-gframe("Load predictive checks calculation",cont=group)

         Button14<<-gbutton("Open",handler=OpenPC)
         edit14<<-gedit("")
         add(tmp,Button14)
         add(tmp,edit14)
 
         CI.list<<-gdroplist(c("95%","90%","80%"))
         Button<<-gbutton("OK",handler=updatePlot)
#         Button.save<<-gbutton("save",handler=plot.save)
         tmp<-gframe("PI",cont=group)
         add(tmp,CI.list)
         tmp<-gframe("Plot",cont=group)
         add(tmp,Button)  
#         add(tmp,Button.save)
                   
         tmp<-gframe("Summary",cont=group)
         Button3<<-gbutton("OK",handler=NumericalCheck)
         Button3.save<<-gbutton("save",handler=NumericalCheck.save)
         add(tmp,Button3)  
         add(tmp,Button3.save)  
         add(BigGroup,ggraphics())
      }

     
      VPC1GUI<-function(h,...)
      {  vpcwin<<-gwindow("Predictive checks")
         BBgroup<-ggroup(cont=vpcwin,horizontal=FALSE)
 
         tmp<-gframe("Find run number subfolder",cont=BBgroup)
         control.t<<-gedit(" ",width=50)
         button1<-gbutton("Open control file",handler=openControl)
         add(tmp,button1)
         add(tmp,control.t)

         tmp<-gframe("Find run number subfolder ",cont=BBgroup)
         button2<-gbutton("Open data file for predictive checks",handler=opendata,width=20,height=10)
         data.t<<-gedit(" ",width=50)
         add(tmp,button2)
         add(tmp,data.t)
 
         tmp=gframe("Number of simulations",container=BBgroup)
         VPC.label<-glabel("")
         VPC.input<<-gedit("1000",width=10)
         Button<-gbutton("Start predictive checks",handler=set.VPCN)
         add(tmp,VPC.label)
         add(tmp,VPC.input)
         tmp=gframe("",container=BBgroup)
         add(tmp,Button)
         ButtonF<-gbutton("Predictive checks with PC.sim ($TABLE ID TIME  FILE=PC.SIM)",handler=VPCwithFile)
         tmp=gframe("",container=BBgroup)
         add(tmp,ButtonF) 
     }
     
     set.VPCN<-function(h,...)
     {  VPC.N<<-as.numeric(svalue(VPC.input))
        VPC.try()
     }

     VPC.try<-function()
     {  Current.CTL<-current.ctl
        temp<-strsplit(Current.CTL,split=" ")
        indicator<-NULL
        for(i in 1:length(temp))
           indicator<-rbind(indicator,temp[[i]][1])
        t.id<-which(indicator=="$EST" | indicator=="$ESTIMATION")
        VPC.CTL<-Current.CTL[1:(t.id[1]-1)]
        temp.ctl1<-paste("$SIMULATION (20030521) ONLYSIM SUBPROBLEMS=",VPC.N,sep="")
        input.id<-which(indicator=="$INPUT")
        input.CTL<-Current.CTL[input.id]
        temp.ctl2<-"$TABLE ID TIME DV NOAPPEND NOPRINT ONEHEADER FILE=PC.SIM"
      
        VPC.CTL<-c(VPC.CTL,temp.ctl1,temp.ctl2)
        write.table(VPC.CTL,"PC.CTL",quote=FALSE,row.names=FALSE,col.names=FALSE)
        VPC.command<-paste(Default.NMpath," PC.ctl PC.res")
        system(VPC.command,invisible=F,show.output.on.console=F)      
        VPC2GUI()
     }
    
     VPC2GUI<-function()
     {   VPC.N<<-as.numeric(svalue(VPC.input)) ;print(VPC.N)    
         dispose(vpcwin)
         win<-gwindow("Predictive checks")
         BigGroup<<-ggroup(cont=win)
         group<<-ggroup(horizontal=FALSE,cont=BigGroup)     
      
         dir.g1<<-gedit("")
         button.g1<-gbutton("Open folder",handler=openF)

         tmp<-gframe("Run number folder with PC.sim",container=group)
         add(tmp,button.g1)
         add(tmp,dir.g1)

      
         N.g1<<-gedit(VPC.N)
         tmp<-gframe("Number of simulated sample",container=group)
         add(tmp,N.g1)
    }

   ###################################
   # Bootstrap
   ###################################
   
   Boot.ctl<-function()
   {  Current.CTL<-current.ctl
      temp.CTL<-Current.CTL
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
      write.table(Boot.CTL,"boot.ctl",quote=FALSE,row.names=FALSE,col.names=FALSE)
   }

   show.BTsummary<-function()
   {  
      temp.id<-which(TOT.RUN$data[,"ID"]==Boot.RUN)
      temp.id<-temp.id[length(temp.id)]
      current.dir<-TOT.RUN$data[temp.id,"path"]
      D.LST<-readLines(paste(current.dir,"\\",Boot.RUN,".res",sep=""))
      D.temp<-matrix(D.LST)
      indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])

      final.start.id<-grep("FINAL PARAMETER ESTIMATE",D.LST)
      final.start.id<-final.start.id[length(final.start.id)]
      Result.LST<-D.LST[final.start.id:length(D.LST)]

      theta.id<-grep("THETA",Result.LST)
      theta.line<-0
      theta.flag<-T
      while(theta.flag)
      {  if(Result.LST[theta.id[1]+3+theta.line]!=" ")
         {  theta.line<-theta.line+1
         } else
         {  theta.flag<-FALSE
         }  
      }
      temp<-NULL
      for(i in 1:theta.line)
         temp<-c(temp,unlist(strsplit(Result.LST[theta.id[1]+3+theta.line+i],split=" ")))
      temp<-as.numeric(temp[temp!=""])
      THETA<-temp
      N.theta<-length(THETA)
      
      seTHETA<-rep(NA,length(THETA))  
      if(length(theta.id)!=1)
      {  temp<-NULL
         for(i in 1:theta.line)
            temp<-c(temp,unlist(strsplit(Result.LST[theta.id[2]+3+theta.line+i],split=" ")))
         temp<-as.numeric(temp[temp!=""])
         seTHETA<-temp
      } 

      omega.id<-grep("OMEGA",Result.LST)
      omega.line<-0
      omega.flag<-T
      while(omega.flag)
      {  if(Result.LST[omega.id[1]+3+omega.line]!=" ")
         {  omega.line<-omega.line+1
         } else
         {  omega.flag<-FALSE
         }  
      }
      temp<-NULL
      for(i in 1:omega.line)
         temp<-c(temp,unlist(strsplit(Result.LST[omega.id[1]+2+i],split=" ")))
      temp<-temp[temp!=""]

      N.eta<-length(temp)
      OMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
      seOMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
      omega.name<-NULL
      id.current<-omega.id[1]+4+omega.line  
      id.current1<-omega.id[2]+4+omega.line  
      
      for(i in 1:N.eta)
      {  temp<-NULL;temp1<-NULL
         flag<-T
         while(flag)
         {  id.current<-id.current+1;id.current1<-id.current1+1
            flag<-Result.LST[id.current]!=" "
            if(flag)
            {  temp<-c(temp,unlist(strsplit(Result.LST[id.current],split="+ ")))
               temp1<-c(temp1,unlist(strsplit(Result.LST[id.current1],split="+ ")))            
            }
         }
         temp<-temp[temp!=""];temp1<-temp1[temp1!=""]
         temp<-temp[temp!="+"] ;temp1<-temp1[temp1!="+"] 
         temp1[ temp1=="........."]<-NA 
        
         for(j in 1:N.eta)
         {  OMEGA[i,j]<-as.numeric(temp[j])
            seOMEGA[i,j]<-as.numeric(temp1[j])
            omega.name<-c(omega.name,paste("OMEGA(",i,"/",j,")",sep=""))
         }  
         id.current<-id.current+1 
         id.current1<-id.current1+1              
      }

      sigma.id<-grep("SIGMA",Result.LST)
      temp<-unlist(strsplit(Result.LST[sigma.id[1]+6],split="  "))
      temp<-temp[temp!=""]; temp<-temp[temp!="+"]
      SIGMA<-as.numeric(temp)
      N.eps<-length(SIGMA)

      s.Boot.total<-NULL
## All
      Boot.keep.t<-matrix(as.numeric(Boot.keep[,-c(1:4)]),nrow=nrow(Boot.keep))
      Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) mean(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) sd(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.5,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.025,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.975,na.rm=T)}))      
      Boot.summary<-t(Boot.summary)
      Boot.summary<-cbind(c("All",rep(" ",nrow(Boot.summary)-1)),Boot.summary)
      colnames(Boot.summary)<-c("Condition","Estimates","Mean","SD","Median","2.5%","97.5%")
      o.id<-NULL
      for(i in 1:N.eta)
         for(j in i:N.eta)
            o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*N.eta+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
                                  paste("OMEGA(",o.id[,1],"/",o.id[,2],")",sep=""),  
                                  paste("SIGMA(",1:N.eps,"/",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(s.Boot.summary[,1],rownames(s.Boot.summary),s.Boot.summary[,-1])
      s.Boot.total<-rbind(s.Boot.total,s.Boot.summary)

## Minimization Successful

      Boot.keep.t<-matrix(as.numeric(Boot.keep[Boot.keep[,3]=="SUCCESSFUL",-c(1:4)]),nrow=sum(Boot.keep[,3]=="SUCCESSFUL"))
      Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) mean(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) sd(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.5,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.025,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.975,na.rm=T)}))      
      Boot.summary<-t(Boot.summary)
      Boot.summary<-cbind(c("Min success",rep(" ",nrow(Boot.summary)-1)),Boot.summary)
      colnames(Boot.summary)<-c("Condition","Estimates","Mean","SD","Median","2.5%","97.5%")
      o.id<-NULL
      for(i in 1:N.eta)
         for(j in i:N.eta)
            o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*N.eta+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
                                  paste("OMEGA(",o.id[,1],"/",o.id[,2],")",sep=""),  
                                  paste("SIGMA(",1:N.eps,"/",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(s.Boot.summary[,1],rownames(s.Boot.summary),s.Boot.summary[,-1])
      s.Boot.total<-rbind(s.Boot.total,s.Boot.summary)

## Minimization Successful & covariance OK

      Boot.keep.t<-matrix(as.numeric(Boot.keep[Boot.keep[,3]=="SUCCESSFUL"&Boot.keep[,4]=="OK",-c(1:4)]),nrow=sum(Boot.keep[,3]=="SUCCESSFUL"&Boot.keep[,4]=="OK"))
      Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) mean(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) sd(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.5,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.025,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.975,na.rm=T)}))      
      Boot.summary<-t(Boot.summary)
      Boot.summary<-cbind(c("COV OK",rep(" ",nrow(Boot.summary)-1)),Boot.summary)
      colnames(Boot.summary)<-c("Condition","Estimates","Mean","SD","Median","2.5%","97.5%")
      o.id<-NULL
      for(i in 1:N.eta)
         for(j in i:N.eta)
            o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*N.eta+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
                                  paste("OMEGA(",o.id[,1],"/",o.id[,2],")",sep=""),  
                                  paste("SIGMA(",1:N.eps,"/",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(s.Boot.summary[,1],rownames(s.Boot.summary),s.Boot.summary[,-1])
      s.Boot.total<-rbind(s.Boot.total,s.Boot.summary)
      colnames(s.Boot.total)[1:2]<-c("Condition","Parameter")
      win<-gwindow("Bootstrap Summary",width=600,height=300)
      gtable(s.Boot.total,cont=win)
      s.Boot.summary<<-s.Boot.total
  }

   Boot.try<-function()
   {  var.name<-tolower(colnames(D.data))
      ID.id<-which(var.name=="x.id")
      set.seed(as.numeric(svalue(Seed.input)))
      ID.list<-as.numeric(names(table(D.data[,ID.id])))
      Boot.tot<-NULL
      win<-gwindow(paste("Bootstrap progress : B=",B,sep=""),width=300,height=50)
      Boot.progress<-gslider(from=0,to=B,by=1,value=0,cont=win)
 
      for(b in 1:B)
      {  svalue(Boot.progress)<-b
         Boot.sample.id<-sample(ID.list,length(ID.list),replace=T)
         Boot.sample<-NULL
         for(i in 1:length(Boot.sample.id))
         {  id.temp<-which(D.data[,ID.id]==Boot.sample.id[i])
            id.temp<-D.data[id.temp,]
            id.temp$X.ID<-i
            Boot.sample<-rbind(Boot.sample,id.temp)
         }
         write.table(Boot.sample,"boot.csv",quote=FALSE,row.names=FALSE,col.names=FALSE,na=".",sep=",")
         Boot.command<-paste(Default.NMpath," Boot.CTL Boot.RES")
         system(Boot.command,invisible=F,show.output.on.console=F)      

         D.LST<-readLines("Boot.res")
         D.temp<-matrix(D.LST)
         indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
         term.id<-which(indicator=="0PROGRAM")
         
         if(length(term.id)==0)
         { min.id<- which(indicator=="0MINIMIZATION")
         min.id<-min.id[length(min.id)]
         Min<-strsplit(D.LST[min.id],split=" ")[[1]][2]

         indicator<-apply(D.temp,1,function(x) strsplit(x,split=":")[[1]][1])
         temp<-strsplit(D.LST[length(D.LST)-4],split=" ")[[1]]
         temp<-as.numeric(temp[temp!=""&temp!="+"])
         cond.num<-round(max(temp)/min(temp),3)
         obj.id<-which(indicator==" #OBJV")
         if(length(obj.id)!=0)
         {  obj.id<-obj.id[length(obj.id)]
         } else
         {  obj.id<-9+which(indicator==" ********************                           MINIMUM VALUE OF OBJECTIVE FUNCTION                  ********************" )
         } 
         temp<-strsplit(D.LST[obj.id],split=" ")[[1]]
         temp<-temp[3:(length(temp)-3)]
         temp<-as.numeric(temp[temp!=""])
         Obj<-temp[!is.na(temp)]
         
         final.start.id<-grep("FINAL PARAMETER ESTIMATE",D.LST)
         final.start.id<-final.start.id[length(final.start.id)]
         Result.LST<-D.LST[final.start.id:length(D.LST)]
         theta.id<-grep("THETA",Result.LST)
         theta.line<-0
         theta.flag<-T
         while(theta.flag)
         {  if(Result.LST[theta.id[1]+3+theta.line]!=" ")
            {  theta.line<-theta.line+1
            } else
            {  theta.flag<-FALSE
            }  
         }
         temp<-NULL
         for(i in 1:theta.line)
            temp<-c(temp,unlist(strsplit(Result.LST[theta.id[1]+3+theta.line+i],split=" ")))
         temp<-as.numeric(temp[temp!=""])
         THETA<-temp
      
         seTHETA<-rep(NA,length(THETA))  
         if(length(theta.id)!=1)
         {  temp<-NULL
            for(i in 1:theta.line)
               temp<-c(temp,unlist(strsplit(Result.LST[theta.id[2]+3+theta.line+i],split=" ")))
            temp<-as.numeric(temp[temp!=""])
            seTHETA<-temp
         }   

         omega.id<-grep("OMEGA",Result.LST)
         omega.line<-0
         omega.flag<-T
         while(omega.flag)
         {  if(Result.LST[omega.id[1]+3+omega.line]!=" ")
            {  omega.line<-omega.line+1
            } else
            {  omega.flag<-FALSE
            }  
         }
         temp<-NULL
         for(i in 1:omega.line)
            temp<-c(temp,unlist(strsplit(Result.LST[omega.id[1]+2+i],split=" ")))
         temp<-temp[temp!=""]

         N.eta<-length(temp)
         OMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
         seOMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
         omega.name<-NULL
         id.current<-omega.id[1]+4+omega.line  
         id.current1<-omega.id[2]+4+omega.line  
      
         for(i in 1:N.eta)
         {  temp<-NULL;temp1<-NULL
            flag<-T
            while(flag)
            {  id.current<-id.current+1;id.current1<-id.current1+1
               flag<-Result.LST[id.current]!=" "
               if(flag)
               {  temp<-c(temp,unlist(strsplit(Result.LST[id.current],split="+ ")))
                  temp1<-c(temp1,unlist(strsplit(Result.LST[id.current1],split="+ ")))            
               }
            }
            temp<-temp[temp!=""];temp1<-temp1[temp1!=""]
            temp<-temp[temp!="+"] ;temp1<-temp1[temp1!="+"] 
            temp1[ temp1=="........."]<-NA 
         
            for(j in 1:N.eta)
            {  OMEGA[i,j]<-as.numeric(temp[j])
               seOMEGA[i,j]<-as.numeric(temp1[j])
               omega.name<-c(omega.name,paste("OMEGA(",i,"/",j,")",sep=""))
            }  
            id.current<-id.current+1 
            id.current1<-id.current1+1              
         }      

         sigma.id<-grep("SIGMA",Result.LST)
         temp<-unlist(strsplit(Result.LST[sigma.id[1]+6],split="  "))
         temp<-temp[temp!=""]; temp<-temp[temp!="+"]
         SIGMA<-as.numeric(temp)
         seSIGMA<-rep(NA,length(SIGMA))
         if(length(theta.id)!=1)
         {  temp<-unlist(strsplit(Result.LST[sigma.id[2]+6],split="  "))
            temp<-temp[temp!=""]; temp<-temp[temp!="+"]
            seSIGMA<-as.numeric(temp)      
         } 

         names.est<-c(paste("TH",1:length(THETA),sep=""),omega.name,paste("SIGMA",1:length(SIGMA)))      
         EST<-c(THETA,OMEGA,SIGMA)
         SE<-c(seTHETA,seOMEGA,seSIGMA)
         if(sum(!is.na(seTHETA))==0)
         {  COV.s<-"NONE"
            cond.num<-NA
         } else
         {  COV.s<-"OK"
         }

         N.theta<<-length(THETA)
         N.eta<<-ncol(OMEGA)
         N.eps<<-length(SIGMA)
         tot<-c(b,Obj,Min,COV.s,EST,SE)
         } else
         {   tot<-c(b,NA,"TERMINATED","NONE",rep(NA,ncol(Boot.tot)-4))
         }
         Boot.tot<-rbind(Boot.tot,tot)
      colnames(Boot.tot)<-c("Prob","Obj","Min","Covariance",names.est,paste(names.est,":se",sep=""))
      Boot.keep<<-Boot.tot  
      }

      dispose(win)
      colnames(Boot.tot)<-c("Prob","Obj","Min","Covariance",names.est,paste(names.est,":se",sep=""))
      Boot.keep<<-Boot.tot  
      show.BTsummary()
   }

   show.BTsummary1<-function(Boot.keep.A,D.LST)
   {  D.temp<-matrix(D.LST)
      indicator<-apply(D.temp,1,function(x) strsplit(x,split=" ")[[1]][1])
      final.start.id<-grep("FINAL PARAMETER ESTIMATE",D.LST)
      final.start.id<-final.start.id[length(final.start.id)]
      Result.LST<-D.LST[final.start.id:length(D.LST)]

      theta.id<-grep("THETA",Result.LST)
      theta.line<-0
      theta.flag<-T
      while(theta.flag)
      {  if(Result.LST[theta.id[1]+3+theta.line]!=" ")
         {  theta.line<-theta.line+1
         } else
         {  theta.flag<-FALSE
         }  
      }
      temp<-NULL
      for(i in 1:theta.line)
         temp<-c(temp,unlist(strsplit(Result.LST[theta.id[1]+3+theta.line+i],split=" ")))
      temp<-as.numeric(temp[temp!=""])
      THETA<-temp
      N.theta<-length(THETA)

      seTHETA<-rep(NA,length(THETA))  
      if(length(theta.id)!=1)
      {  temp<-NULL
         for(i in 1:theta.line)
            temp<-c(temp,unlist(strsplit(Result.LST[theta.id[2]+3+theta.line+i],split=" ")))
         temp<-as.numeric(temp[temp!=""])
         seTHETA<-temp
      } 

      omega.id<-grep("OMEGA",Result.LST)
      omega.line<-0
      omega.flag<-T
      while(omega.flag)
      {  if(Result.LST[omega.id[1]+3+omega.line]!=" ")
         {  omega.line<-omega.line+1
         } else
         {  omega.flag<-FALSE
         }  
      }
      temp<-NULL
      for(i in 1:omega.line)
         temp<-c(temp,unlist(strsplit(Result.LST[omega.id[1]+2+i],split=" ")))
      temp<-temp[temp!=""]

      N.eta<-length(temp)
      OMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
      seOMEGA<-matrix(NA,nrow=N.eta,ncol=N.eta)
      omega.name<-NULL
      id.current<-omega.id[1]+4+omega.line  
      id.current1<-omega.id[2]+4+omega.line  
      
      for(i in 1:N.eta)
      {  temp<-NULL;temp1<-NULL
         flag<-T
         while(flag)
         {  id.current<-id.current+1;id.current1<-id.current1+1
            flag<-Result.LST[id.current]!=" "
            if(flag)
            {  temp<-c(temp,unlist(strsplit(Result.LST[id.current],split="+ ")))
               temp1<-c(temp1,unlist(strsplit(Result.LST[id.current1],split="+ ")))            
            }
         }
         temp<-temp[temp!=""];temp1<-temp1[temp1!=""]
         temp<-temp[temp!="+"] ;temp1<-temp1[temp1!="+"] 
         temp1[ temp1=="........."]<-NA 
        
         for(j in 1:N.eta)
         {  OMEGA[i,j]<-as.numeric(temp[j])
            seOMEGA[i,j]<-as.numeric(temp1[j])
            omega.name<-c(omega.name,paste("OMEGA(",i,"/",j,")",sep=""))
         }  
         id.current<-id.current+1 
         id.current1<-id.current1+1              
      }

      sigma.id<-grep("SIGMA",Result.LST)
      temp<-unlist(strsplit(Result.LST[sigma.id[1]+6],split="  "))
      temp<-temp[temp!=""]; temp<-temp[temp!="+"]
      SIGMA<-as.numeric(temp)
      N.eps<-length(SIGMA)
      s.Boot.total<-NULL
      
## All
      Boot.keep.t<-matrix(as.numeric(unlist(Boot.keep.A[,-c(1:4)])),nrow=nrow(Boot.keep.A))
      Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) mean(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) sd(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.5,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.025,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.975,na.rm=T)}))      
      Boot.summary<-t(Boot.summary)
      Boot.summary<-cbind(c("All",rep(" ",nrow(Boot.summary)-1)),Boot.summary)
      colnames(Boot.summary)<-c("Condition","Estimates","Mean","SD","Median","2.5%","97.5%")
      o.id<-NULL
      for(i in 1:N.eta)
         for(j in i:N.eta)
            o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*N.eta+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
                                  paste("OMEGA(",o.id[,1],"/",o.id[,2],")",sep=""),  
                                  paste("SIGMA(",1:N.eps,"/",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(s.Boot.summary[,1],rownames(s.Boot.summary),s.Boot.summary[,-1])
      s.Boot.total<-rbind(s.Boot.total,s.Boot.summary)

## Minimization Successful

      Boot.keep.t<-matrix(as.numeric(unlist(Boot.keep.A[Boot.keep.A[,3]=="SUCCESSFUL",-c(1:4)])),nrow=sum(Boot.keep.A[,3]=="SUCCESSFUL"))
      Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) mean(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) sd(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.5,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.025,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.975,na.rm=T)}))      
      Boot.summary<-t(Boot.summary)
      Boot.summary<-cbind(c("Min success",rep(" ",nrow(Boot.summary)-1)),Boot.summary)
      colnames(Boot.summary)<-c("Condition","Estimates","Mean","SD","Median","2.5%","97.5%")
      o.id<-NULL
      for(i in 1:N.eta)
         for(j in i:N.eta)
            o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*N.eta+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
                                  paste("OMEGA(",o.id[,1],"/",o.id[,2],")",sep=""),  
                                  paste("SIGMA(",1:N.eps,"/",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(s.Boot.summary[,1],rownames(s.Boot.summary),s.Boot.summary[,-1])
      s.Boot.total<-rbind(s.Boot.total,s.Boot.summary)

## Minimization Successful & covariance OK

      Boot.keep.t<-matrix(as.numeric(unlist(Boot.keep.A[Boot.keep.A[,3]=="SUCCESSFUL"&Boot.keep.A[,4]=="OK",-c(1:4)])),
                                        nrow=sum(Boot.keep.A[,3]=="SUCCESSFUL"&Boot.keep.A[,4]=="OK"))
      Boot.summary<-c(unlist(THETA),unlist(OMEGA),unlist(SIGMA))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) mean(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x) sd(x,na.rm=T)))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.5,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.025,na.rm=T)}))
      Boot.summary<-rbind(Boot.summary,apply(Boot.keep.t,2,function(x){quantile(x,probs=0.975,na.rm=T)}))      
      Boot.summary<-t(Boot.summary)
      Boot.summary<-cbind(c("COV OK",rep(" ",nrow(Boot.summary)-1)),Boot.summary)
      colnames(Boot.summary)<-c("Condition","Estimates","Mean","SD","Median","2.5%","97.5%")
      o.id<-NULL
      for(i in 1:N.eta)
         for(j in i:N.eta)
            o.id<-rbind(o.id,c(i,j))

      select.id<-c(1:N.theta,N.theta+((o.id[,1]-1)*N.eta+o.id[,2]),
                   N.theta+N.eta*N.eta+(1:N.eps)*(1:N.eps))
      s.Boot.summary<-Boot.summary[select.id,]
      rownames(s.Boot.summary)<-c(paste("THETA(",1:N.theta,")",sep=""),
                                  paste("OMEGA(",o.id[,1],"/",o.id[,2],")",sep=""),  
                                  paste("SIGMA(",1:N.eps,"/",1:N.eps,")",sep=""))
      s.Boot.summary<-cbind(s.Boot.summary[,1],rownames(s.Boot.summary),s.Boot.summary[,-1])
      s.Boot.total<-rbind(s.Boot.total,s.Boot.summary)
      colnames(s.Boot.total)[1:2]<-c("Condition","Parameter")
      win<-gwindow("Bootstrap Summary",width=600,height=300)
      gtable(s.Boot.total,cont=win)
      s.Boot.summary<<-s.Boot.total
   }
   
   Boot.B.init<-function()
   {  setB<-function(h,...)
      {  B<<-as.numeric(svalue(Boot.input))
         Boot.ctl()     
         Boot.try()
      }

      openControl<-function(h,...)
      {  control.file<-gfile(text="Open control file",type="open")
         current.ctl<<-readLines(control.file)
         svalue(control.t)<-control.file
         temp<-strsplit(control.file,split="\\\\")[[1]]
         Boot.RUN<-temp[length(temp)]
         setwd(strsplit(control.file,split=Boot.RUN)[[1]])
         Boot.RUN<<-strsplit(Boot.RUN,split="\\.")[[1]][1]
      }
 
      opendata<-function(h,...)
      {  data.file<<-gfile(text="Open data file",type="open")
         D.data<<-read.csv(data.file)
         svalue(data.t)<-data.file
      }
 
     save1<-function(h,...)
     {  write.csv(Boot.keep,paste(gfile(text="Save bootstrap raw data as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
     }

     save2<-function(h,...)
     {  write.csv(s.Boot.summary,paste(gfile(text="Save bootstrap summary data as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv")))),".csv",sep=""),row.names=F)
     }
     
     save3<-function(h,...)
     {   boot.filename<-gfile("Open joined bootstrap raw data file",type="open")
         A<-read.csv(boot.filename)
         D.LST<-readLines(gfile("Open result file(runnumber.res)",type="open"))
         show.BTsummary1(A,D.LST)
     }
     
      bootwin<-gwindow("Bootstrap")
      BBgroup<-ggroup(cont=bootwin,horizontal=FALSE)
 
      tmp<-gframe("",cont=BBgroup)
      control.t<-gedit(" ",width=50)
      button1<-gbutton("Open control file",handler=openControl)
      add(tmp,button1)
      add(tmp,control.t)

      tmp<-gframe("",cont=BBgroup)
      button2<-gbutton("Open data files",handler=opendata,width=20,height=10)
      data.t<-gedit(" ",width=50)
      add(tmp,button2)
      add(tmp,data.t)
  
      tmp=gframe("Number of bootstrap replicates / Seed number",container=BBgroup)
      Boot.label<-glabel("# of replicates")
      Boot.input<-gedit("2000",width=10)
      add(tmp,Boot.label)
      add(tmp,Boot.input)
      Seed.label<-glabel("Seed number")
      Seed.input<<-gedit("0",width=10)
      add(tmp,Seed.label)
      add(tmp,Seed.input)     
            
      Button<-gbutton("Start bootstrap",handler=setB)

      tmp=gframe("",container=BBgroup)
      add(tmp,Button)

      tmp<-gframe("",cont=BBgroup)
      Button1<-gbutton("Save bootstrap raw data as csv",handler=save1)
      add(tmp,Button1)
      tmp<-gframe("",cont=BBgroup)
      Button2<-gbutton("Save bootstrap summary data as csv",handler=save2)
      add(tmp,Button2)
      tmp<-gframe("",cont=BBgroup)
      Button3<-gbutton("Summary data from joined bootstrap raw data file (Prob,Obj,Min,COV,EST,SE)",handler=save3)
      add(tmp,Button3)   
      tmp<-gframe("",cont=BBgroup)
      Button4<-gbutton("Save bootstrap summary data from joined bootstrap raw data file",handler=save2)
      add(tmp,Button4)          
   }

   Boot<-function(h,...)
   {  Boot.B.init()
   }
  
   Help1<-function(h,...)
   {
     browseURL(Default.Helppath)
   }
   
   Help2<-function(h,...)
   {
     browseURL(Alternative.Helppath)
   }   
   PI<-function(h,...)
   {  gmessage("*** Program information ***
                \n\"fit4NM\" stands for \"Fit for NONMEM\".
                \nEun-Kyung Lee, Ph.D. 
                \n         Assistant Professor
                \n         Department of Statistics
                \n         Ewha Womans University
                \n         Seoul, Korea.
                \nGyujeong Noh, M.D. & Ph.D.
                \n         Professor
                \n         Department of Clinical Pharmacology and Therapeutics
                \n         Department of Anesthesiology and Pain Medicine
                \n         Asan Medical Center
                \n         University of Ulsan College of Medicine
                \n         Seoul, Korea.
                \nVersion: 3.1.9 (November 7, 2010)",cont=TRUE,width=600)   

   }
   
   ConfigNotes<-function(h,...)
   {  gmessage("*** Notes before configuration ***
                \nNONMEM path
                \n      Copy \'c:\\NONMEM folder\\run or util\\nmfe6.bat or nmfe7.bat\' to other folder.
                \n      To run NONMEM in R, add REM to the lines containing \'del\' in \'nmfe6.bat\' and \'nmfe7.bat\'.
                \n      Create \'c:\\NONMEM folder\\fit4NM\\' and copy  \'these modified nmfe6.bat or nmfe7.bat' to 'c:\\NONMEM folder\\fit4NM'.
                \n      Use \'these modified nmfe7.bat and nmfe6.bat\' as default and alternative NONMEM pathways.
                \nNONMEM help
                \n      Find \'c:\\NONMEM folder\\html\\index.htm\\' for both of the default and alternative NONMEM.",cont=TRUE,width=600)   

   }
     
   dataNotes<-function(h,...)
   {  gmessage("*** Notes before NONMEM data creation ***
                \nIDs should be successive in ascending order.
                \n   1, 2, 3, 4, 5, ... (O)
                \n   1, 2, 5, 7, 8, ... (X)",cont=TRUE,width=600)   
   }

   showRES<-function(h,...)
   {   runnum.path<-TOT.RUN$data[which(TOT.RUN$data[,"ID"]==FILE.ID),"path"]
       tot.res<-read.csv(paste(runnum.path,"\\",FILE.ID,".sum",sep=""))[,-1]
       temp<-matrix(as.character(as.matrix(tot.res)),ncol=ncol(tot.res))
       temp[is.na(temp)]<-""
       tot.res<-temp
       colnames(tot.res)<-c("Parameters","Estimates","SE","%RSE","Lower","Upper","%Shrinkage","%CV")
       gtable(tot.res, cont=gwindow(paste(FILE.ID,".sum",sep="")),do.subset=TRUE,width=150)
    }

    simulationD<-function(h,...)
    {    
       sim2GUI<-function()
       {   win<-gwindow("Summaries of Simulation")
           BigGroup<<-ggroup(cont=win)
           group<<-ggroup(horizontal=FALSE,cont=BigGroup)    
           dir.g1<<-gedit("")
           button.g1<-gbutton("Open simulated data",handler=openF)
           tmp<-gframe("",container=group)
           add(tmp,button.g1)
           add(tmp,dir.g1)
           N.g1<<-gedit("")
           tmp<-gframe("Number of simulations",container=group)
           add(tmp,N.g1)
      }
      openF<-function(h,...)
      {  SIM.file<<-gfile("Select simulated data",type="open")
         svalue(dir.g1)<-SIM.file
         SIM.var.temp<-colnames(read.table(SIM.file,skip=1,nrows=1,header=T))
         SIM.var<-SIM.var.temp[which(tolower(SIM.var.temp)!="id" &tolower(SIM.var.temp)!="time")]
         tmp<-gframe("Simulation summaries",cont=group)
         var.list<<-gdroplist(SIM.var)  
         add(tmp,var.list)       
         Button1<<-gbutton("Calculate summaries",handler=CalcSIM)
         add(tmp,Button1)
         Button2<<-gbutton("Save summaries",handler=SaveSIM)
         add(tmp,Button2)

         tmp<-gframe("Load summaries of simulation",cont=group)
         Button14<<-gbutton("Open",handler=OpenSIM)
         edit14<<-gedit("")
         add(tmp,Button14)
         add(tmp,edit14)
         CI.list<<-gdroplist(c("95%","90%","80%"))
         tmp<-gframe("PI",cont=group)
         add(tmp,CI.list)
         Button<<-gbutton("OK",handler=updatePlot)
         tmp<-gframe("Plot",cont=group)
         add(tmp,Button)  
         add(BigGroup,ggraphics())
     }

    CalcSIM<-function(h,...)
    {    SIM.win<-gwindow("Summaries of Simulation progress",width=300,height=50)
         SIM.progress<-gslider(from=0,to=100,by=1,value=0,cont=SIM.win)
         svalue(SIM.progress)<-0
         n.sim<-as.numeric(svalue(N.g1))
         # file # of obs
         flag<-TRUE
         i<-0
         while(flag)
         {  i<-i+1
            temp<-readLines(SIM.file,i*1000)
            t.grep<-grep("TABLE",temp)
            if(length(t.grep)>=2)
             { flag<-FALSE
               N.obs<-t.grep[2]-3
             }
         }
         D.data<-read.table(SIM.file,nrows=N.obs,skip=1,header=T)
         TIME.id<-as.numeric(as.character(names(table(round(D.data[,"TIME"],3)))))
         print(length(TIME.id))
         Quantile.tot1<-NULL
         n.data<-N.obs
         NN<-0
         for(i in 1:length(TIME.id))
         {  b<-round(i/length(TIME.id)*100)
            svalue(SIM.progress)<-b

            ID.list<-which(round(D.data[,"TIME"],3)==TIME.id[i]); NN<-NN+length(ID.list)
            con<-file(SIM.file,"r")
            TT<-list()
            temp<-scan(con,skip=1+ID.list[1],nlines=1,quiet=TRUE)
            if(length(ID.list)>1)
            {  for(ki in 2:length(ID.list))
                  temp<-rbind(temp,scan(con,skip=ID.list[ki]-ID.list[ki-1]-1,nlines=1,quiet=TRUE))
            }
            for(k in 2:n.sim)
            {  temp<-rbind(temp,scan(con,skip=(n.data+1)-ID.list[length(ID.list)]+ID.list[1],nlines=1,quiet=TRUE))
               if(length(ID.list)>1)
               {  for(ki in 2:length(ID.list))
                   temp<-rbind(temp,scan(con,skip=ID.list[ki]-ID.list[ki-1]-1,nlines=1,quiet=TRUE))
               }
            }
            close(con)
            colnames(temp)<-colnames(D.data)
            sort.DV<-sort(unlist(temp[, svalue(var.list)]))
            n.DV<-length(sort.DV)
            Q025<-sort.DV[max(round(n.DV*0.025),1)]
            Q05<-sort.DV[max(round(n.DV*0.05),1)]
            Q10<-sort.DV[max(round(n.DV*0.10),1)]
            Q25<-sort.DV[max(round(n.DV*0.25),1)]
            Q50<-sort.DV[round(n.DV*0.50)]
            Q75<-sort.DV[round(n.DV*0.75)]
            Q90<-sort.DV[round(n.DV*0.90)]   
            Q95<-sort.DV[round(n.DV*0.95)]
            Q975<-sort.DV[round(n.DV*0.975)]
            N<-length(ID.list)
            q.temp<-c(TIME.id[i],N,Q025,Q05,Q10,Q25,
                     Q50,Q75,Q90,Q95,Q975)
            Quantile.tot1<-rbind(Quantile.tot1,unlist(q.temp))            
         }

         dispose(SIM.win)
         colnames(Quantile.tot1)<-c("TIME","N","Q025","Q05","Q10","Q25","Q50","Q75","Q90","Q95","Q975")
         Quantile.keep<<-data.frame(Quantile.tot1)
    }

    OpenSIM<-function(h,...)
    {     file.SIM<<-gfile(text="Open summaries of simulation",
              type="open",filter=list("csv files"=list(patterns=c("*.csv"))))
          svalue(edit14)<-file.SIM       
          Quantile.keep<<-read.csv(file.SIM)
    }

    SaveSIM<-function(h,...)
    {  file.SIM<<-gfile(text="Save Predictive check calculation as csv",
              type="save",filter=list("csv files"=list(patterns=c("*.csv"))))
       write.csv(Quantile.keep,paste(file.SIM,".csv",sep=""),row.names=F)
       svalue(edit14)<-paste(file.SIM,".csv",sep="")  
    }

    updatePlot<-function(h,...)
      {  Quantile.tot<-Quantile.keep
         colnames(Quantile.tot)<-NA
         plot.data<-NULL
         for(j in c(3,4,5,7,9,10,11))
           plot.data<-rbind(plot.data,Quantile.tot[,c(1,j)])
         Quantile.tot<-Quantile.keep
         CI.range<-svalue(CI.list)
         plot(plot.data[,1],plot.data[,2],type='n',
                   xlab="TIME",ylab=svalue(var.list),ylim=range(plot.data[,2]),pch=16,cex=0.7,col="grey")
         lines(Quantile.tot$TIME,Quantile.tot$Q50,lty=1,col=2,lwd=2)                 
         if(CI.range=="95%")
         {  lines(Quantile.tot$TIME,Quantile.tot$Q025,lty=1,col=4,lwd=2)
            lines(Quantile.tot$TIME,Quantile.tot$Q975,lty=1,col=4,lwd=2) 
         }else if(CI.range=="90%")   
         {  lines(Quantile.tot$TIME,Quantile.tot$Q05,lty=1,col=4,lwd=2)
            lines(Quantile.tot$TIME,Quantile.tot$Q95,lty=1,col=4,lwd=2) 
         }else if(CI.range=="80%")   
         {  lines(Quantile.tot$TIME,Quantile.tot$Q10,lty=1,col=4,lwd=2)
            lines(Quantile.tot$TIME,Quantile.tot$Q90,lty=1,col=4,lwd=2)
         }
         
      }
      sim2GUI()
   }    
   ###################################
   # main GUI
   ###################################   
   TOT.temp<-list()
   TOT.temp$num<-0
   TOT.temp$data<-c(NULL,NULL,NULL)
   
   TOT.RUN<<-TOT.temp
   TOT.RESULT<<-list()

   NONMEM.win<<-gwindow("GUI for NONMEM",width=850,height=300)
   menu.list<-list(Configuration=
                       list('Notes before configuration'=list(handler=ConfigNotes),
                            'Set NONMEM path-default'=list(handler=NMpath1),
                            'Set NONMEM help-default'=list(handler=NMpathHelp1),
                            'Set NONMEM path-alternative'=list(handler=NMpath2),
                            'Set NONMEM help-alternative'=list(handler=NMpathHelp2),
                            'Set external editor'=list(handler=Editorpath),                            
                            'Save configuration(c:/fit4NM/.Rdata)'=list(handler=saveConfig)
                           ),
                   Data=
                       list(
                            'Data manipulation'=
                                 list('Calculate elapsed time'=list(handler=CalcTime),
                                      'Data join'=list(handler=DataJoinhandler),
                                      'Data split'=list(handler=DataSplit)                                     
                                     ),
                            'NONMEM data'=
                                list('Notes before NONMEM data creation'=list(handler=dataNotes),
                                     'Create NONMEM data'=list(handler=DataPrep),
                                     'Create successive ID'=list(handler=data.ID)),
                            'Explore NONMEM data'=
                                 list('Select data file'=list(handler=OpenEDAData),
                                      'Summary'=list('Summary statistics-continuous'=list(handler=Summary.stat), 
                                                     'Summary statistics-categorical-single level per person'=list(handler=Summary.cat),     
                                                     'Summary statistics-categorical-multiple levels per person'=list(handler=Summary.cat1)),                                                     
                                      'Plot'=list('XY plot'=list(handler=XY.plot),
                                                  'DV vs TIME by ID'=list(handler=ID.plot),
                                                  'DV vs TIME by covariates'=list(handler=IDCOV.plot),
                                                  'Covariate vs covariate'=list(handler=COVvsCOV.plot))
                                     )
                           ),
                   'Control stream'=
                       list('Edit with default editor'=list(handler=EditEditor),
                            'Edit with external editor'=list(handler=ExternalEditor)
                           ),
                   'NONMEM run'=
                       list( 'Run'=list('Notes before run'=list(handler=BeforeRun),
                                        'From default editor'=list(handler=Editor),
                                        'From external editor'=list(handler=ExternalRun),
                                        'Direct run'=list(handler=DirectRun)),                                     
                            'Run table'=list('Make run table from runnumber subfolders'=list(handler=AddRunTable),
                                             'Save run table as csv'=list(handler=saveRUNTABLE.handler),
                                             'Load run table'=list(handler=loadRUNTABLE.handler)),   
                            'Model tree'=list(handler=Tree.handler),
                            'Explore output'=
                                 list('Select output data' = list(handler=outputselect),
                                      'View run summary' = list(handler=showRES),                                 
                                      'Plot'=list('XY plot'=list(handler=postXY.plot),                                    
                                                  'PRED vs DV'=list(handler=DVvsPRED.plot),
                                                  'RES vs DV'=list(handler=DVvsRES.plot), 
                                                  'RES vs TIME'=list(handler=TIMEvsRES.plot),                  
                                                  'Predictions and DV vs TIME'=list(handler=TIMEvsDVandPRED.plot),
                                                  'Predictions and DV vs TIME by ID'=list(handler=TIMEvsDVandPREDID.plot),                                                 
                                                  'Covariate vs parameter'=list(handler=EBEvsCOV.plot))),   
                            'Explore simulated data'=list(handler=simulationD),                                                                           
                            'Open R terminal with xpose'=list(handler=OpenXpose)
                           ),
                   'Model evaluations'=
                       list('Notes before model evaluations'=list(handler=VPCNote),  
                            'Randomization test'=list(handler=RandomTest),                     
                            'Predictive checks'=list(handler=VPC1GUI),
                            'Bootstrap'=list(handler=Boot)
                           ),   
                   'NONMEM help'=list('Open default help'=list(handler=Help1),
                                      'Open alternative help'=list(handler=Help2)),    
                   'Program information'=  list(handler=PI),                           
                   'Exit'=  list(handler=function(h,...) dispose(NONMEM.win))
                  ) 
   gmenu(menu.list,cont=NONMEM.win)
   table.name<-c("Run number","Date","Time","MIN","COV","OFV","AIC","AICc","SBC","Condtion number",
                "Parents","Model description","# of parameters") 
   nonmem.run<<-matrix("",ncol=length(table.name),nrow=2)
   colnames(nonmem.run)<-table.name
   run.table<<-gtable(nonmem.run,chosencol=length(table.name),cont=NONMEM.win)
}


 

   



