library(extremevalues)
for(i in list.files()[grep("*.csv", list.files())]){
        #set the workign environment to the directory contianing the data
        data_set <- read.csv(i)[,c(1,2)]
        y <- data_set$thickness
        y <- sort(y[!is.na(y)])
        print(summary(y))
        #detect outliers usign the getOutliers function. rho=c(1,1)
        L_0_95 = getOutliers(y, method = "I", rho = c(0.01,1), FLim=c(0,0.95), distribution= "lognormal")
        L_0_75 = getOutliers(y, method = "I", rho = c(0.01,1), FLim=c(0,0.75), distribution= "lognormal")
        
        dir.create("output_methodI_0_95", showWarnings = FALSE)
        #generate plots
        pdf(file.path("output_methodI_0_95",paste(strsplit(i,".csv")[[1]],"_QQ.pdf",sep = "")))
        qqFitPlot(y, L_0_95)
        dev.off()
        
        pdf(file.path("output_methodI_0_95",paste(strsplit(i,".csv")[[1]],"_hist.pdf",sep = "")))
        hist(y, probability = T, breaks = 100, col = "grey", main = i)
        curve(dlnorm(x, meanlog = L_0_95$mu,  sdlog = L_0_95$sigma), col="darkblue", lwd=2, add = T)
        dev.off()
        
        #write data to a file
        df <- do.call(rbind, lapply(L_0_95, data.frame, stringsAsFactors=FALSE))
        write.csv(df, file.path("output_methodI_0_95",paste(strsplit(i,".csv")[[1]],"_statistics.csv",sep = "")), row.names = T)
        
        dir.create("output_methodI_0_75", showWarnings = FALSE)
        #generate plots
        pdf(file.path("output_methodI_0_75",paste(strsplit(i,".csv")[[1]],"_QQ.pdf",sep = "")))
        qqFitPlot(y, L_0_75)
        dev.off()
        
        pdf(file.path("output_methodI_0_75",paste(strsplit(i,".csv")[[1]],"_hist.pdf",sep = "")))
        hist(y, probability = T, breaks = 100, col = "grey", main = i)
        curve(dlnorm(x, meanlog = L_0_75$mu,  sdlog = L_0_75$sigma), col="darkblue", lwd=2, add = T)
        dev.off()
        
        #write data to a file
        df <- do.call(rbind, lapply(L_0_75, data.frame, stringsAsFactors=FALSE))
        write.csv(df, file.path("output_methodI_0_75",paste(strsplit(i,".csv")[[1]],"_statistics.csv",sep = "")),row.names = T)
}        
