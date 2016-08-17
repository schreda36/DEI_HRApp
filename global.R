suppressPackageStartupMessages( c(library(tm),library(shiny),library(stringr),
                                  library(data.table),library(NLP),library(ggplot2)) )

myData <- read.csv(file="./data/SampleData.csv")


#predict next word using ngram files
plotBasic <- function(textLength,textInput){
      
            
      g <- ggplot(NEI.baltimore.veh, aes(x=year, y=Emissions, group=1))+
            geom_point(color="blue", alpha=0.35)+
            labs(x="Year", y="Emissions (tons)", title="Baltimore Vehicle PM2.5 Emissions")+
            stat_summary(fun.y=sum, colour="red", geom="line", size = 1, show.legend = TRUE)+
            geom_point(aes(color="Total"))+
            geom_point(aes(color="Data"))+
            scale_colour_manual("", values=c("Data"="blue","Total"="red"))+
            guides(colour = guide_legend(override.aes = list(linetype=c(0,1)
                                                             , shape=c(16,NA))))
      #g <- qplot(year, Emissions, data=pm2.5.data, color=type, geom = "path",
       #            main="Baltimore PM2.5 Emissions Type (1999 to 2008)", xlab="Year", ylab="Total Emissions (thousands of tons)")
      
      #print g
      g
}

