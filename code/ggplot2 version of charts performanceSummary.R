

gg.charts.simple.chart <- function(rtn.obj, geometric = TRUE, main = "", plot = TRUE) {
   
   # load libraries
   suppressPackageStartupMessages(require(ggplot2))
   suppressPackageStartupMessages(require(scales))
   suppressPackageStartupMessages(require(reshape))
   suppressPackageStartupMessages(require(PerformanceAnalytics))
   suppressPackageStartupMessages(require(lubridate))
   suppressPackageStartupMessages(require(ggrepel))
   
   # create function to clean returns if having NAs in data
   clean_xts_returns <- function(xts_returns, na.replace=0) {
      
      xts_returns[is.na(xts_returns)]<- na.replace
      return(xts_returns)
   }
   
   # Create cumulative return function
   cum.rtn <- function(clean.xts.obj, g = TRUE)
   {
      x <- clean.xts.obj
      if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
      y
   }
   
   # Create function to calculate drawdowns
   dd.xts <- function(clean.xts.obj, g = TRUE)
   {
      x <- clean.xts.obj
      if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
      y
   }
   
   # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
   cps.df <- function(xts.obj,geometric)
   {
      x <- clean.rtn.xts(xts.obj)
      series.name <- colnames(xts.obj)[1]
      tmp <- cum.rtn(x,geometric)
      tmp$rtn <- x
      tmp$dd <- dd.xts(x,geometric)
      colnames(tmp) <- c("Index","Return","Drawdown") # names with space
      tmp.df <- as.data.frame(coredata(tmp))
      tmp.df$Date <- as.Date(index(tmp))
      tmp.df.long <- melt(tmp.df,id.var="Date")
      tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
      tmp.df.long
   }
   
   # A conditional statement altering the plot according to the number of assets
   if(ncol(rtn.obj)==1)
   {
      # using the cps.df function
      df <- cps.df(rtn.obj, geometric)
      # adding in a title string if need be
      if(main == ""){
         title.string <- paste("Asset Performance")
      } else {
         title.string <- main
      }
      
      gg.xts <- 
         ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
         facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
         geom_line(data = subset(df, variable == "Index"), aes_string(color = "variable")) +
         geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
         geom_line(data = subset(df, variable == "Drawdown")) +
         geom_hline(yintercept = 0, size = 0.5, colour = "gray") +
         ggtitle(title.string) +
         theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
         scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
         ylab("") +
         xlab("")
      
   } 
   else 
   {
      # a few extra bits to deal with the added rtn columns
      no.of.assets <- ncol(rtn.obj)
      asset.names <- colnames(rtn.obj)
      df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
      df$asset <- ordered(df$asset, levels=asset.names)
      if(main == ""){
         title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
      } else {
         title.string <- main
      }
      
      if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
      
      
      gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
         
         # panel layout
         facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                       , labeller = label_value, switch = "y") + # label_value is default
         
         # display points for Index and Drawdown, but not for Return
         # geom_point(data = subset(df, variable == c("Index","Drawdown"))
         #            , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) + 
         
         # manually select shape of geom_point
         # scale_shape_manual(values = c(1,2,3)) + 
         
         # line colours for the Index
         geom_line(data = subset(df, variable == "Index"), 
                   aes(colour = factor(asset)), show.legend = TRUE) +
         
         # bar colours for the Return
         # geom_bar(data = subset(df, variable == "Return"), stat = "identity"
         #          , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
         
         # line colours for the Drawdown
         geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
         
         # horizontal line to indicate zero values
         geom_hline(yintercept = 0, size = 0.5, colour = "white") +
         
         # color set
         scale_color_brewer(palette = "Set1") +
         
         # horizontal ticks
         scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b %Y")) + # , expand = expand_scale(mult = c(0, .2))) + #date_format("%b %Y")) +
         # main y-axis title
         ylab("") +
         # main x-axis title
         xlab("") +
         # main chart title
         ggtitle(title.string)
      
      
      # legend 
      gglegend <- guide_legend(override.aes = list(size = 2))
      
      gg.xts <- gg.xts + guides(colour = gglegend, size = "none") #+
         
         # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
         # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
         
         # theme( legend.title = element_blank(),
         #        legend.position = "right", # c(0,1)
         #        legend.justification = c(0,1),
         #        # , legend.background = element_rect(color = "#272b30"),
         #        legend.text = element_text(color = "white"),
         #        legend.key = element_rect(fill = "#272b30", colour = "#272b30"),
         #        legend.background = element_rect(fill = "#272b30"),
         #        axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.5, color = "white", size = 12)
         #        , strip.background = element_rect(fill = "#272b30", color = NA),
         #        strip.text.y = element_text(color = "white"),
         #        
         #        # , panel.background = element_rect(fill = "white", colour = "white")
         #        # , panel.grid.major = element_line(colour = "grey", size = 0.5) 
         #        # , panel.grid.minor = element_line(colour = NA, size = 0.0),
         #        
         #        axis.text.y = element_text(color = "white", size = 12),
         #        axis.ticks = element_blank(),
         #        panel.background = element_rect(fill = "#272b30", color = NA),
         #        panel.border = element_rect(fill = NA, color = "#272b30"),
         #        panel.grid.major = element_blank(),
         #        panel.grid.minor = element_blank(),
         #        plot.background = element_rect(color = "#272b30", fill = "#272b30"),
         #        plot.title = element_text(color = "white"),
         #        plot.subtitle = element_text(color = "white") 
         #        
         #        
         # )
      
   }
   
   assign("gg.xts", gg.xts,envir=.GlobalEnv)
   if(plot == TRUE){
      plot(gg.xts)
   } else {}
   
}














# 
# library(xts)
# set.seed(88)
# X.stock.rtns <- xts(rnorm(1000,0.00001,0.0003), Sys.Date()-(1000:1))
# Y.stock.rtns <- xts(rnorm(1000,0.00003,0.0004), Sys.Date()-(1000:1))
# Z.stock.rtns <- xts(rnorm(1000,0.00005,0.0005), Sys.Date()-(1000:1))
# rtn.obj <- merge(X.stock.rtns , Y.stock.rtns, Z.stock.rtns)
# colnames(rtn.obj) <- c("AA","BB","XX")


gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", plot = TRUE) {
      
      # load libraries
      suppressPackageStartupMessages(require(ggplot2))
      suppressPackageStartupMessages(require(scales))
      suppressPackageStartupMessages(require(reshape))
      suppressPackageStartupMessages(require(PerformanceAnalytics))
      suppressPackageStartupMessages(require(lubridate))
      suppressPackageStartupMessages(require(ggrepel))
      
      # create function to clean returns if having NAs in data
      clean_xts_returns <- function(xts_returns, na.replace=0) {
            
         xts_returns[is.na(xts_returns)]<- na.replace
         return(xts_returns)
      }
      
      # Create cumulative return function
      cum.rtn <- function(clean.xts.obj, g = TRUE)
      {
            x <- clean.xts.obj
            if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
            y
      }
      
      # Create function to calculate drawdowns
      dd.xts <- function(clean.xts.obj, g = TRUE)
      {
            x <- clean.xts.obj
            if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
            y
      }
      
      # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
      cps.df <- function(xts.obj,geometric)
      {
            x <- clean.rtn.xts(xts.obj)
            series.name <- colnames(xts.obj)[1]
            tmp <- cum.rtn(x,geometric)
            tmp$rtn <- x
            tmp$dd <- dd.xts(x,geometric)
            colnames(tmp) <- c("Index","Return","Drawdown") # names with space
            tmp.df <- as.data.frame(coredata(tmp))
            tmp.df$Date <- as.Date(index(tmp))
            tmp.df.long <- melt(tmp.df,id.var="Date")
            tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
            tmp.df.long
      }
      
      # A conditional statement altering the plot according to the number of assets
      if(ncol(rtn.obj)==1)
      {
            # using the cps.df function
            df <- cps.df(rtn.obj, geometric)
            # adding in a title string if need be
            if(main == ""){
                  title.string <- paste("Asset Performance")
            } else {
                  title.string <- main
            }
            
            gg.xts <- 
               ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
               facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
               geom_line(data = subset(df, variable == "Index"), aes_string(color = "variable")) +
               geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
               geom_line(data = subset(df, variable == "Drawdown")) +
               geom_hline(yintercept = 0, size = 0.5, colour = "gray") +
               ggtitle(title.string) +
               theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
               scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
               ylab("") +
               xlab("")
            
      } 
      else 
      {
            # a few extra bits to deal with the added rtn columns
            no.of.assets <- ncol(rtn.obj)
            asset.names <- colnames(rtn.obj)
            df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
            df$asset <- ordered(df$asset, levels=asset.names)
            if(main == ""){
                  title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
            } else {
                  title.string <- main
            }
            
            if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
            
            
            gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
                  
                  # panel layout
                  facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                                   , labeller = label_value, switch = "y") + # label_value is default
                  
                  # display points for Index and Drawdown, but not for Return
                  # geom_point(data = subset(df, variable == c("Index","Drawdown"))
                  #            , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) + 
                  
                  # manually select shape of geom_point
                  # scale_shape_manual(values = c(1,2,3)) + 
                  
                  # line colours for the Index
                  geom_line(data = subset(df, variable == "Index"), 
                            aes(colour = factor(asset)), show.legend = TRUE) +
                  
                  # bar colours for the Return
                  geom_bar(data = subset(df, variable == "Return"), stat = "identity"
                           , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
                  
                  # line colours for the Drawdown
                  geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
                  
                  # horizontal line to indicate zero values
                  geom_hline(yintercept = 0, size = 0.5, colour = "white") +
                  
                  # color set
                  scale_color_brewer(palette = "Set1") +
                  
                  # horizontal ticks
                  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b %Y")) + # , expand = expand_scale(mult = c(0, .2))) + #date_format("%b %Y")) +
                  # main y-axis title
                  ylab("") +
                  # main x-axis title
                  xlab("") +
                  # main chart title
                  ggtitle(title.string)
            
            
            # legend 
            gglegend <- guide_legend(override.aes = list(size = 3))
            
            gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
                  
                  # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
                  # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
                  
                  theme( legend.title = element_blank(),
                         legend.position = "right", # c(0,1)
                         legend.justification = c(0,1),
                         # , legend.background = element_rect(color = "#272b30"),
                         legend.text = element_text(color = "white"),
                         legend.key = element_rect(fill = "#272b30", colour = "#272b30"),
                         legend.background = element_rect(fill = "#272b30"),
                         axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.5, color = "white", size = 12)
                         , strip.background = element_rect(fill = "#272b30", color = NA),
                         strip.text.y = element_text(color = "white"),
                         
                         # , panel.background = element_rect(fill = "white", colour = "white")
                         # , panel.grid.major = element_line(colour = "grey", size = 0.5) 
                         # , panel.grid.minor = element_line(colour = NA, size = 0.0),

                         axis.text.y = element_text(color = "white", size = 12),
                         axis.ticks = element_blank(),
                         panel.background = element_rect(fill = "#272b30", color = NA),
                         panel.border = element_rect(fill = NA, color = "#272b30"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         plot.background = element_rect(color = "#272b30", fill = "#272b30"),
                         plot.title = element_text(color = "white"),
                         plot.subtitle = element_text(color = "white") 

                         
                         )
            
      }
      
      assign("gg.xts", gg.xts,envir=.GlobalEnv)
      if(plot == TRUE){
            plot(gg.xts)
      } else {}
      
}

# display chart
# gg.charts.PerformanceSummary(rtn.obj, geometric = TRUE)



