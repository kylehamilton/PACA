library(shiny)
library(shinyAce)
library(psych)



shinyServer(function(input, output) {
    
  q <- observe({
    # Stop the app when the quit button is clicked
    if (input$quit == 1) stopApp()
  })
    
    bs <- reactive({
        if (input$rowname == 1) {
            x <- read.csv(text=input$text, sep="\t")
            x <- x[, -1]
        }else{
            x <- read.csv(text=input$text, sep="\t")
        }
        describe(x)[2:13]
    })
    
    
    
    correl <- reactive({
        if (input$rowname == 1) {
            x <- read.csv(text=input$text, sep="\t")
            x <- x[, -1]
        }else{
            x <- read.csv(text=input$text, sep="\t")
        }
        round(cor(cbind(x), use = "complete"),3)
    })
    
    
    
    makecorPlot <- function(){
        if (input$rowname == 1) {
            x <- read.csv(text=input$text, sep="\t")
            x <- x[, -1]
        }else{
            x <- read.csv(text=input$text, sep="\t")
        }
        pairs.panels(x)
    }
    
    output$corPlot <- renderPlot({
        print(makecorPlot())
    })


    
    makesPlot <- function(){
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            dat <- dat[, -1]
        }else{
            dat <- read.csv(text=input$text, sep="\t")
        }
        VSS.scree(dat, main = "")
    }
    
    output$sPlot <- renderPlot({
        print(makesPlot())
    })
    
    
    
    
    
    
    pcaresult <- reactive({
        options(digits=3)
        
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)

            info <- summary(respca)
            eigen <- info[[1]]^2
            newinfo <- rbind("Eigen values"=eigen, info$importance)
            cat("Importance of components:", "\n")
            print(newinfo)
            
            pcloadings <- t(respca$sdev*t(respca$rotation))
            cat("\n", "Principal component loadings:", "\n")
            print(pcloadings)
            
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))
            cat("\n", "Principal component scores:", "\n")
            print(pcscores)
        
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            
            info <- summary(respca)
            eigen <- info[[1]]^2
            newinfo <- rbind("Eigen values"=eigen, info$importance)
            cat("Importance of components:", "\n")
            print(newinfo)
            
            pcloadings <- t(respca$sdev*t(respca$rotation))
            cat("\n", "Principal component loadings:", "\n")
            print(pcloadings)
            
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))
            cat("\n", "Principal component scores:", "\n")
            print(pcscores)
        }
    })
    
    
    
    makePlot1 <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            datpca <- dat[,-1]
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }
        
        par(mfrow = c(2, 1))
        barplot(pcloadings[,1], main="PC 1", ylim=c(-1,1), cex.names=0.7)
        barplot(pcloadings[,2], main="PC 2", ylim=c(-1,1), cex.names=0.7)
    }



    output$pcPlot1 <- renderPlot({
        print(makePlot1())
    })
    
    
    
    makePlot2 <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))
        }


        plot(pcloadings[,1:2], type="n", xlab="PC 1", ylab="PC 2", cex.axis=0.8, cex.lab=0.8)
        text(pcloadings[,1:2], labels=colvar, cex=0.9, adj=c(0.25,1.5))
        abline(h=0,lty="dotted")
        abline(v=0,lty="dotted")
        title(main="Principal Component Analysis: PC Loadings")
    }
    
    
    output$pcPlot2 <- renderPlot({
        print(makePlot2())
    })
    
    
    
    makePlot3 <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }
        
        
        PCSpc1min <- min(pcscores[,1:2][,1])
        PCSpc1min <- PCSpc1min-(abs(PCSpc1min-PCSpc1min*1.25))
        PCSpc1max <- max(pcscores[,1:2][,1])
        PCSpc1max <- PCSpc1max*1.25
        
        PCSpc2min <- min(pcscores[,1:2][,2])
        PCSpc2min <- PCSpc2min-(abs(PCSpc2min-PCSpc2min*1.25))
        PCSpc2max <- max(pcscores[,1:2][,2])
        PCSpc2max <- PCSpc2max*1.25
        
        plot(pcscores[,1:2], xlab="PC 1", ylab="PC 2", type="n", xlim=c(PCSpc1min, PCSpc1max), ylim=c(PCSpc2min, PCSpc2max), cex.axis=0.8,cex.lab=0.8)
        
        text(pcscores[,1:2], labels=rowvar, cex=0.9, adj=c(0.25,1.5))
        abline(h=0,lty="dotted")
        abline(v=0,lty="dotted")
        title(main="Principal Component Analysis: PC Scores")
    }
    
    
    output$pcPlot3 <- renderPlot({
        print(makePlot3())
    })
    
    
    
    makePlot4 <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }
        
        
        pc1min <- min(c(pcloadings[,1:2][,1], pcscores[,1:2][,1]))
        pc1min <- pc1min*1.25
        pc1max <- max(c(pcloadings[,1:2][,1], pcscores[,1:2][,1]))
        pc1max <- pc1max*1.25
        
        pc2min <- min(c(pcloadings[,1:2][,2], pcscores[,1:2][,2]))
        pc2min <- pc2min*1.25
        pc2max <- max(c(pcloadings[,1:2][,2], pcscores[,1:2][,2]))
        pc2max <- pc2max*1.25
        
        
        biplot(pcscores[,1:2], pcloadings[,1:2], var.axes = F, xlim=c(pc1min, pc1max), ylim=c(pc2min, pc2max))
        abline(v=0, lty=3)
        abline(h=0, lty=3) 
    }
    
    
    output$pcPlot4 <- renderPlot({
        print(makePlot4())
    })
    
    
    
    makePlot5 <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }
        
        dat$PCA1 <- pcscores[,1]
        dat$PCA2 <- pcscores[,2]
        
        z <- dat[, c("PCA1","PCA2")]
        z.d <- dist(z)^2
        result <- hclust(z.d, method=input$clusteroptions)
        par(mar=c(1,6,3,1))
        plot(result, xlab="", sub="")
    }
    
    
    output$pcPlot5 <- renderPlot({
        print(makePlot5())
    })
    
    
    
    check <- reactive({
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datpca <- as.matrix(dat)
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))

        }
        
        dat$PCA1 <- pcscores[,1]
        dat$PCA2 <- pcscores[,2]
        dat$PCA3 <- pcscores[,3]
        dat
    })


    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })
    

    output$textarea.out <- renderPrint({
        bs()
    })
  
    output$clusteroptions.out <- renderPrint({paste("Selected method is:", input$clusteroptions )})

   output$clusteroptions1.out <- renderPrint({paste(input$clusteroptions , "method with the squared Euclidean distance technique")})
  
    output$correl.out <- renderPrint({
        correl()
    })
    
    output$pcaresult.out <- renderPrint({
        pcaresult()
    })
    
    output$downloadPlot1 <- downloadHandler(
    filename = function() {
        paste('Plot1-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot1())
		dev.off()
	}
    )
    
    output$downloadPlot2 <- downloadHandler(
    filename = function() {
        paste('Plot2-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot2())
		dev.off()
	}
    )
    
    output$downloadPlot3 <- downloadHandler(
    filename = function() {
        paste('Plot3-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot3())
		dev.off()
	}
    )
    
    output$downloadPlot4 <- downloadHandler(
    filename = function() {
        paste('Plot4-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot4())
		dev.off()
	}
    )
    
    output$downloadPlot5 <- downloadHandler(
    filename = function() {
        paste('Plot5-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot5())
		dev.off()
	}
    )

    output$downloadCorPlot <- downloadHandler(
    filename = function() {
        paste('Corplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makecorPlot())
		dev.off()
	}
    )
    
    output$downloadSPlot <- downloadHandler(
    filename = function() {
        paste('ScreePlot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
        print(makesPlot())
		dev.off()
	}
    )
    
    output$downloadData <- downloadHandler(
    filename = function() {
        paste('PCA-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
        write.csv(check(), file)
    }
    )




})
