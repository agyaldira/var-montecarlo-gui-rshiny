library(shiny)
library(shinythemes)
ui <- fluidPage(theme=shinytheme("sandstone"),
                titlePanel("Value at Risk (VaR) Stocks with Monte Carlo Simulation"),
                h4("by : Agy Aldira"),
                navbarPage("VaR Monte Carlo",
                           
                           tabPanel("Data",
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("data","Upload Closing Stock Price Data",accept = c("text",".txt"))
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "pills",id = "navbar",
                                                    tabPanel("Graph",
                                                             plotOutput("tsplot"),
                                                             value="Plot"),
                                                    tabPanel("Data",verbatimTextOutput("deskriptif"),verbatimTextOutput("data"),verbatimTextOutput("ret"),value="Data"),
                                                    tabPanel("Normality Test for Stocks Return",verbatimTextOutput("normalitas"),value="Uji Normalitas Return")
                                        )	)	)
                           ),
                           tabPanel("Monte Carlo Simulation",
                                    sidebarLayout(
                                      sidebarPanel(
                                        textInput("w","Initial Investment"),
                                        textInput("N","Generate Data"),
                                        actionButton("hitung1","Run",class="btn-primary")
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "pills",id = "navbar",
                                                    tabPanel("Monte Carlo Simulation Results",verbatimTextOutput("VAR"),verbatimTextOutput("interpretasi"),value="Hasil")
                                        ))))
                ))
VaR.MC=function(x,Wo=1,n=1000,alpha=c(0.01,0.025,0.05,0.1),k=1) 
{
  x=as.matrix(x)
  n.data=dim(x)[1]
  
  Mean.Rt=mean(x)
  Sigma.Rt=sd(x)
  return.sim=rnorm(n,Mean.Rt,Sigma.Rt)  
  Rbintang=quantile(return.sim,alpha) 
  VaR=Wo*Rbintang*sqrt(k)
  tampilan=as.matrix(t(VaR))
  colnames(tampilan)=paste(((1-alpha)*100),"%",sep="")
  rownames(tampilan)=""
  return(tampilan)
  
}

VaR_MC=function(x,m,Wo)
{
  VaR.MC=matrix(NA,m,4)
  for(i in 1:m)
  {
    VaR.MC[i,1]=VaR.MC(x,Wo)[1]
    VaR.MC[i,2]=VaR.MC(x,Wo)[2]
    VaR.MC[i,3]=VaR.MC(x,Wo)[3]
    VaR.MC[i,4]=VaR.MC(x,Wo)[4]
  }
  VaR.MC
  y=colMeans(VaR.MC)
  rata2=as.matrix(t(y))
  cat("=================================================================","\n") 
  cat("VaR value with Monte Carlo Simulation for confidence level: \n")
  cat("=================================================================","\n")
  cat("     99%     97.5%     95%     90%     \n")
  cat("=================================================================","\n")
  print(VaR.MC)
  colnames(rata2)=paste(((1-c(0.01,0.025,0.05,0.1))*100),"%",sep="")
  rownames(rata2)=""
  cat("=================================================================","\n")
  cat("Average VaR value with",m,"repetitions for confience level: \n")
  return(rata2)
}

server <- function(input, output) {
  output$tsplot<-renderPlot({
    data<-input$data
    if(is.null(data)){return()}
    file<-read.csv(data$datapath,header = T ,sep ='\t')
    x<-file[,1]
    ts.plot(x,main="Harga Penutupan",col="red",xlab="periode",ylab="data")
  })
  output$deskriptif<-renderPrint({
    data<-input$data
    if(is.null(data)){return()}
    file<-read.csv(data$datapath,header = T ,sep ='\t')
    x<-file[,1]
    cat("=======================\n")
    cat("Descriptive Statistics\n")
    cat("=======================\n")
    cat("mean=",mean(x),"\n")
    cat("variance=",var(x),"\n")
    cat("max.=",max(x),"\n")
    cat("min.=",min(x),"\n")
    cat("=======================\n")
  })
  output$data<-renderPrint({
    data<-input$data
    if(is.null(data)){return()}  
    file<-read.csv(data$datapath,header = T ,sep ='\t')
    x<-file[,1]
    saham=matrix(x,ncol=1)
    cat("=======================\n")
    cat("Clossing Price\n")
    cat("=======================\n")
    print(saham)
    cat("=======================\n")
    
  })
  output$ret<-renderPrint({
    data<-input$data
    if(is.null(data)){return()}
    file<-read.csv(data$datapath,header = T ,sep ='\t')
    x<-file[,1]
    n=length(x)
    ret_saham=rep(0,n-1)
    for(i in 2:n)
    {
      ret_saham[i]=log(x[i]/x[i-1])
    }
    retsaham=matrix(ret_saham,ncol=1)
    cat("=======================\n")
    cat("Return Data\n")
    cat("=======================\n")
    print(retsaham)
    cat("=======================\n")
  })
  output$normalitas<-renderPrint({
    data<-input$data
    if(is.null(data)){return()}
    file<-read.csv(data$datapath,header = T ,sep ='\t')
    x<-file[,1]
    n=length(x)
    ret_saham=rep(0,n-1)
    for(i in 2:n)
    {
      ret_saham[i]=log(x[i]/x[i-1])
    }
    retsaham=matrix(ret_saham,ncol=1)
    cat("=============================\n")
    cat("Normality Test for Return\n")
    cat("=============================\n")
    ks=ks.test(retsaham, "pnorm")
    print(ks)
    cat("=============================\n")
    cat("Hypothesis\n")
    cat("H0: returns are normally distributed\n")
    cat("H1: returns are not normally distributed\n")
    cat("Critical Value: Reject H0 if p-value < 0.05\n")
    cat("Statistical Test: p-value=",ks$p.value,"\n")
    if(ks$p.value<0.05){
      cat("Conclusion: returns are not normally distributed\n")}
    else{cat("Conclusion: returns are normally distributed\n")}
  })
  observeEvent(input$hitung1,{
    data<-input$data
    if(is.null(data)){return()}
    file<-read.csv(data$datapath,header = T ,sep ='\t')
    x<-file[,1]
    n=length(x)
    ret_saham=rep(0,n-1)
    for(i in 2:n)
    {
      ret_saham[i]=log(x[i]/x[i-1])
    }
    Wo<-as.numeric(input$w)
    m<-as.numeric(input$N)
    output$VAR<-renderPrint({VaR_MC(ret_saham,m,Wo)})
    output$interpretasi<-renderPrint({
      cat("The average VaR value above corresponds to the level of confidence and repetition that you input, 
showing the maximum loss in the next period\n")})
  })
}

#Running App
shinyApp(ui=ui,server=server)
