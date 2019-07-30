##This an edited version of the function described by 
##https://gist.github.com/johnDorian/5561272 (by Jason Lessels, jlessels@gmail.com). 
    
### This now consists of three functions. 
#   transform_piper_data: transforms the data to match 
#   ggplot_piper: plot the Piper diagram
#   toPercent: copied from the hydrogeo package
## The example currently utilises the function
#   conc2meq from the smwrGraphs package: this converts the mg/L to meq/L via mg/L * mol/g * valence = meq/L
### the coordinates of the piper diagram. ggplot_piper does all of the background.

##See also https://github.com/markolipka/ggplot_Piper
##Here we modify the function so as to plot as a ggplot or plotly for interactive viewing

transform_piper_data <- function(Mg, Ca, Cl,SO4, name=NULL){
  if(is.null(name)){
    name = rep(1:length(Mg),3)
  } else {
    name = rep(name,3)
  }
  y1 <- Mg * 0.86603
  x1 <- 100*(1-(Ca/100) - (Mg/200))
  y2 <- SO4 * 0.86603
  x2 <-120+(100*Cl/100 + 0.5 * 100*SO4/100)
  new_point <- function(x1, x2, y1, y2, grad=1.73206){
    b1 <- y1-(grad*x1)
    b2 <- y2-(-grad*x2)
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1], y=t_mat[2,1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation=name,x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y))
}


ggplot_piper <- function(piper.data,output = c("ggplot","plotly")) {
  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))
  
  p <- ggplot2::ggplot() +
    ## left hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=100, yend=0)) +
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=50, yend=86.603)) +
    ggplot2::geom_segment(ggplot2::aes(x=50,y=86.603, xend=100, yend=0)) +
    ## right hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=220, yend=0)) +
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=170, yend=86.603)) +
    ggplot2::geom_segment(ggplot2::aes(x=170,y=86.603, xend=220, yend=0)) +
    ## Upper diamond
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=60, yend=103.9236)) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=160, yend=103.9236)) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=160, yend=103.9236)) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=60, yend=103.9236)) +
    ## Add grid lines to the plots
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ### Labels and grid values
     
    ggplot2::geom_text(ggplot2::aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    #ggplot2::geom_text(ggplot2::aes(c(95,85,75,65),grid1p3$y2, label=c(80, 60, 40, 20)), size=3) +
    #geom_text(aes(17,50, label="Mg^'2+'"), parse=T, angle=60, size=4) +
    ggplot2::coord_equal(ratio=1) +  
    #ggplot2::geom_text(ggplot2::aes(c(155,145,135,125),grid2p2$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(ggplot2::aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    geom_text(ggplot2::aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()) +
    ggplot2::geom_point(ggplot2::aes(x,y, colour=factor(observation)), data=piper_data)
  
  
  if (output == "ggplot"){
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(17,50, label="Mg^'2+'"), angle=60, size=4, parse=TRUE) +  
      ggplot2::geom_text(ggplot2::aes(77.5,50, label="Na^'+'~+~K^'+'"), angle=-60, size=4,parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(50,-10, label="Ca^'2+'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(170,-10, label="Cl^'-'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(205,50, label="SO[4]^'-'"), angle=-60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(142,50, label="Alkalinity~as~{HCO[3]^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(72.5,150, label="SO[4]^'-'~+~Cl^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(147.5,150, label="Ca^'2+'~+~Mg^'2+'"), angle=-60, size=4, parse=TRUE)
  }
  
  if (output == "plotly"){
    #this fixes an issue that plotly can't render geom_text() with the  angle option set properly
    p <- plotly::ggplotly(p)
    p <- p  %>% plotly::layout(
      annotations=list(text=c("Mg<sup>2+</sup>",
                              "Na<sup>+</sup> + K<sup>+</sup>",
                              "Ca<sup>2+</sup>",
                              "Cl<sup>-</sup>",
                              "SO<sub>4</sub><sup>-</sup>",
                              "Alkalinity as HCO<sub>3</sub><sup>-</sup>",
                              "SO<sub>4</sub><sup>-2</sup> + Cl<sup>-</sup>",
                              "Ca<sup>2+</sup> + Mg<sup>2+</sup>"),
                       x = c(17,77.5,50,170,205,142.5,72.5,147.5),
                       y = c(50,50,-10,-10,50,50,150,150),
                       textangle = c(-60,60,0,0,60,-60,-60,60),
                       "showarrow"=F, font=list(size = 12, color = "black")
      ))
  }
  
  return(p)
}
toPercent <- function (d) {
  totalCations <- d$Ca + d$Mg + d$Na + d$K
  d$Ca <- 100 * (d$Ca/totalCations)
  d$Mg <- 100 * (d$Mg/totalCations)
  d$Na <- 100 * (d$Na/totalCations)
  d$K <- 100 * (d$K/totalCations)
  totalAnions <- d$Cl + d$SO4 + d$CO3 + d$HCO3
  d$Cl <- 100 * (d$Cl/totalAnions)
  d$SO4 <- 100 * (d$SO4/totalAnions)
  d$CO3 <- 100 * (d$CO3/totalAnions)
  d$HCO3 <- 100 * (d$HCO3/totalAnions)
  return(d)
}

#Read raw data
library(smwrGraphs)
path <- "."
WQFile <- "WQ_Database.csv"
WQdata <- read.csv(paste0(path,"\\",WQFile), stringsAsFactors = FALSE, header = TRUE)
#set date column format
datecols <- grep("Date",names(WQdata))
for (i in 1:length(datecols)) {
  WQdata[,datecols[i]] <- as.Date(unlist(c(WQdata[,datecols[i]])), format = "%d/%m/%Y")
}
#set values below pql to 0.5* pql
pqls <- as.numeric(WQdata[2,])
units <- WQdata[1,]
WQdata <- WQdata[-c(1,2,3),]
for (j in (datecols[length(datecols)]+1):dim(WQdata)[2]){
  pos.below.pql <- grep("<",WQdata[,j])
  if (length(pos.below.pql)>0){
    WQdata[pos.below.pql,j] <- paste(pqls[j]/2)
  }
  WQdata[,j] <- as.numeric(WQdata[,j])
}
# Transform the data from mg/L to meq/L
db <- transform(WQdata, Ca.meq = conc2meq(Calcium...Dissolved, "calcium"),
                Mg.meq = conc2meq(Magnesium...Dissolved, "magnesium"),
                Na.meq = conc2meq(Sodium...Dissolved, "sodium"),
                K.meq = conc2meq(Potassium...Dissolved, "potassium"),
                Cl.meq = conc2meq(Chloride, "chloride"),
                SO4.meq = conc2meq(Sulphate, "sulfate"),
                HCO3.meq = conc2meq(Bicarbonate.HCO3.as.CaCO3, "bicarb"),
                CO3.meq = conc2meq(Carbonate.CO32..as.CaCO3,"carbonate"))
db$SS <- WQdata$Sample
data=as.data.frame(list("Ca"=db$Ca.meq,"Mg"=db$Mg.meq,
                        "K" = db$K.meq,"Na" = db$Na.meq,
                        "Cl"=db$Cl.meq, "CO3" = db$CO3.meq,
                        "SO4"=db$SO4.meq, "HCO3" =db$HCO3.meq))
data <- toPercent(data)
data$observations <- db$Sample
#transform the data into piper based coordinates
piper_data <- transform_piper_data(Ca=data$Ca, Mg = data$Mg, Cl=data$Cl, SO4= data$SO4, name=data$observations)
ggplot_piper(piper_data,output = c("plotly"))
