permfly <- function(fwfile, bwfile, N = 100, raw = FALSE) {
    #fwfile is the path to the CSV file for the treatment group e.g. "fw.csv"
    #bwfile is the path to the CSV file for the treatment group
    #N is the number of permutations for each Night/Day group

    #this function generates a "fly" data object. It works like the "workhorse"
    #function and is usually called by other functions that produce nicer output

    require(plyr) #note that this package is required for the code to run
                  #it can be installed by running
                  #install.packages("plyr")

    ###internal functions###

    #this is D -- as discussed in the meeting -- the permutation statistic of interest
    stat.proper <- function(df..) {
        absdiff <- function(x) {
            abs(mean(x[treat.group])-mean(x[cont.group]))
        }
        sum(apply(df..,1,absdiff))
    }

    #this is the permutation step
    calc.stat <- function(x,df.) {
        s <- empty.ind
        s <- sample(ind)
        stat.proper(df.[,s])
    }

    #this is used to calc the observed value of the statistic of interest
    calc.stat.noperm <- function(df) {
        df <- df[,-c(1,2,3)]
        stat.proper(df)
    }

    #this is used to duplicate the permutation
    calc.stat.df <- function(df) {
        df <- df[,-c(1,2,3)]
        out <- empty #preallocate
        out <- sapply(1:N,calc.stat,df)
        out
    }

    #calculates the pvalue
    prop.bigger <- function(i) {
        ob <- observed.test[i,3]
        pt <- ptest[i,-c(1,2)]
        binary.count <- (ob < pt)
        sum(binary.count)/length(binary.count)
    }

    empty <- rep(0,N)
    fw <- read.csv(file=fwfile,header=TRUE)
    bw <- read.csv(file=bwfile,header=TRUE)
    fw <- fw[,-c(1,2,3)]
    bw <- bw[,-c(1,2,3)]
    nt <- ncol(fw)-3 #number of treatment flies
    nc <- ncol(bw)-3 #number of control flies
    dat <- cbind(fw,bw[,-c(1,2,3)]) #combine the two datasets

    #define the subsets of interest for the permutation test
    ind <- 1:(nt+nc)
    empty.ind <- rep(0,length(ind))
    treat.group <- 1:nt
    cont.group <- nt+1:nc

    ptest <- ddply(dat,.(Day,Daytime.Nighttime),calc.stat.df) #conduct the perm test N times
    observed.test <- ddply(dat,.(Day,Daytime.Nighttime),calc.stat.noperm) #look at the true value of test stat

    test.ind <- 1:nrow(observed.test)
    pvalues <- sapply(test.ind,prop.bigger)
    no.cycles <- ddply(dat,.(Day,Daytime.Nighttime),nrow)[,3]

    output <- cbind(observed.test,pvalues,no.cycles)
    names(output) <- c("Day","Phase","D","P-value","No.Cycles")

    if (raw == FALSE) {
        return(output)
    } else {
        rawoutput=cbind(ptest,observed.test[,3])
        return(list(rawoutput=rawoutput,output=output))
    }
}

#foo as constructed here is an acceptable input to the gen.plot function
#foo=permfly("fw.csv","bw.csv",100,raw=TRUE)
#foo=foo$rawoutput

gen.plot = function(foo) {
    require(ggplot2)
    
    restruct=function(x) {
        dat=x[3:length(x)]
        norows=length(dat)
        first=rep(x[1],norows)
        second=rep(x[2],norows)
        third=c(rep("S",norows-1),"T")
        data.frame(day=first,daynight=second,sim=third,value=as.numeric(dat))
    }
    foo2=apply(foo,1,restruct)
    foo3=data.frame()
    for (i in 1:length(foo2)) {
        foo3=rbind(foo3,foo2[[i]])
    }
    foo3$day=factor(foo3$day)
    foo3$daynight=factor(foo3$daynight)
    foo3$sim=factor(foo3$sim)
    foo3$value=as.numeric(foo3$value)
    foo3=cbind(1:nrow(foo3),foo3)
    names(foo3)[1] <- "index"
    ggplot(foo3, aes(x=value)) + geom_density() + facet_grid(day~daynight)
    foo4=foo3[foo3$sim=="T",]
    ggplot(foo3, aes(x=value)) + geom_density() + geom_vline(aes(xintercept=value),data=foo4)+facet_grid(day~daynight)
}
