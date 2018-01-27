# custom string concatenation operator 
"+" = function(x,y) {
    if(is.character(x) || is.character(y)) {
        return(paste(x , y, sep=""))
    } else {
        .Primitive("+")(x,y)
    }
}

# generate a frequency table, similar to `table()`, reports both the count and %, also reports % of `NA`'s
tab=function(x){
	t1=as.data.frame(table(x))
	t1=t1[order(t1$Freq),]
	t1$perc=round((t1$Freq/length(x))*100,1)+"%"
	t2=as.data.frame(matrix(c("NA`s",length(x[is.na(x)]),round((length(x[is.na(x)])/length(x))*100,1)+"%"),nrow=1))
	names(t2)=names(t1)
	rownames(t1)=NULL
	res=list(t1,t2)
	names(res)=c("vals","na")	
	res
}
