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

# performs a left join between 2 data frames on one or more variables. It also provides options for situations where no match was found
index_match=function(samp,d_samp,o_id_cols_array=c("ag","wkn"),l_id_cols_array=c("ag","wkn"), ret_array=c("weekly_sched_hrs"),ifna=NA) {    
        samp=as.data.frame(samp)
        d_samp=as.data.frame(d_samp)

        if(length(o_id_cols_array)>1 | length(l_id_cols_array)>1){
                res=d_samp[,ret_array][match(do.call(paste0, samp[,o_id_cols_array]), do.call(paste0, d_samp[,l_id_cols_array]))]
        }else{
                res=d_samp[,ret_array][match(samp[,o_id_cols_array], d_samp[,l_id_cols_array])] 
        }

        if(is.na(ifna)){return(res)}    # if ifna is NA: return NA
        if(ifna=="original"){               
            res[is.na(res)]=samp[is.na(res),o_id_cols_array] # if ifna=="original": return original value
        }else{
            res[is.na(res)]=ifna    # else: return value supplied in the `ifna` arg
        }   

        return(res)

}

# trim white space
trim=function (x) gsub("^\\s+|\\s+$", "", x)
