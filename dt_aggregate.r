

dt_aggregate=function(d, by, by_all,...){

	library(data.table)

	my_args=substitute(.(...))
	
	my_dims=as.character(substitute(by))
	my_dims=my_dims[2:length(my_dims)]	
	
	my_dims_d=as.character(substitute(by_all))
	my_dims_d=my_dims_d[2:length(my_dims_d)]
	
	print(my_dims)
	print(my_dims_d)
	
	
	funct_comb=function(d,num){unlist(lapply(seq(nrow(d)),function(x){paste(as.character(d[x,num]),collapse="")}))}
		
	eg=lapply(1:(length(my_dims_d)),function(x){c(my_dims_d,"[blank]")})
	comb=expand.grid(eg,stringsAsFactors = FALSE)
	comb$x=funct_comb(comb,seq(length(my_dims_d)))	
	comb=comb[grepl("\\[blank\\]",comb$x),]	
	comb=as.data.frame(lapply(seq(length(my_dims_d)),function(x){dat=comb[,x];dat[dat %in% c("[blank]",my_dims_d[x])]}),stringsAsFactors=FALSE)
	names(comb)=my_dims_d
	comb=comb[!duplicated(funct_comb(comb,seq(length(my_dims_d)))),]
	
	if(class(comb)=="character"){comb=data.frame(my_dims_d=comb,stringsAsFactors=FALSE);names(comb)=my_dims_d}
	
	comb=rbind(my_dims_d,comb)
	comb=comb[rev(order(funct_comb(comb,seq(length(my_dims_d))))),,drop=FALSE]
	rownames(comb)=NULL
	
	non_part=as.data.frame(lapply(my_dims[!(my_dims %in% my_dims_d)],function(x){rep(x,nrow(comb))}),stringsAsFactors=FALSE)
	if(nrow(non_part)>0){
		names(non_part)=non_part[1,]
		comb=cbind(comb,non_part)
	}
	
	l=list()
	d=as.data.table(d)
	for(i in 1:nrow(comb)){
		dims_to_run=as.character(comb[i,])
		
		dims_to_run_i=dims_to_run[!is.na(match(names(comb),dims_to_run))]
		dims_to_run_e=names(comb)[is.na(match(names(comb),dims_to_run))]
		
		if(length(dims_to_run_i)>0){		
			dims_to_run_ev=paste0(".(",paste(dims_to_run_i,collapse=","),")")
			print(dims_to_run_ev)				
			eval(parse(text=paste0("dd=d[,eval(my_args),by=",dims_to_run_ev,"]")))
		}else{
			dd=d[,eval(my_args)]						
		}
				
		if(length(dims_to_run_e)>0){	
			all_df=as.data.frame(lapply(dims_to_run_e,function(x){rep(paste0("all ",x,"s"),nrow(dd))}),stringsAsFactors=FALSE)
			names(all_df)=dims_to_run_e
			dd=cbind.data.frame(dd,all_df)
		}
		
		l[[length(l)+1]]=dd			
	}
	
	
	rbindlist(l, use.names=TRUE)
	
		
}
