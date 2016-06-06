# TODO: Add comment
# 
# Author: ecor
###############################################################################
#! /usr/bin/Rscript
rm(list=ls(all.names=TRUE))

set.seed(89)

datadir <-  '/home/ecor/Dropbox/R-packages/AnalyseGEOtop/data'


datafiles <- list.files(datadir,full.names=TRUE,pattern="RData")


for (it in datafiles) {
	print(it)
	vv <- ls(all.names=TRUE)
	rm(list=vv[!(vv %in% c("it","datadir","datafiles"))])
	
	load(it)
	vv <- ls(all.names=TRUE)
	print(vv[!(vv %in% c("it","datadir","datafiles","vv"))])
	
	rm(.Random.seed)
	
	vv <- ls(all.names=TRUE)
	aa <- (vv[!(vv %in% c("it","datadir","datafiles","vv"))])
	
	itn <- it 
	
	save(list=aa,file=itn,compression_level=9,compress=TRUE)
	print(aa)
	
}