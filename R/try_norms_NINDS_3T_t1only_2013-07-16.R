#####################################################################################################################
#####################################################################################################################
# This file considers the derivatives of the histograms.
# July 16, 2013
#####################################################################################################################
############################################################################################

library(AnalyzeFMRI)
library(RColorBrewer)
library(R.utils)
library(SemiPar)
cols=brewer.pal(8,'Dark2')

#data.path<-'/dexter/disk1/smart/msmri/NINDS_3T/NINDS_3T/'
data.path<-'/project/taki/msmri/NINDS_3T/NINDS_3T/'
curr.path<-data.path
scan.ids<-unique(substr(list.dirs(data.path),40,99))
scan.ids<-scan.ids[nchar(scan.ids)>5]
scan.ids<-scan.ids[1:150]



# setwd('~/NIRVANA')
epsilon<-1E-8
whitestripe.q.width<-0.05 #radius of white stripe
whitestripe.t1only.q.width<-0.05 #radius of white stripe

 
#####################################################################################################################
#####################################################################################################################




#scratch.location<-'/scratch/temp/rshinoha' #HOPKINS CLUSTER
#scratch.location<-'/project/taki2/scratch' #PENN CLUSTER
# scratch.location<-'/scratch/rshi' #PENN CLUSTER

##This function unzips and reads a .nii.gz file FROM A NODE.
readniigz <- function(file){
	if (!file.exists(scratch.location)) dir.create(scratch.location)
	name <- tempfile(pattern = "tempfile", tmpdir = scratch.location, fileext = ".nii")
	file.copy(file,paste(name,'.gz',sep=''),overwrite=TRUE)
	gunzip(paste(name,'.gz',sep=''),name,overwrite=TRUE)
	image <- f.read.nifti.volume(name)
	try(unlink(name))
	return(image)
}


##This function writes and zips a .nii.gz file FROM A NODE.
writeniigz <- function(image,file){
	if (!file.exists(scratch.location)) dir.create(scratch.location)
	name <- tempfile(pattern = "tempfile", tmpdir = scratch.location, fileext = ".nii")
	f.write.nifti(image,name,size="float",nii=TRUE)
	gzip(name, paste(name,'.gz',sep=''), remove = TRUE,overwrite=TRUE)
	try.save<-file.copy(paste(name,'.gz',sep=''),file,overwrite=TRUE)
	try(unlink(paste(name,'.gz',sep='')))
}


#####################################################################################################################
#####################################################################################################################
# Normalize the data
#####################################################################################################################
#####################################################################################################################


d.t1<-list();d.t1.bet.norm<-list();d.t1.brain.norm<-list();d.t1.new.norm<-list();d.t1.whitestripe.norm<-list();d.t1.wholeimage.norm<-list();d.t1.whitestripe.t1only.norm<-list()
d.t2<-list();d.t2.bet.norm<-list();d.t2.brain.norm<-list();d.t2.new.norm<-list();d.t2.whitestripe.norm<-list();d.t2.wholeimage.norm<-list();d.t2.whitestripe.t1only.norm<-list()
d.flair<-list();d.flair.bet.norm<-list();d.flair.brain.norm<-list();d.flair.new.norm<-list();d.flair.whitestripe.norm<-list();d.flair.wholeimage.norm<-list();d.flair.whitestripe.t1only.norm<-list()
d.pd<-list();d.pd.bet.norm<-list();d.pd.brain.norm<-list();d.pd.new.norm<-list();d.pd.whitestripe.norm<-list();d.pd.wholeimage.norm<-list();d.pd.whitestripe.t1only.norm<-list()

whitestripe.t1<-list();whitestripe.t2<-list();whitestripe.flair<-list();whitestripe.pd<-list();
# 
# for (scan.i in 1:length(scan.ids)) {
# #for (scan.i in 21:length(scan.ids)) {
# 	scan.id<-scan.ids[scan.i]
# 	
# 	#####################################################################################################################
# 	#Read in the data
# 	#####################################################################################################################
# 
# 	#Registered MRI data
# 	system.time(t1 <- readniigz(paste(curr.path,scan.id,'/VolumetricT1.nii.gz',sep='')))
# 	system.time(t2 <- readniigz(paste(curr.path,scan.id,'/T2.nii.gz',sep='')))
# 	system.time(flair <- readniigz(paste(curr.path,scan.id,'/FLAIR.nii.gz',sep='')))
# 	system.time(pd <- readniigz(paste(curr.path,scan.id,'/PD.nii.gz',sep='')))
# 
# 	###########################
# 	#Normalize the t2 data
# 
# 	#Use standard normalization
# 	t2.wholeimage.norm<-(t2-mean(t2))/sd(t2)
# 
# 	t2.voi<-t2[,,80:120,]
# 	try(t2.hist<-hist(t2.voi[t2.voi>mean(t2)],breaks=2000,plot=FALSE))
# 	y.in<-t2.hist$counts
# 	x.in<-t2.hist$mids
# 	x.in<-x.in[!is.na(y.in)];y.in<-y.in[!is.na(y.in)]
# 	t2.last.mode<-get.largest.mode(x.in,y.in,t2.voi)
# 	t2.last.mode.q<-mean(t2.voi[t2.voi>mean(t2)]<t2.last.mode)
# 	whitestripe<-quantile(t2.voi[t2.voi>mean(t2)],probs=c(t2.last.mode.q-whitestripe.q.width,t2.last.mode.q+whitestripe.q.width))
# 	whitestripe.ind<-which((t2>(whitestripe[1]))&(t2<(whitestripe[2])))
# 	if (length(whitestripe.ind)==0) whitestripe.ind<-1:prod(dim(t1)) #ERROR IN WHITE STRIPE MEANS DO WHOLE-IMAGE NORMALIZATION
# 
# 	#Save mask for whitestripe
# 	whitestripe.mask<-array(0,dim=dim(t1))
# 	whitestripe.mask[whitestripe.ind]<-1
# 	writeniigz(whitestripe.mask,paste(curr.path,scan.id,'/t2_whitestripe_t2.nii.gz',sep=''))
# 
# 	#Use new white stripe normalization
# 	mu.new<-mean(t2[whitestripe.ind])
# 	sig.whitestripe<-sd(t2[whitestripe.ind])
# 	t2.whitestripe.norm<-(t2-mu.new)/sig.whitestripe
# 
# 	#Save normalized images
# 	writeniigz(t2.wholeimage.norm,paste(curr.path,scan.id,'/t2_norm_wholeimage.nii.gz',sep=''))
# 	writeniigz(t2.whitestripe.norm,paste(curr.path,scan.id,'/t2_whitestripe_new_t2.nii.gz',sep=''))
# 
# 	#Find densities
# 	d.t2[[scan.i]]<-density(t2)
# 	d.t2.wholeimage.norm[[scan.i]]<-density(t2.wholeimage.norm)
# 	d.t2.whitestripe.norm[[scan.i]]<-density(t2.whitestripe.norm)
# 	#Normalize the t1 data
# 
# 	#Use standard normalization
# 	t1.wholeimage.norm<-(t1-mean(t1))/sd(t1)
# 
# 	#Use new white stripe normalization
# 	mu.new<-mean(t1[whitestripe.ind])
# 	sig.whitestripe<-sd(t1[whitestripe.ind])
# 	t1.whitestripe.norm<-(t1-mu.new)/sig.whitestripe
# 	whitestripe.t1[[scan.i]]<-quantile(t1[whitestripe.ind],probs=c(0.1,0.9))
# 	
# 	#Save normalized images
# 	writeniigz(t1.wholeimage.norm,paste(curr.path,scan.id,'/t1_norm_wholeimage.nii.gz',sep=''))
# 	writeniigz(t1.whitestripe.norm,paste(curr.path,scan.id,'/t1_whitestripe_new_t2.nii.gz',sep=''))
# 
# 	#Find densities
# 	d.t1[[scan.i]]<-density(t1)
# 	d.t1.wholeimage.norm[[scan.i]]<-density(t1.wholeimage.norm)
# 	d.t1.whitestripe.norm[[scan.i]]<-density(t1.whitestripe.norm)
# 	
# 	###########################
# 	#Normalize the pd data
# 
# 	#Use standard normalization
# 	pd.wholeimage.norm<-(pd-mean(pd))/sd(pd)
# 	
# 	#Use new white stripe normalization
# 	mu.new<-mean(pd[whitestripe.ind])
# 	sig.whitestripe<-sd(pd[whitestripe.ind])
# 	pd.whitestripe.norm<-(pd-mu.new)/sig.whitestripe
# 	whitestripe.pd[[scan.i]]<-quantile(pd[whitestripe.ind],probs=c(0.1,0.9))
# 
# 	#Save normalized images
# 	writeniigz(pd.wholeimage.norm,paste(curr.path,scan.id,'/pd_norm_wholeimage.nii.gz',sep=''))
# 	writeniigz(pd.whitestripe.norm,paste(curr.path,scan.id,'/pd_whitestripe_new_t2.nii.gz',sep=''))
# 
# 	#Find densities
# 	d.pd[[scan.i]]<-density(pd)
# 	d.pd.wholeimage.norm[[scan.i]]<-density(pd.wholeimage.norm)
# 	d.pd.whitestripe.norm[[scan.i]]<-density(pd.whitestripe.norm)
# 
# 	###########################
# 	#Normalize the flair data
# 
# 	#Use standard normalization
# 	flair.wholeimage.norm<-(flair-mean(flair))/sd(flair)
# 	
# 	#Use new white stripe normalization
# 	mu.new<-mean(flair[whitestripe.ind])
# 	sig.whitestripe<-sd(flair[whitestripe.ind])
# 	flair.whitestripe.norm<-(flair-mu.new)/sig.whitestripe
# 	whitestripe.flair[[scan.i]]<-quantile(flair[whitestripe.ind],probs=c(0.1,0.9))
# 
# 	#Save normalized images
# 	writeniigz(flair.wholeimage.norm,paste(curr.path,scan.id,'/flair_norm_wholeimage.nii.gz',sep=''))
# 	writeniigz(flair.whitestripe.norm,paste(curr.path,scan.id,'/flair_whitestripe_new_t2.nii.gz',sep=''))
# 
# 	#Find densities
# 	d.flair[[scan.i]]<-density(flair)
# 	d.flair.wholeimage.norm[[scan.i]]<-density(flair.wholeimage.norm)
# 	d.flair.whitestripe.norm[[scan.i]]<-density(flair.whitestripe.norm)
# 	
# 
# ###########################
# ###########################	
# #Use new white stripe normalization - T1 only
# 
# ### DEFINE THE WHITE STRIPE ON NON-BG VOXELS.
# 	#t1.voi<-t1[52:156,,,]
# 	#t1.voi<-t1[,,52:156,]
# 	t1.voi<-t1[,,80:120,]
# 	t1.hist<-hist(t1.voi[t1.voi>mean(t1)],breaks=2000,plot=FALSE)
# 	y.in<-t1.hist$counts
# 	x.in<-t1.hist$mids
# 	x.in<-x.in[!is.na(y.in)];y.in<-y.in[!is.na(y.in)]
# 	t1.last.mode<-get.last.mode(x.in,y.in,t1.voi)
# 	t1.last.mode.q<-mean(t1.voi[t1.voi>mean(t1)]<t1.last.mode)
# 	whitestripe.t1only<-quantile(t1.voi[t1.voi>mean(t1)],probs=c(max(t1.last.mode.q-whitestripe.t1only.q.width,0),min(t1.last.mode.q+whitestripe.t1only.q.width,1)))
# 	whitestripe.t1only.ind<-which((t1>(whitestripe.t1only[1]))&(t1<(whitestripe.t1only[2])))
# 	mu.whitestripe.t1only<-t1.last.mode
# 	sig.whitestripe.t1only<-sd(t1[whitestripe.t1only.ind])
# 	t1.whitestripe.t1only.norm<-(t1-mu.whitestripe.t1only)/sig.whitestripe.t1only
# 	
# 	mu.whitestripe.t1only<-mean(t2[whitestripe.t1only.ind])
# 	sig.whitestripe.t1only<-sd(t2[whitestripe.t1only.ind])
# 	t2.whitestripe.t1only.norm<-(t2-mu.whitestripe.t1only)/sig.whitestripe.t1only
# 	
# 	mu.whitestripe.t1only<-mean(flair[whitestripe.t1only.ind])
# 	sig.whitestripe.t1only<-sd(flair[whitestripe.t1only.ind])
# 	flair.whitestripe.t1only.norm<-(flair-mu.whitestripe.t1only)/sig.whitestripe.t1only
# 	
# 	mu.whitestripe.t1only<-mean(pd[whitestripe.t1only.ind])
# 	sig.whitestripe.t1only<-sd(pd[whitestripe.t1only.ind])
# 	pd.whitestripe.t1only.norm<-(pd-mu.whitestripe.t1only)/sig.whitestripe.t1only
# 	
# 	d.t1.whitestripe.t1only.norm[[scan.i]]<-density(t1.whitestripe.t1only.norm)
# 	d.t2.whitestripe.t1only.norm[[scan.i]]<-density(t2.whitestripe.t1only.norm)
# 	d.flair.whitestripe.t1only.norm[[scan.i]]<-density(flair.whitestripe.t1only.norm)
# 	d.pd.whitestripe.t1only.norm[[scan.i]]<-density(pd.whitestripe.t1only.norm)
# 
# 	#Save mask for whitestripe
# 	whitestripe.t1only.mask<-array(0,dim=dim(t1))
# 	whitestripe.t1only.mask[whitestripe.t1only.ind]<-1
# 	writeniigz(whitestripe.t1only.mask,paste(curr.path,scan.id,'/whitestripet1only_new.nii.gz',sep=''))
# 
# 	writeniigz(t1.whitestripe.t1only.norm,paste(curr.path,scan.id,'/t1_whitestripet1only_new.nii.gz',sep=''))
# 	writeniigz(t2.whitestripe.t1only.norm,paste(curr.path,scan.id,'/t2_whitestripet1only_new.nii.gz',sep=''))
# 	writeniigz(flair.whitestripe.t1only.norm,paste(curr.path,scan.id,'/flair_whitestripet1only_new.nii.gz',sep=''))
# 	writeniigz(pd.whitestripe.t1only.norm,paste(curr.path,scan.id,'/pd_whitestripet1only_new.nii.gz',sep=''))
# 
# 	#Use COMBINED white stripe normalization
# 	#Save mask for whitestripe
# 	whitestripe.combined.ind<-intersect(whitestripe.ind,whitestripe.t1only.ind)
# 	whitestripe.combined.mask<-array(0,dim=dim(t1))
# 	whitestripe.combined.mask[whitestripe.combined.ind]<-1
# 	writeniigz(whitestripe.combined.mask,paste(curr.path,scan.id,'/whitestripe_combined_t2.nii.gz',sep=''))
# 
# 	mu.new.combined<-mean(t1[whitestripe.combined.ind])
# 	sig.whitestripe.combined<-sd(t1[whitestripe.combined.ind])
# 	t1.whitestripe.combined.norm<-(t1-mu.new.combined)/sig.whitestripe.combined
# 	writeniigz(t1.whitestripe.combined.norm,paste(curr.path,scan.id,'/t1_whitestripe_combined_new_t2.nii.gz',sep=''))
# 	
# 	mu.new.combined<-mean(t2[whitestripe.combined.ind])
# 	sig.whitestripe.combined<-sd(t2[whitestripe.combined.ind])
# 	t2.whitestripe.combined.norm<-(t2-mu.new.combined)/sig.whitestripe.combined
# 	writeniigz(t2.whitestripe.combined.norm,paste(curr.path,scan.id,'/t2_whitestripe_combined_new_t2.nii.gz',sep=''))	
# 
# 	mu.new.combined<-mean(flair[whitestripe.combined.ind])
# 	sig.whitestripe.combined<-sd(flair[whitestripe.combined.ind])
# 	flair.whitestripe.combined.norm<-(flair-mu.new.combined)/sig.whitestripe.combined
# 	writeniigz(flair.whitestripe.combined.norm,paste(curr.path,scan.id,'/flair_whitestripe_combined_new_t2.nii.gz',sep=''))	
# 
# 	mu.new.combined<-mean(pd[whitestripe.combined.ind])
# 	sig.whitestripe.combined<-sd(pd[whitestripe.combined.ind])
# 	pd.whitestripe.combined.norm<-(pd-mu.new.combined)/sig.whitestripe.combined
# 	writeniigz(pd.whitestripe.combined.norm,paste(curr.path,scan.id,'/pd_whitestripe_combined_new_t2.nii.gz',sep=''))	
# 	
# 	print(paste('Finished with subject ',scan.id,' ...'))
# }
# 
# save(d.flair,d.flair.wholeimage.norm,d.flair.new.norm,d.t1,d.t1.wholeimage.norm,
#      d.t1.new.norm,d.pd,d.pd.wholeimage.norm,d.pd.new.norm,d.t2,d.t2.wholeimage.norm,
#      d.t2.new.norm,d.flair.whitestripe.norm,d.t1.whitestripe.norm,d.t2.whitestripe.norm,
#      d.pd.whitestripe.norm,whitestripe.t1,whitestripe.t2,whitestripe.flair,whitestripe.pd,
#      d.t1.whitestripe.t1only.norm,d.t2.whitestripe.t1only.norm,d.pd.whitestripe.t1only.norm,
#      d.flair.whitestripe.t1only.norm,file='normalization_densities_whitestripe_revisedt2norm2_NINDS_3T.RData')
# #load('normalization_densities_whitestripe_revisedt2norm2_NINDS_3T.RData')

#####################################################################################################################
#####################################################################################################################
# Assess the normalizations
#####################################################################################################################
#####################################################################################################################

# # setwd('~/NIRVANA/out')
# 
# 
# ##t1
# pdf('ms_ninds3tdata_t1_hists.pdf')
# par(mfrow=c(2,2))
# cols=rep(brewer.pal(9,'Set1'),length(scan.ids.subset)/9+1)
# plot(c(-500,3000),c(0,0.0005),type='n',main='Raw t1',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1[[scan.i]]$x,d.t1[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.4),type='n',main='Normalized t1 - Whole Image',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1.wholeimage.norm[[scan.i]]$x,d.t1.wholeimage.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized t1 - Proposed',xlab='t1 Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.t1.new.norm[[scan.i]]$x,d.t1.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1.whitestripe.norm[[scan.i]]$x,d.t1.whitestripe.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-80,50),c(0,0.04),type='n',main='Proposed White Stripe - T1',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1.whitestripe.t1only.norm[[scan.i]]$x,d.t1.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()



##t2
# pdf('ms_ninds3tdata_t2_hists.pdf')
# par(mfrow=c(2,2))
# plot(c(-500,3000),c(0,0.005),type='n',main='Raw t2',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2[[scan.i]]$x,d.t2[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.4),type='n',main='Normalized t2 - Whole Image',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2.wholeimage.norm[[scan.i]]$x,d.t2.wholeimage.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized t2 - Proposed',xlab='t2 Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.t2.new.norm[[scan.i]]$x,d.t2.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2.whitestripe.norm[[scan.i]]$x,d.t2.whitestripe.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - T1',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2.whitestripe.t1only.norm[[scan.i]]$x,d.t2.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# 
# ##FLAIR
# pdf('ms_ninds3tdata_flair_hists.pdf')
# par(mfrow=c(2,2))
# plot(c(-500,3000),c(0,0.005),type='n',main='Raw FLAIR',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair[[scan.i]]$x,d.flair[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.8),type='n',main='Normalized FLAIR - Whole Image',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair.wholeimage.norm[[scan.i]]$x,d.flair.wholeimage.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized FLAIR - Proposed',xlab='FLAIR Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.flair.new.norm[[scan.i]]$x,d.flair.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-100,20),c(0,0.2),type='n',main='Proposed White Stripe - FLAIR',xlab='FLAIR Intensity',ylab='density',ylim=c(0,0.05))
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair.whitestripe.norm[[scan.i]]$x,d.flair.whitestripe.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-4,2),c(0,1),type='n',main='Proposed White Stripe - T1',xlab='FLAIR Intensity',ylab='density',ylim=c(0,0.5))
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair.whitestripe.t1only.norm[[scan.i]]$x,d.flair.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# 
# 
# ##pd
# pdf('ms_ninds3tdata_pd_hists.pdf')
# par(mfrow=c(2,2))
# plot(c(-500,3000),c(0,0.005),type='n',main='Raw pd',xlab='pd Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd[[scan.i]]$x,d.pd[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.4),type='n',main='Normalized pd - Whole Image',xlab='pd Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd.wholeimage.norm[[scan.i]]$x,d.pd.wholeimage.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized pd - Proposed',xlab='pd Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.pd.new.norm[[scan.i]]$x,d.pd.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd.whitestripe.norm[[scan.i]]$x,d.pd.whitestripe.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - T1',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd.whitestripe.t1only.norm[[scan.i]]$x,d.pd.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# pdf('ms_ninds3tdata_raw_hists.pdf')
# par(mfrow=c(2,2))
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw FLAIR',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair[[scan.i]]$x,d.flair[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw T1',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1[[scan.i]]$x,d.t1[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw T2',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2[[scan.i]]$x,d.t2[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw PD',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd[[scan.i]]$x,d.pd[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# #####################################################################################################################
# #####################################################################################################################
# # Assess the normalizations -- REMOVE BG
# #####################################################################################################################
# #####################################################################################################################
# 
# #This function plots a line only within a range of x values.
# lines.inrange<-function(x,y,xlim,...) lines(x[(x>=min(xlim))&(x<=max(xlim))],y[(x>=min(xlim))&(x<=max(xlim))],...)
# 
# #This function calculates the mean of a density.
# get.mean<-function(d) {
# 	d.mean.fun<-approxfun(d$x,d$y*d$x)
# 	return(integrate(d.mean.fun,min(d$x),max(d$x),subdivisions=1E5,stop.on.error=FALSE)$value)
# }
# 
# ##t1
# pdf('ms_ninds3tdata_t1_hists_nobg.pdf')
# par(mfrow=c(2,2))
# cols=rep(brewer.pal(9,'Set1'),length(scan.ids)/9+1)
# plot(c(500,3000),c(0,0.0005),type='n',main='Raw t1',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t1[[scan.i]]$x,d.t1[[scan.i]]$y,xlim=c(get.mean(d.t1[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(0,5),c(0,0.4),type='n',main='Normalized t1 - Whole Image',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t1.wholeimage.norm[[scan.i]]$x,d.t1.wholeimage.norm[[scan.i]]$y,xlim=c(get.mean(d.t1.wholeimage.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized t1 - Proposed',xlab='t1 Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.t1.new.norm[[scan.i]]$x,d.t1.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-3,3),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t1.whitestripe.norm[[scan.i]]$x,d.t1.whitestripe.norm[[scan.i]]$y,xlim=c(get.mean(d.t1.whitestripe.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-55,30),c(0,0.04),type='n',main='Proposed White Stripe - T1',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t1.whitestripe.t1only.norm[[scan.i]]$x,d.t1.whitestripe.t1only.norm[[scan.i]]$y,xlim=c(get.mean(d.t1.whitestripe.t1only.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# 
# 
# ##t2
# pdf('ms_ninds3tdata_t2_hists.pdf_nobg')
# par(mfrow=c(2,2))
# plot(c(200,3000),c(0,0.001),type='n',main='Raw t2',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t2[[scan.i]]$x,d.t2[[scan.i]]$y,xlim=c(get.mean(d.t2[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(0,5),c(0,0.4),type='n',main='Normalized t2 - Whole Image',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t2.wholeimage.norm[[scan.i]]$x,d.t2.wholeimage.norm[[scan.i]]$y,xlim=c(get.mean(d.t2.wholeimage.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized t2 - Proposed',xlab='t2 Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.t2.new.norm[[scan.i]]$x,d.t2.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-3,5),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t2.whitestripe.norm[[scan.i]]$x,d.t2.whitestripe.norm[[scan.i]]$y,xlim=c(get.mean(d.t2.whitestripe.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-2,5),c(0,0.4),type='n',main='Proposed White Stripe - T1',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.t2.whitestripe.t1only.norm[[scan.i]]$x,d.t2.whitestripe.t1only.norm[[scan.i]]$y,xlim=c(get.mean(d.t2.whitestripe.t1only.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# 
# ##FLAIR
# pdf('ms_ninds3tdata_flair_hists_nobg.pdf')
# par(mfrow=c(2,2))
# plot(c(100,2000),c(0,0.002),type='n',main='Raw FLAIR',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.flair[[scan.i]]$x,d.flair[[scan.i]]$y,col=cols[scan.i],xlim=c(get.mean(d.flair[[scan.i]]),Inf),type='l',lwd=3)
# }
# plot(c(0,5),c(0,0.6),type='n',main='Normalized FLAIR - Whole Image',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.flair.wholeimage.norm[[scan.i]]$x,d.flair.wholeimage.norm[[scan.i]]$y,xlim=c(get.mean(d.flair.wholeimage.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized FLAIR - Proposed',xlab='FLAIR Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.flair.new.norm[[scan.i]]$x,d.flair.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-20,10),c(0,0.05),type='n',main='Proposed White Stripe - FLAIR',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.flair.whitestripe.norm[[scan.i]]$x,d.flair.whitestripe.norm[[scan.i]]$y,xlim=c(get.mean(d.flair.whitestripe.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-3,3),c(0,0.5),type='n',main='Proposed White Stripe - T1',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.flair.whitestripe.t1only.norm[[scan.i]]$x,d.flair.whitestripe.t1only.norm[[scan.i]]$y,xlim=c(get.mean(d.flair.whitestripe.t1only.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# 
# 
# ##pd
# pdf('ms_ninds3tdata_pd_hists_nobg.pdf')
# par(mfrow=c(2,2))
# plot(c(500,4000),c(0,0.001),type='n',main='Raw pd',xlab='pd Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.pd[[scan.i]]$x,d.pd[[scan.i]]$y,xlim=c(get.mean(d.pd[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(0,3),c(0,1),type='n',main='Normalized pd - Whole Image',xlab='pd Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.pd.wholeimage.norm[[scan.i]]$x,d.pd.wholeimage.norm[[scan.i]]$y,xlim=c(get.mean(d.pd.wholeimage.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# # plot(c(-5,5),c(0,0.4),type='n',main='Normalized pd - Proposed',xlab='pd Intensity',ylab='density')
# # for (scan.i in scan.ids.subset) {
# # 	lines(d.pd.new.norm[[scan.i]]$x,d.pd.new.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# # }
# plot(c(-4,5),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.pd.whitestripe.norm[[scan.i]]$x,d.pd.whitestripe.norm[[scan.i]]$y,xlim=c(get.mean(d.pd.whitestripe.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - T1',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines.inrange(d.pd.whitestripe.t1only.norm[[scan.i]]$x,d.pd.whitestripe.t1only.norm[[scan.i]]$y,xlim=c(get.mean(d.pd.whitestripe.t1only.norm[[scan.i]]),Inf),col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
# 
# pdf('ms_ninds3tdata_raw_hists.pdf')
# par(mfrow=c(2,2))
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw FLAIR',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair[[scan.i]]$x,d.flair[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw T1',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1[[scan.i]]$x,d.t1[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw T2',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2[[scan.i]]$x,d.t2[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# plot(c(-500,3500),c(0,0.001),type='n',main='Raw PD',xlab='FLAIR Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd[[scan.i]]$x,d.pd[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# dev.off()
#########################
# INSPECT FAILED CURVES #
#########################
# 
# ##t1
# #make histogram plot
# par(mfrow=c(1,1))
# plot(c(-80,50),c(0,0.04),type='n',main='Normalized t1 - Proposed White Stripe - T1 Only',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1.whitestripe.t1only.norm[[scan.i]]$x,d.t1.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# #select area
# xy<-locator(2)
# xy$x<-sort(xy$x)
# xy$y<-sort(xy$y)
# #find any scans with curves in this region
# problem.curves<-c()
# for (scan.i in scan.ids.subset) {
# 	if (sum((d.t1.whitestripe.t1only.norm[[scan.i]]$x>xy$x[1])&(d.t1.whitestripe.t1only.norm[[scan.i]]$x<xy$x[2])&(d.t1.whitestripe.t1only.norm[[scan.i]]$y>xy$y[1])&(d.t1.whitestripe.t1only.norm[[scan.i]]$y<xy$y[2]))>0) problem.curves<-c(problem.curves,scan.ids[scan.i])
# }
# problem.curves
# 
# ##t1
# #make histogram plot
# plot(c(-5,5),c(0,0.4),type='n',main='Proposed White Stripe - FLAIR',xlab='t1 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t1.whitestripe.norm[[scan.i]]$x,d.t1.whitestripe.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# #select area
# xy<-locator(2)
# xy$x<-sort(xy$x)
# xy$y<-sort(xy$y)
# #find any scans with curves in this region
# problem.curves<-c()
# for (scan.i in scan.ids.subset) {
# 	if (sum((d.t1.whitestripe.norm[[scan.i]]$x>xy$x[1])&(d.t1.whitestripe.norm[[scan.i]]$x<xy$x[2])&(d.t1.whitestripe.norm[[scan.i]]$y>xy$y[1])&(d.t1.whitestripe.norm[[scan.i]]$y<xy$y[2]))>0) problem.curves<-c(problem.curves,scan.ids[scan.i])
# }
# problem.curves
# 
# 
# ##flair
# #make histogram plot
# par(mfrow=c(1,1))
# plot(c(-4,2),c(0,0.5),type='n',main='Normalized flair - Proposed White Stripe - t1 Only',xlab='flair Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.flair.whitestripe.t1only.norm[[scan.i]]$x,d.flair.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# #select area
# xy<-locator(2)
# xy$x<-sort(xy$x)
# xy$y<-sort(xy$y)
# #find any scans with curves in this region
# problem.curves<-c()
# for (scan.i in scan.ids.subset) {
# 	if (sum((d.flair.whitestripe.t1only.norm[[scan.i]]$x>xy$x[1])&(d.flair.whitestripe.t1only.norm[[scan.i]]$x<xy$x[2])&(d.flair.whitestripe.t1only.norm[[scan.i]]$y>xy$y[1])&(d.flair.whitestripe.t1only.norm[[scan.i]]$y<xy$y[2]))>0) problem.curves<-c(problem.curves,scan.ids[scan.i])
# }
# problem.curves
# 
# 
# 
# 
# ##t2
# #make histogram plot
# par(mfrow=c(1,1))
# plot(c(-5,15),c(0,0.2),type='n',main='Normalized t2 - Proposed White Stripe - t1 Only',xlab='t2 Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.t2.whitestripe.t1only.norm[[scan.i]]$x,d.t2.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# #select area
# xy<-locator(2)
# xy$x<-sort(xy$x)
# xy$y<-sort(xy$y)
# #find any scans with curves in this region
# problem.curves<-c()
# for (scan.i in scan.ids.subset) {
# 	if (sum((d.t2.whitestripe.t1only.norm[[scan.i]]$x>xy$x[1])&(d.t2.whitestripe.t1only.norm[[scan.i]]$x<xy$x[2])&(d.t2.whitestripe.t1only.norm[[scan.i]]$y>xy$y[1])&(d.t2.whitestripe.t1only.norm[[scan.i]]$y<xy$y[2]))>0) problem.curves<-c(problem.curves,scan.ids[scan.i])
# }
# problem.curves
# 
# 
# 
# ##pd
# #make histogram plot
# par(mfrow=c(1,1))
# plot(c(-5,15),c(0,0.8),type='n',main='Normalized pd - Proposed White Stripe - t1 Only',xlab='pd Intensity',ylab='density')
# for (scan.i in scan.ids.subset) {
# 	lines(d.pd.whitestripe.t1only.norm[[scan.i]]$x,d.pd.whitestripe.t1only.norm[[scan.i]]$y,col=cols[scan.i],type='l',lwd=3)
# }
# #select area
# xy<-locator(2)
# xy$x<-sort(xy$x)
# xy$y<-sort(xy$y)
# #find any scans with curves in this region
# problem.curves<-c()
# for (scan.i in scan.ids.subset) {
# 	if (sum((d.pd.whitestripe.t1only.norm[[scan.i]]$x>xy$x[1])&(d.pd.whitestripe.t1only.norm[[scan.i]]$x<xy$x[2])&(d.pd.whitestripe.t1only.norm[[scan.i]]$y>xy$y[1])&(d.pd.whitestripe.t1only.norm[[scan.i]]$y<xy$y[2]))>0) problem.curves<-c(problem.curves,scan.ids[scan.i])
# }
# problem.curves



