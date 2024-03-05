library(igraph)
library(colorRamps)
library(extraDistr)
library(matrixStats)

#Nanda was here

# function to create a rooted tree which represents a cultural system
generate_rooted_tree_branching <- function(num_nodes, branching_factor) {
  g <- graph.empty(n = num_nodes, directed = TRUE)
  
  edgeList<-c()
  for (i in 1:(num_nodes-1)){
#	outDegreeThisNode<-1+floor(runif(1)*branching_factor)
#	outDegreeThisNode<-max(1, round(rnorm(1,mean=branching_factor,sd=1)))
	outDegreeThisNode<-branching_factor
	k<-i+1
	numConn<-0
	for (j in (i+1):num_nodes){
		if (numConn<outDegreeThisNode && !j%in%edgeList[c(FALSE,TRUE)]) {
			edgeList<-c(edgeList, i,j)
			numConn<-numConn+1
		}
	}
  }
  g<-add_edges(g, c(edgeList))
  return(g)
}


# Function to create a rooted tree which represents a cultural system
generate_rooted_tree_betaDistr <- function(num_nodes, tree_layers, alpha1, beta1) {
  g <- graph.empty(n = num_nodes, directed = TRUE)
  edgeList<-c()
  
## define distribution of nodes across layers
	nodeLayer<-1 + rbbinom((num_nodes-1), (tree_layers-1), alpha = alpha1, beta = beta1)

	layerDistr<-rep(1,tree_layers)
	for (node in nodeLayer[1:(length(nodeLayer)-tree_layers)]){
		layerDistr[node]<-layerDistr[node]+1
	}

	layerNode<-0
	for (i in 1:length(layerDistr)) layerNode<-c(layerNode,rep(i,layerDistr[i]))
	inDegrees<-rep(0,num_nodes)
  
	for (layer in 0:(max(layerNode)-1)){
		if (layer==0) {
			for (j in which(layerNode==1)){
				edgeList<-c(edgeList,1,j)
				inDegrees[j]<-inDegrees[j]+1
			}
		}
		if (layer > 0){
			for (i in which(layerNode==layer)){
				nextLayer<-which(layerNode==(layer+1))	
				lowestInNextLayer<-min(inDegrees[nextLayer])
			
				candidates<-nextLayer[which(inDegrees[nextLayer]==lowestInNextLayer)]
			
				targetNodes<-candidates[1]
				if (length(targetNodes>0)){
					for (j in targetNodes){
						edgeList<-c(edgeList,i,j)
						inDegrees[j]<-inDegrees[j]+1
					}
				}
			}
			for (i in which(layerNode==(layer+1))){
				if (inDegrees[i] == 0){
					sourceNodes<-which(layerNode==layer)
					sourceNode<-ifelse (length(sourceNodes)==1, sourceNodes[1], sample(sourceNodes,1))
					edgeList<-c(edgeList,sourceNode,i)
					inDegrees[i]<-inDegrees[i]+1
				}
			}
		}
	}
	g<-add_edges(g, c(edgeList))
	return(g)
}

initializePopulation<-function(N,num_nodes, adj_matrix){
	## start with empty repertoires (but fill them up in the next step)
	repertoires<-matrix(0, nrow=N, ncol=num_nodes)
	## everyone has the root trait of the cultural system
	repertoires[,1]<-1
			
	### initialize the population with repertoires (all connected to the tree root) 
	### with sizes drawn from a uniform distribution between 1:num_nodes
	for (ind in 1:N){
		numTraits<- sample(1:num_nodes,1)	# number of traits of this agent
		for (tr in 2:numTraits){			# add (randomly chosen) learnable traits
			unknownTraits<-which(repertoires[ind,]==0)
			# for which of these traits is the parent trait in the repertoire?
			# these are the traits currently 'learnable' to the individual
			learnableTraits<-c()
			for (trait in unknownTraits){
				parent<-which(adj_matrix[,trait]==1)
				if (prod(repertoires[ind,parent]==1)) learnableTraits<-c(learnableTraits,trait)
			}
			
			if (length(learnableTraits)==1) repertoires[ind,learnableTraits]<-1
			if (length(learnableTraits)>1){
				addTrait<-sample(learnableTraits,1)
				repertoires[ind,addTrait]<-1
			}
		}
	}
	return(repertoires)
}

assignAges<-function(repertoires){
	## for age-based social learning, we need to assume initial ages. 
	## let's assume the age is proportional to the repertoire size
	ageScalar<-1	
	N<-nrow(repertoires)	
	popAge<- rep(0,N)
	for (ind in 1:N) popAge[ind]<-ageScalar * sum(repertoires[ind,])
	
	return (popAge)
	
}

getLearnableTraits<-function(repertoires, ind, adj_matrix){
	# which traits are currently not in the individual's repertoire?
	unknownTraits<-which(repertoires[ind,]==0)
					
	# for which of these traits is the parent trait in the repertoire?
	# these are the traits currently 'learnable' to the individual
	learnableTraits<-c()
	for (trait in unknownTraits){
		parent<-which(adj_matrix[,trait]==1)
		if (prod(repertoires[ind,parent]==1)) learnableTraits<-c(learnableTraits,trait)
	}
	
	## if there are any negative mutual relationships between traits, remove them from the set of learnable traits
	for (trait in learnableTraits){
		blocker<-which(adj_matrix[,trait]== -1)
		if (sum(repertoires[ind,blocker])>0) learnableTraits<-learnableTraits[!learnableTraits==trait]
	}
	
	return(learnableTraits)
}

learnSocially <- function(repertoires, ind, adj_matrix, learningStrategy, M, popAge, olderPref){
	## sample M random other individuals
	pool<-1:N
	poolOthers<-pool[-ind] # agents do not sample themselves
	models<-sample(poolOthers, M, replace=FALSE)
	
	## randomly pick 1 trait from each model
	## only consider traits the learning agent do not know yet
	observedBehaviours<-c()
	observedModels<-c()
	for (model in models){
		newTraits<-c()
		for (k in 1:num_nodes){
			if (repertoires[model,k]==1 && repertoires[ind,k]==0) newTraits<-c(newTraits,k)
		}
		if (length(newTraits)>1) {
			tr<-sample(newTraits,1)
			observedBehaviours<-c(observedBehaviours, tr)
			observedModels<-c(observedModels, model)
		}
		if (length(newTraits)==1) {
			observedBehaviours<-c(observedBehaviours, newTraits)
			observedModels<-c(observedModels, model)
		}
	}
	
	## check if the vectors are filled as expected
	observedBehaviours
	observedModels
	
	## if there's no trait to learn among the observed ones, skip
	## SLpay was initialized at NA so nothing happens in that case
	## if there IS something to learn:
	if (length(observedBehaviours)>0){
		## list of weights of each observed behaviour
		## weights depend on learning strategy
		## normalized weights are used for choice
		wList<-c()
		selectedTrait<-c()	

		##### STRATEGY 1: payoff-based social learning #####
		
		if (learningStrategy==1){
			wList<-payoffs[observedBehaviours]/sum(payoffs[observedBehaviours])
		}
		##################
		
		###### STRATEGY 2: similarity based learning ######
		if (learningStrategy==2){
			## check for all agents how similar they are to self
			for (mod in observedModels){
				simToFocal<-0
				for (k in 1:num_nodes){ ## loop over all traits and sum similarity
					if (repertoires[ind,k]==repertoires[mod,k]) simToFocal<-simToFocal+1;
				}
				wList<-c(wList, simToFocal)
			}
		}
		##################	

		######	STRATEGY 3: age-based social learning #####				
		if (learningStrategy==3){
			## based on age similarity
			## check for all agents how similar they are to self
			for (mod in observedModels){
				w<- 10^-8
				ageDif<-popAge[mod]-popAge[ind]								
				if (ageDif >=0) w<- 0.5^ageDif
#				if (ageDif >=0) 
#				w<-(10^-8) + dnorm(ageDif, mean=olderPref, sd=1)
				wList<-c(wList, w)
			}			
		}	
		################
		
		######	STRATEGY 4: conformist social learning #####				
		if (learningStrategy==4){
			## count the selected behaviours and weigh common ones more
			for (mod in 1:length(observedBehaviours)){
				w<- length(which(observedBehaviours==observedBehaviours[mod]))
				wList<-c(wList, w)
			}			
		}
		
		################
								
		
		###### STRATEGY 0:  random learning benchmark
		if (learningStrategy==0){
			## select a trait you dont have at random
			wList<-rep(1,length(observedBehaviours))							
		}
		############
		
		### MAKE CHOICE ###
		selectedTrait<-ifelse(length(observedBehaviours)==1, observedBehaviours[1], sample(observedBehaviours,1,prob=wList))
#		if (length(observedBehaviours)==1) {selectedTrait<-observedBehaviours[1]}
#		else {selectedTrait<-sample(observedBehaviours, 1, prob=wList)}
		return(selectedTrait)	
	}						
}

## if showPopState==1, vary node size with portion of agents with that trait
plotTree <- function(tree, repertoires, showPopState) {	
	# Use layout_as_tree to create a tree layout
	layout <- layout_as_tree(tree, root=1, rootlevel=0)

	# Calculate the x-coordinates to center the tree horizontally
	levels <- distances(tree, v = 1, to = V(tree), mode = "out")
	level_widths <- table(levels)

	# Calculate the total width of the tree
	total_width <- max(level_widths)
	mid_x<-total_width/2
	x_coordinates<-c()
	for (level in level_widths) {
		x0<-mid_x
		for (k in 1:level){
			x<- (k-0.5) / level
			x_coordinates<-c(x_coordinates, x)
		}
	}
	# Set the x-coordinates in the layout
	layout[, 1] <- x_coordinates

	# set colouring according to depth
	nodeDepths<-1+distances(tree,v=1,to=V(tree),mode="out")
	colramp <- colorRampPalette(c("white", "blue","green","orange", "red"))
	color_palette <- colramp(max(nodeDepths))
	color <- color_palette[1:num_nodes]
	V(tree)$color <- color_palette[nodeDepths]
	
	## plot the tree
	if (showPopState==0) {
		plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color='black',
		vertex.color = V(tree)$color, vertex.label=NA)
	}
	if (showPopState==1){
		V(tree)$propAdopted <- rep(0,num_nodes)
		for (tr in 2:num_nodes) V(tree)$propAdopted[tr]<-mean(repertoires[,tr])
		plot(tree, layout = layout, vertex.size=30*V(tree)$propAdopted, edge.arrow.size = 0.5, edge.color='black',
			vertex.color = V(tree)$color, vertex.label=NA)		
	}
}


#### simulation parameters
num_nodes <- 16				# size of the cultural systems (number of nodes)
N<-100						# population size
M<-10						# number of demonstrators
timesteps<-5000			# number of time steps in the simulation
replicateSimulations<-30	# number of simulations per parameter setting
S<-0.99						# reliance on social learning; (1-S) is innovation rate
reset_rate<- 0.01			# probability that an individual is replaced by a naive individual

## set some default values for parameters for bookkeeping (some of these are undefined in the strategySuccess matrix)
tree_layers<-5
branching_factor<-2
alpha1<-1
beta1<-1
olderPref<-0


# parameters for plotting (number of time steps per printing event frequencies)
windowSize<-1000

### define a summary matrix with success of learning strategies
## it records the number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
strategySuccess<-matrix(nrow=0, ncol=9)


### PARAMETERS ARE SET ###
# loop over social learning strategies:
## 0=random; 1=payoff-based; 2=similarity-based; 3=age-based; 4=similarity-based
for (learningStrategy in c(0,1,2,3,4)){
#	dev.new(width=40, height=100)
	# define plotting areas for diagnostic simulation inspection
	par(mfrow=c(4,3), mar=c(1,1,1,1), las=1, xaxs='i', yaxs='i')
	
	# loop over graph settings
#	for (alpha1 in c(1,3)){
#		for (beta1 in c(1,3)){
#		for (olderPref in -10:20){
#	for (num_nodes in c(15)){
		for (branching_factor in c(1,3,num_nodes)){
			### set up environment (cultural system)
			exampleTree <- generate_rooted_tree_branching(num_nodes, branching_factor)
#			exampleTree <- generate_rooted_tree_betaDistr(num_nodes, tree_layers, alpha1, beta1)
			# for diagnostic plotting, we only show an example tree
			# for the actual simulations, we make a new tree in each replicate
			plotTree(exampleTree,0,0)
					
			## bookkeeping for some plotting 
			summmeanTraitsInSystem<-rep(0,timesteps)
			summVarTraitsInSystem<-rep(0,timesteps)
			summSLpay<-matrix(nrow=0, ncol=timesteps)
			
			### start simulation
			for (repl in 1:replicateSimulations){
				## show simulation progress on screen
				flush.console()
				print(paste('numNodes=', num_nodes, ' strat=', learningStrategy, ' bf=', branching_factor, ' alpha=', alpha1, ' beta=', beta1, ' olderPref=', olderPref, '  repl=', repl, sep=''))

				### define the cultural system ###
				## generate rooted tree to represent the cultural system
				tree <- generate_rooted_tree_branching(num_nodes, branching_factor)
				#tree <- generate_rooted_tree_betaDistr(num_nodes, tree_layers, alpha1, beta1)

				## derive square matrix of parent/child traits
				adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
				## root trait (at position 1,1) is its own parent
				adj_matrix[1,1]<-1
				
				### traits with a negative relationships
#				adj_matrix[2,3]<--1
#				adj_matrix[3,2]<--1
				
				
				## for some tracking the distribution of traits across depths we need to know the depth of each node			
				nodeDepths<-1+distances(tree,v=1,to=V(tree),mode="out")
				maxNodeDepth<-max(nodeDepths)
			
				## bookkeep the frequency of traits across tree depths (for checking stability over time)
				traitsAtDepth<-matrix(NA, nrow=timesteps, ncol=max(nodeDepths))
				## bookkeeping for output
				SLpay<-rep(NA,timesteps)				## payoff for social learning
				meanTraitsInSystem<-rep(NA,timesteps) 	## number of traits in the system
				varTraitsInSystem<-rep(NA, timesteps)
				
		# 		set payoffs for each trait
		#		payoffs<-rep(1,num_nodes)  	# fixed payoffs
				payoffs<-runif(num_nodes)	# random payoffs from uniform distribution
		#		payoffs<-runif(num_nodes) * nodeDepths # payoffs INcrease with depth
		#		payoffs<-runif(num_nodes) * (max(nodeDepths) - nodeDepths + 1) # payoffs DEcrease with depth
				payoffs<-2*payoffs/max(payoffs)
				### SYSTEM AND NODE PAYOFFS ARE SET

				####### INITIALIZE POPULATION #####
				repertoires<-initializePopulation(N, num_nodes, adj_matrix)
				popAge<-assignAges(repertoires)
				
				### population is now initialized... start running the model
				for (t in 1:timesteps){
					## sample a random individual
					ind<-sample(1:N,1)
					## will they learn individually or socially?
					r<-runif(1)
					
					learnableTraits<-getLearnableTraits(repertoires, ind, adj_matrix)
					SLpay[t]<-NA
					if (length(learnableTraits)>0){  #only try to learn if there's anything to learn for this agent
						if (r<S) {  # social learning
							selectedTrait<-learnSocially(repertoires, ind, adj_matrix, learningStrategy, M, popAge, olderPref)														
							
							if (length(selectedTrait)==1){
								######## calculate payoffs of learning
								focalPay<-0
								if (selectedTrait%in%learnableTraits) {
									repertoires[ind,selectedTrait]<-1
									focalPay<-payoffs[selectedTrait]
								}
								SLpay[t]<-focalPay
							}
							
						}
						else {	# individual learning (=innovation)
							selectedTrait<- learnableTraits[1]
							if (length(learnableTraits)>1) selectedTrait<-sample(learnableTraits,1)
							repertoires[ind,selectedTrait]<-1									
						}
					}
					## each time step the agent was sampled, their age increases by 1
					popAge[ind]<-popAge[ind]+1
				
					## replace an individual with a naive one at random
					## NB this is not appropriate for evolutionary sims
					if (runif(1) < reset_rate) {
						repertoires[ind,]<-c(1,rep(0,num_nodes-1))
						popAge[ind]<-0  ## reset the age of the agent to 0
					}
					
					# BOOKKEEPING for each time step
					# number of traits in the system
					meanTraitsInSystem[t]<-sum(repertoires) / (num_nodes * N)
			#		varTraitsInSystem[t]<-mean(colVars(repertoires))	
					
					# distribution of traits across the depths (layers) of the tree
					trDepth<-rep(0,maxNodeDepth)
					for (d in 1:maxNodeDepth){
						x<-which(nodeDepths==d)
						thisDepth<-sum(repertoires[,x], na.rm=TRUE)
						trDepth[d]<-thisDepth
					}
					traitsAtDepth[t,]<-trDepth/sum(repertoires)
				}
				
#				plotTree(tree, repertoires, 1)

				
				## do some plotting for inspecting the simulations
				if (repl==1) {plot(1:timesteps, SLpay, type='n', ylim=c(0,1), col='red', 
					axes=FALSE)
					axis(1, labels=F)
					axis(2)
					box()
				}
			
				## plot payoffs for social leraning for time windows (to smoothen)
				cols<-c("green", "orange", "red")
				for (w in 1:(timesteps/windowSize)){
					wi<-SLpay[1+((w-1)*windowSize):(w*windowSize)]			
					meanPayBin<- mean(wi, na.rm=TRUE)
					x<-(w-0.5)*windowSize
					points(x, meanPayBin, col=adjustcolor("blue", alpha=0.5), pch=16, cex=0.5)
				}
				
				
				## bookkeep for summary across simulation replicates
				summSLpay<-rbind(summSLpay,SLpay)
				summmeanTraitsInSystem<-summmeanTraitsInSystem+meanTraitsInSystem
			#	summVarTraitsInSystem<-summVarTraitsInSystem+varTraitsInSystem
				
				### add summary statistics to the overall master matrix
				## number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
				## characterize mean payoffs for a strategy as the last 10% of timesteps in the simulation
				#summThisSimulation<-c(num_nodes, branching_factor, learningStrategy, 
				#	repl, mean(SLpay[round(timesteps*0.9):timesteps], na.rm=TRUE))

				summThisSimulation<-c(num_nodes, branching_factor, tree_layers, alpha1, beta1, learningStrategy, olderPref,
					repl, mean(SLpay[round(timesteps*0.9):timesteps], na.rm=TRUE))
				
				strategySuccess<-rbind(strategySuccess, summThisSimulation)
				
				
				## simulation replicate is done
			}
			
			## plot some summary statistics for this simulation setting
			summSuc<-c()
			for (w in 1:(timesteps/windowSize)){
				wi<-1+((w-1)*windowSize):(w*windowSize)	
				x<-c()
				for (k in wi) {
					if (k<=ncol(summSLpay)) x<-c(x,summSLpay[,k])
				}
				summSuc<-c(summSuc, mean(x,na.rm=TRUE))
			}
			
			## add lines to the plot with mean payoffs of social learning
			## and the number of traits in the system  (proportion of total)
			lines((1:(timesteps/windowSize))*windowSize, summSuc, col='black', lwd=2)
			lines(1:timesteps, summmeanTraitsInSystem/replicateSimulations, col='red', lwd=2)
		#	lines(1:timesteps, summVarTraitsInSystem/replicateSimulations, col='purple', lwd=2)

			## set the colours for each layer
			colramp <- colorRampPalette(c("white", "blue","green","orange", "red"))
			color_palette <- colramp(max(nodeDepths))
			color <- color_palette[1:num_nodes]
						
			V(tree)$color <- color_palette[nodeDepths]
			
			## add a plot for the distributions across layers
			plot(1:timesteps, traitsAtDepth[,1], type='n', ylim=c(0,1))
			## calculate the areas for plotting
			plotMatTraitDepth<-matrix(1,nrow=1,ncol=timesteps)
			for (i in 1:ncol(traitsAtDepth)){
				yy<- 0;
				for (j in 1:i) yy<-yy+traitsAtDepth[,j]
				plotMatTraitDepth<-rbind(plotMatTraitDepth, 1-yy)
			}
			## do the plotting
			for (i in 1:(nrow(plotMatTraitDepth)-1)){
				yy0<-plotMatTraitDepth[i,]
				yy1<-plotMatTraitDepth[i+1,]
				polygon(c(1:timesteps, timesteps:1), c(yy0, rev(yy1)), col=unique(V(tree)$color)[i])
			}
		}
#	}
#	}
}

## give names to the columns of the simulation summary matrix: number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
strategySuccess<-data.frame(strategySuccess)
#names(strategySuccess)<-c('numNodes', 'branching', 'learningStrat', 'repl', 'meanPayoff')
names(strategySuccess)<-c('numNodes', 'branching_factor', 'tree_layers', 'alpha', 'beta', 'learningStrat', 'olderPref', 'repl', 'meanPayoff')
rownames(strategySuccess) <- NULL
strategySuccess

stratCols<-c('black', 'deepskyblue', 'firebrick', 'violet', 'forestgreen')
dev.off()

par(mfrow=c(1,3))
for (alph in unique(strategySuccess$alpha)){
	## what are the relevant x values?
	b<-subset(strategySuccess, alpha==alph)

	xx<-unique(b$beta)

	#dev.new(width=150, height=150)
	## create an overview plot
	plot(1:max(xx), 1:max(xx)*0+1, type='n', ylim=c(0,0.7), xlim=c(0,max(xx)),
		xlab='beta', ylab='Success of social learning strategy')
	stratCols<-c('black', 'deepskyblue', 'firebrick', 'violet', 'forestgreen')	

	for (learnStrat in 0:4){
		d<-subset(b, learningStrat==learnStrat)

		summM<-c()
		for (bet in unique(d$beta)){
			e<-subset(d, beta==bet)
			m<-mean(e$meanPayoff)
			summM<-c(summM, m)
			points(bet,m, pch=15+learnStrat, cex=1.5, col=stratCols[learnStrat+1])
		}
		lines(xx, summM, col=stratCols[learnStrat+1])
	}		
}
legend('bottomright', c('random', 'payoff', 'similarity', 'age', 'conformity'), col=stratCols, pch=15:19)

## what are the relevant x values?
xx<-unique(strategySuccess$branching_factor)
#xx<-unique(strategySuccess$numNodes)

#dev.new(width=150, height=150)
## create an overview plot
par(mfrow=c(1,3), xaxs='r', mar=c(5,4,4,2),lend=1)


for (brf in unique(strategySuccess$branching_factor)){
	b<-subset(strategySuccess,branching_factor==brf)
	boxplot(meanPayoff ~ learningStrat, data=b, col=stratCols, ylim=c(0,1.5))

	legend('bottomright', c('random', 'payoff', 'similarity', 'age', 'conformity'), col=stratCols, pch=15:19)
}
#legend('topright', c('random', 'payoff', 'similarity', 'age', 'conformity'), col=stratCols, pch=15:19)
