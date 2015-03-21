# R will create a list (cachelist) in globalenv to save the matrix
# and its inverse, if it does not already exist.
makeCacheMatrix<-function(){
    if(!exists("cachelist",parent.env(environment()),inherit=TRUE)){
        cachelist<<-list(list(matrix(),matrix()))# cachelist is a list
        # which elements are lists of two elements (a matrix and its 
        # inverse).
    }
}

# R will look into cachelist. If the matrix is there means that its
# inverse has been already calculated and will return it, if not will
# calculate it and save both (matrix and its invers) as a new element
# of cachelist.
cacheSolve<-function(m){
    makeCacheMatrix()
    l<-length(cachelist)+1 # find the next free position in the cachelist.
    list_cache<-cachelist # to work in local env.
    for(i in 1:length(list_cache)){ # look for in cachelist... 
        if(identical(m,list_cache[[i]][[1]])){ # compare the matrix in
            # cachelist with the matrix-objective.
            message("getting cache data")
            mi<-list_cache[[i]][[2]]
            return(mi) # returne inverse of matrix-objective.
        }
    } # if matrix is not in cachelist...
    mi<-solve(m) # calculate the inverse
    list_cache[[l]]<-list(m,mi) # save the new element in the next
    # free position of cachelist.
    cachelist<<-list_cache # to update cachelist in globalenv.
    mi  # returne inverse of matrix-objective.
}