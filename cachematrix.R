## These functions create a matrix, inverse the matrix and then stores the results
## in a cache to be called upon at a later time 

## makeCacheMatrix is a function that retains a list of commands that creates
## a matrix and then inverses this matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inversem) m<<-inversem
	getinverse<-function() m
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)

}


## Return a matrix that is the inverse of x, if this has already been computated
##return "getting cached data" and then this function will retrieve the inverse
##from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m<-x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinverse(m)
	m

}
