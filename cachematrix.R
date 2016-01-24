## This function creates a special "matrix" object that can cache its inverse

## I wrote this function according to the example of Caching the Mean of a Vector. 
## solve is the function to get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<-solve
        getmatrix<-function() m
        list(set=set,get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## if the inverse has already been calculated, this function would retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setmatrix(m)
        m
}

