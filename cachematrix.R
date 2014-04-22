## makeCacheMatrix creates a cache matrix 
## It creates four functions set, get, setinverse, getinverse

## It creates matrix x and initilizes its inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y) {
        x<<-y
        inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(in1) inverse<<-in1
        getinverse<- function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## It complutes the inverse of the matrix for the first time then it access cached data

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if(!is.null(inverse)) {
        message("getting cache data")
        return(inverse)
        }
        data<-x$get()
        x2<-solve(data)
        x$setinverse(x2)
        x2
}
