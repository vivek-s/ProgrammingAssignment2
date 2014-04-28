## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y,n_row=1,n_col=1){
                x<<-matrix(y,nrow=n_row,ncol=n_col)
                inv<<-matrix(nrow=n_row,ncol=n_col)
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(sum(is.na(inv)) == nrow(inv)*ncol(inv)){
                data <- x$get()
                inv<-(solve(data))
                x$setinv(inv)
                return(inv)
        }
        message("getting cached data")
        inv
}
