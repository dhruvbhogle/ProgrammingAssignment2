## This programme and its functions are used to calculate the inverse of a matrix
#and cache it in memory. For complex and iterative programs, if there is no change
#to the values being calculated in the current iteration, we can optimise our 
#code by first checking whether a similar calculation has already been performed, 
#and if so, simply recall that calculation's result.

## This function is used to create a list of 4 inner functions: 
#get, set, getinv and setinv. 
#Get and set are used to obtain or set the value of the matrix 
#getinv and setinv are used to obtain and recall the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	mat<- NULL
    set <- function (y){
        x<<-y
        mat<<-NULL
    }
    get <- function() x
    setinv <- function(inv) mat <<- inv
    getinv <- function() mat
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function calculates the inverse of the matrix and stores it in the cache. 

cacheSolve <- function(x, ...) {
       mat<-x$getinv()
    if (!is.null(mat)){
        message("Getting the cached data")
        return (mat)
    }
    data <- x$get();
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
}
