## The two below fucntions compute and cache the inverse of 
## a square invertible matrix.


##Returns a list that contains four elemets, and each of 
##them is a funtion.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        inv <<- NULL
        x   <<- y
    }
    get<-function(){
        x
    }
    setInv <- function(inverse){
        inv <<- inverse
    }
    getInv <- function(){
        inv
    }
    list(set    = set,
         get    = get,
         setInv = setInv,
         getInv = getInv)
}


## Compute the inverse of 'x'.
## Note: if  the inverse of 'x' has already been computed and cached,
##       it will return the inversion matrix without any calculation.
##       Otherwise, this function will compute and cache the inversion. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    ## make sure that 'x' is invertible
    dataDim=attributes(data)$dim
    if(dataDim[1] != dataDim[2]){
        message("Error: x must be a square matrix")
        return(m)
    }
    else{
        if(det(data) == 0){
            message("Error: x is an exactly singular matrix")
            return(m)
        }
        else{        
            m <- solve(data, ...)
            x$setInv(m)
        }
    }
    m
}
