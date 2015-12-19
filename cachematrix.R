## My plan is to copy the example's approach
## 

## Same constructor as in the example
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setSolve <- function(Solve) i <<- Solve
    getSolve <- function() i
    list(set = set, get = get,
         setSovle = setSolve,
         getSolve = getSolve)
}


## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getSolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    N <- nrow(data)
    ## Make an augmented (new word for me :) matrix 
    augmented <- cbind(data, diag(N))
    
    ## makes gaussian transform for the row j
    makerow <- function (j) {
        #dividing the row by it's main element
        divisor <- augmented[j, j]
        augmented[j,] <<- augmented[j,]/divisor
        
        #substracting rows (so j-column is all 0, except at j-row)
        for (k in 1:N){
            if (k == j) next
            augmented[k, ] <<- augmented[k, ] - augmented[j, ] * augmented[k, j]
        }
    }
    
    #swappng two selected rows
    swaprows <- function(j, s){
        t <- augmented[j, ]
        augmented[j, ] <<- augmented[s, ]
        augmented[s, ] <<- t
    }
    
    for (j in 1:N){
        #check for diagonal element != 0
        z <- j + 1
        while (augmented[j, j] == 0){
            z <- z + 1
            swaprows(j, z)
            if (z > N){return ("MISTAKE")}
        }
        makerow(j)
    }
    
    i <- augmented[, (N+1):(N* 2)]
    x$setSovle(i)
    i
}
