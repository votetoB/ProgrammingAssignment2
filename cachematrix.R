## My plan is to copy the example's approach
## 

## Same constructor as in the example, nothing more.
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



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Algorighm I used can be found at:
    ## https://en.wikipedia.org/wiki/Gaussian_elimination#Finding_the_inverse_of_a_matrix
    
    i <- x$getSolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    #Dimension of the matrix will be usefull later
    N <- nrow(data)
    ## Make an augmented (new word for me :) matrix 
    augmented <- cbind(data, diag(N))
    
    ## function, that makes gaussian transform for the row j
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
    
    #function to swap two rows of our matrix
    swaprows <- function(j, s){
        t <- augmented[j, ]
        augmented[j, ] <<- augmented[s, ]
        augmented[s, ] <<- t
    }
    
    for (j in 1:N){
        #check for diagonal element != 0. If not, find the row with non-zero value
        #                                         at this column
        z <- j + 1
        while (augmented[j, j] == 0){
            swaprows(j, z)
            z <- z + 1
            
            ## if matrix is irrevertable (det(A) == 0), we will get here and
            ##                                          return "MISTAKE"!
            if (z > N){return ("MISTAKE")}
        }
        makerow(j)
    }
    
    i <- augmented[, (N+1):(N* 2)]
    x$setSovle(i)
    i
}
