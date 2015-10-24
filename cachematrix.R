makeCacheMatrix <- function(x=matrix(c(2,5,3,7),2,2))   ##initializing a function with x default square matrix whose inverse is to be calculated
				                                                ## this function create a public evironment where it contains matrix,their inverse and respective addresses
{  
        m <- NULL           ## initializing a null unique variable which is to be used later to calculate inverse
       
	  set <- function(y) {## this function when called will nullify the calculated inverse and updates the internally stored x with new value y. (e.g: x$set(matrix(1:4,2,2)))
               x <<- y      ## (<<-) is an special assignment operator and is used to allows the programmer to modify x declared outside of the current function in which the reference to the variable is made.
               m <<- NULL
        }                   

        get <- function(){  ## public method gets the original matrix x
                       x
			      }   

        setinv <- function(inv){ 
				    ## public method where inverse of x resides for cacheing later ...
				 m <<- inv
				}   
                            ## <<- cause a search to be made through parent environments for an existing definition of the variable being assigned

        getinv <- function()
			{ m
			}         ## public method gets the inverse if calculated else return null

        list(set = set, get = get , setinv = setinv , getinv = getinv)
                            ## this function will return a list which has functions for elements of this environment to global environment where inverse is to be calculated(lexical scoping)
}


cacheSolve <- function(val, ...)    ## this function computes , caches and returns matrix inverse by passing list of previously computed inverses
 {
        m <- val$getinv()   ## m is unique to this environment and holds the inverse of matrix
        if(!is.null(m)) {   ## if inverse is already calculated 

                message("getting cached data")
                return(m)   ## return the cached matrix
        }

        data <- val$get()   ## if not then gets the original matrix
        m <- solve(data, ...)  ## compute the inverse by using slove() function
        val$setinv(m)       ## stores the inverse for cacheing later
        m                   ## returns inverse matrix of val
}



## you can check whether an inverse is accurately calculated by multiplying actual matrix with its inverse (e.g: x %*% xinv)