# makeCacheMatrix defines the methods and variables used to
# store and retrieve the cached inverse of the matrix

# The first time cacheSolve is called, it calculates the inverse
# matrix and stores it. From now on, it returns the stored inverse

# Sample Execution

#a<-rbind(c(1,1,0),c(1,0,1),c(0,1,0))

#mb<-makeCacheMatrix(a)

#i<-cacheSolve(mb)   #First time calculates inv

#i<-cacheSolve(mb)   #Second time returns cached
#returns the cached inverse

# i %*% a            # a product inverse = Identity

#       [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1


#This function defines the methods and variables 
#used to store and retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL        				       
    
    set <- function(pM) { 				   	
		  m <<- pM         				  #save the matrix
		  inv <<-NULL               #set inverse of matrix to NULL
    }
	
   	get <- function() x                    #return stored matrix
    setInv <- function(pInv) inv <<- pInv  #set the inverse of the matrix
	 
    getInv <- function() inv               #return the inverse of the matrix
	 
    list(get = get, set=set, setInv=setInv,getInv=getInv)
}

# The function gets the stored inverse matrix of x 
# from the cache. if the cached inv is NULL,is the first time, then 
# calculates the inverse of x and caches it
# else returns the cached inverse of the matrix

cacheSolve <- function(x) {
    #[A]
    inv <- x$getInv()         #get the cached inverse of the matrix 
    
		if(is.null(inv)){         #if the cached inverse in x is NULL calculate it
			m <- x$get()            #get the matrix stored in x
      #[B]
			inv <- solve(m)         #calculate de inverse of the matrix
			x$setInv(inv)			      #save the inverse of the matrix x
		}
    else message("returns the cached inverse")

    inv							#return the inverse of the matrix x
                    #inv contains either the value retrieved at [A] 
                    #or the one calculated at [B]
}