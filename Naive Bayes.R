# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function(path = "mnist") {
  # Usage:
  #> mnist.data <- load_mnist()
  mnist_url <- list(train_images = paste("D:/","train-images-idx3-ubyte.gz", sep = ""),
                    train_labels = paste("D:/","train-labels-idx1-ubyte.gz", sep = ""),
                    test_images = paste("D:/","t10k-images-idx3-ubyte.gz", sep = ""),
                    test_labels = paste("D:/","t10k-labels-idx1-ubyte.gz", sep = ""))
  
  load_image_file <- function(filename) {
    ret <- list()
    f <- gzfile(filename, "rb")
    readBin(f, "integer", n = 1, size = 4, endian = "big")
    ret$n <- readBin(f, "integer", n = 1, size = 4, endian = "big")
    nrow <- readBin(f, "integer", n = 1, size = 4, endian = "big")
    ncol <- readBin(f, "integer", n = 1, size = 4, endian = "big")
    x <- readBin(f, "integer", n = ret$n * nrow * ncol, size = 1, signed = F)
    ret$x <- matrix(x, ncol = nrow * ncol, byrow = T)
    close(f)
    return(ret)
  }
  load_label_file <- function(filename) {
    f <- gzfile(filename, "rb")
    readBin(f, "integer", n = 1, size = 4, endian = "big")
    n <- readBin(f, "integer", n = 1, size = 4, endian = "big")
    y <- readBin(f, "integer", n = n, size = 1, signed = F)
    close(f)
    return(y)
  }
  tr <- file.path(path, "train-images-idx3-ubyte.gz")
  ts <- file.path(path, "t10k-images-idx3-ubyte.gz")
  tr_l <- file.path(path, "train-labels-idx1-ubyte.gz")
  ts_l <- file.path(path, "t10k-labels-idx1-ubyte.gz")
  mnist <- list()
  message("Loading Training Data...")
  mnist$train$x <- load_image_file(tr)
  message("Loading Testing Data...")
  mnist$test$x <- load_image_file(ts)
  message("Loading Training Labels...")
  mnist$train$y <- load_label_file(tr_l)
  message("Loading Testing Labels...")
  mnist$test$y <- load_label_file(ts_l)
  return(mnist)
}

show_digit <- function(arr784, col = gray(12:1 / 12), ...) {
  image(matrix(arr784, nrow = 28)[, 28:1], col = col, ...)
}

#function get p(x|c) in 32bin mode for each class
prob <- function(data)
{
  df <- data
  df_col <- ncol(df) - 1
  df_row <- nrow(df)
  new_m <- matrix(0, nrow = df_row, ncol = df_col)
  new_p <- matrix(0, nrow = 32, ncol = df_col)
  
  #convert array of RGB data (0-255) to 32bin scale
  for (i in 1:df_col)
  {
    convert <- cut(df[, i], seq(0,256,8), right = FALSE, labels = c(1:32))
    new_m[, i] <- convert
  }
  
  #get the p(x|c) from each class
  for (i in 1:ncol(new_m))
  {
    new_p[, i] <- table(factor(new_m[,i], levels=1:32))/nrow(new_m)
  }
  return(new_p)
}

prob_test <- function(data)
{
  df <- data
  df_col <- ncol(df) - 1
  df_row <- nrow(df)
  new_m <- matrix(0, nrow = df_row, ncol = df_col)
  new_p <- matrix(0, nrow = 32, ncol = df_col)
  
  #convert array of RGB data (0-255) to 32bin scale
  for (i in 1:df_col)
  {
    convert <- cut(df[, i], seq(0,256,8), right = FALSE, labels = c(1:32))
    new_m[, i] <- convert
  }
  
  return(new_m)
}

#Loading all dataset
mnist.data <- load_mnist()

#Seperate Training Data & Testing Data

#Training Data
tr <- as.data.frame(mnist.data$train$x)
tr <- subset(tr, select = -n)
tr_label <- as.data.frame(mnist.data$train$y)
tr <- cbind(tr,tr_label)
colnames(tr)[which(colnames(tr) == 'mnist.data$train$y')] <- 'label'

#Testing Data
te <- as.data.frame(mnist.data$test$x)
te <- subset(te, select = -n)
te_label <- as.data.frame(mnist.data$test$y)
te <- cbind(te,te_label)
colnames(te)[which(colnames(te) == 'mnist.data$test$y')] <- 'label'

#divide training data based on class
p_x_c0 <- as.data.frame(tr[grep(0, tr$label),])
p_x_c1 <- as.data.frame(tr[grep(1, tr$label),])
p_x_c2 <- as.data.frame(tr[grep(2, tr$label),])
p_x_c3 <- as.data.frame(tr[grep(3, tr$label),])
p_x_c4 <- as.data.frame(tr[grep(4, tr$label),])
p_x_c5 <- as.data.frame(tr[grep(5, tr$label),])
p_x_c6 <- as.data.frame(tr[grep(6, tr$label),])
p_x_c7 <- as.data.frame(tr[grep(7, tr$label),])
p_x_c8 <- as.data.frame(tr[grep(8, tr$label),])
p_x_c9 <- as.data.frame(tr[grep(9, tr$label),])

readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer, 0 for discrete, 1 for continuous: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  } else{
      if(n==0){
        
        #DISCRETE MODE
        
        print("calculating in discrete mode")
        
        prior <- matrix(0, nrow=10, ncol = 2)
        
        class <- 0
        for(i in 1:10)
        {
          for (j in 1:2)
          {
            prior[i,j] <- class
          }
          prior[i,j] <- nrow(tr[grep(class, tr$label),]) / nrow(tr)
          class <- class + 1
        }
        
        message("calculating probability of properties on each class")
        
        #discrete mode
        prob_x_c0 <- prob(p_x_c0)
        prob_x_c1 <- prob(p_x_c1)
        prob_x_c2 <- prob(p_x_c2)
        prob_x_c3 <- prob(p_x_c3)
        prob_x_c4 <- prob(p_x_c4)
        prob_x_c5 <- prob(p_x_c5)
        prob_x_c6 <- prob(p_x_c6)
        prob_x_c7 <- prob(p_x_c7)
        prob_x_c8 <- prob(p_x_c8)
        prob_x_c9 <- prob(p_x_c9)
        
        test_result <- rep(0, 10000)
        for (a in 1:10000)
        {
          class <- 0
          tes <- prob_test(te[a, ])
          # print(tes)
          
          m <- matrix(0, ncol = 784, nrow = 1)
          r <- matrix(0, ncol = 2, nrow = 10)
          
          for(i in 1:10)
          {
            tmp <- paste("prob_x_c", class, sep = "")
            coba <- eval(parse(text = tmp))
            
            for (j in 1:784)
            {
              get <- tes[j]
              value <- coba[get,j] + 0.1
              m[,j] <- value
            }
            r[i,1] <- class
            r[i,2] <- sum(log(m)) + log(prior[i,2])
            
            class <- class + 1
          }
          r <- r[order(-r[,2]),]
          print(r)
          test_result[a] <- r[1,1]
        }
        
        predfiles <- data.frame(predicted = test_result, actual = te$label[1:10000])
        
        correctpred <- predfiles$predicted[predfiles$predicted == predfiles$actual]
        
        accuracy <- (length(correctpred)/nrow(predfiles))*100
        cat("accuracy", accuracy, "%", "\n")
        
        err <- ((10000 - length(correctpred)) / 10000)*100
        cat("error rate:", err, "%")
        
      }else if (n==1) {
        
        #CONTINUOUS MODE
        
        print("calculating in continuous mode")
        
        #Continuous Mode
        Mean_x_0 <- colMeans(p_x_c0[,1:784])
        Mean_x_1 <- colMeans(p_x_c1[,1:784])
        Mean_x_2 <- colMeans(p_x_c2[,1:784])
        Mean_x_3 <- colMeans(p_x_c3[,1:784])
        Mean_x_4 <- colMeans(p_x_c4[,1:784])
        Mean_x_5 <- colMeans(p_x_c5[,1:784])
        Mean_x_6 <- colMeans(p_x_c6[,1:784])
        Mean_x_7 <- colMeans(p_x_c7[,1:784])
        Mean_x_8 <- colMeans(p_x_c8[,1:784])
        Mean_x_9 <- colMeans(p_x_c9[,1:784])
        
        colVar <- function(train, c)
        {
          new_data <- train
          class <- c
          col_data <- ncol(new_data)
          tmp <- paste("Mean_x_", c, sep = "")
          mean <- eval(parse(text = tmp))
          varData <- rep(0, 784)
          
          for (i in 1:col_data){
            varData[i] <- sum((new_data[,i] -  mean[i])^2) / nrow(new_data)
            if(varData[i]==0) varData[i] <- 10^-3
          }
          
          return(varData)
        }
        
        Var_x_0 <- colVar(p_x_c0[,1:784] , 0)
        Var_x_1 <- colVar(p_x_c1[,1:784] , 1)
        Var_x_2 <- colVar(p_x_c2[,1:784] , 2)
        Var_x_3 <- colVar(p_x_c3[,1:784] , 3)
        Var_x_4 <- colVar(p_x_c4[,1:784] , 4)
        Var_x_5 <- colVar(p_x_c5[,1:784] , 5)
        Var_x_6 <- colVar(p_x_c6[,1:784] , 6)
        Var_x_7 <- colVar(p_x_c7[,1:784] , 7)
        Var_x_8 <- colVar(p_x_c8[,1:784] , 8)
        Var_x_9 <- colVar(p_x_c9[,1:784] , 9)
        
        test_result2 <- rep(0, 10000)
        for (a in 1:10000)
        {
          class <- 0
          tes2 <- te[a,1:784]
          
          m2 <- matrix(0, ncol = 784, nrow = 1)
          r2 <- matrix(0, ncol = 2, nrow = 10)
          t2 <- matrix(0, ncol = 784, nrow = 1)
          
          for(i in 1:10)
          {
            tmp <- paste("prob_x_c", class, sep = "")
            tmp_m <- paste("Mean_x_", class, sep = "")
            tmp_v <- paste("Var_x_", class, sep = "")
            te_m <- eval(parse(text = tmp_m))
            te_v <- eval(parse(text = tmp_v))
            
            
            for (j in 1:784)
            {
              m2[,j] <- log (1/(sqrt(2*3.14)*te_v[j])) - (0.5*(tes2[,j] - te_m[j])^2) / te_v[j]
            }
            
            r2[i,1] <- class
            r2[i,2] <- sum(m2)
            
            class <- class + 1
          }
          r2 <- r2[order(-r2[,2]),]
          print(r2)
          test_result2[a] <- r2[1,1]
        }
        
        predfiles2 <- data.frame(predicted = test_result2, actual = te$label[1:10000])
        
        correctpred2 <- predfiles2$predicted[predfiles2$predicted == predfiles2$actual]
        
        accuracy2 <- (length(correctpred2)/nrow(predfiles2))*100
        cat("accuracy: ", accuracy2, "%", "\n")
        
        err <- ((10000 - length(correctpred2)) / 10000)*100
        cat("error rate:", err, "%")
        
      }else{
      return(readinteger())
    }
  }
  
  return(as.integer(n))
}

inp <- readinteger()