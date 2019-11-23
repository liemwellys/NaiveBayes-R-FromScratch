# Naive Bayes Classfication Algorithm From Scratch using R Programming Language
Naive Bayes code from scratch using R programming language. The program has 2 calculation mode (discrete or continuous)

## Dataset
The dataset used in this project can be downloaded from [MNIST Dataset Website](http://yann.lecun.com/exdb/mnist/)

## Running The Program
Before running the program, makse sure the dataset can be loaded properly from the right filename path. Otherwise, change the following [code](https://github.com/liemwellys/NaiveBayes-R-FromScratch/blob/master/Naive%20Bayes.R#L12) in line 12-15 into desired filename path.

```R
mnist_url <- list(train_images = paste("D:/","train-images-idx3-ubyte.gz", sep = ""),
                    train_labels = paste("D:/","train-labels-idx1-ubyte.gz", sep = ""),
                    test_images = paste("D:/","t10k-images-idx3-ubyte.gz", sep = ""),
                    test_labels = paste("D:/","t10k-labels-idx1-ubyte.gz", sep = ""))
```

The program has 2 modes of Naive Bayes Calculation. Type "0" for discrete mode and "1" for continuous mode. For more details about discrete and continuous Naive Bayes, visit this [link](https://en.wikipedia.org/wiki/Naive_Bayes_classifier)
