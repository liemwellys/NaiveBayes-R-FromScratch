# Naive Bayes Classfication Algorithm From Scratch using R Programming Language
Naive Bayes code from scratch using R programming language. The program has 2 calculation mode (discrete or continuous)

## Dataset
The dataset used in this project can be downloaded from [MNIST Dataset Website](http://yann.lecun.com/exdb/mnist/)

## Running The Program
Before running the program, makse sure the dataset loading directory has been directed on the right path. If you have different file path while loading the dataset, change the following [code](https://github.com/liemwellys/NaiveBayes-R-FromScratch/blob/master/Naive%20Bayes.R) in line 12-15 into desired filename path.

```R
mnist_url <- list(train_images = paste("D:/","train-images-idx3-ubyte.gz", sep = ""),
                    train_labels = paste("D:/","train-labels-idx1-ubyte.gz", sep = ""),
                    test_images = paste("D:/","t10k-images-idx3-ubyte.gz", sep = ""),
                    test_labels = paste("D:/","t10k-labels-idx1-ubyte.gz", sep = ""))
```
