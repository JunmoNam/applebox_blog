#======================================================================
# mnist classification with few shot learning : 
#       few-shot learning with Siamese Neural Networks 
#
# Reference : http://www.cs.utoronto.ca/~gkoch/files/msc-thesis.pdf
#
# Author : Junmo Nam
#======================================================================

#0. Setting environment ----
options(scipen = 1000,stringsAsFactors = F)
#devtools::install_github("andrie/deepviz") #- install package for visualizing model structrue
sapply(c('dplyr','keras','imager','abind','deepviz'),require,character.only = T)

#set python for keras -- only when python go somethong wrong!
#reticulate::use_python('/Users/junmonam/opt/anaconda3/bin/python',required = T) #manually set python
#reticulate::use_condaenv('r-tensorflow',required = T) #use r-tensorflow environment

#training paramters
batch_size = 16
learning_rate = 0.0008

#1. set training data ----

#load data
mnist = keras::dataset_mnist()
#as.cimg(mnist$train$x[1,,] ) %>% plot

#train dimension
input_dim = c(dim(mnist$train$x)[-1],1)

#sample data by Y
# class = 0 ~ 9, sample only 1
sample_byclass = lapply(0:9,function(x) sample(which(mnist$train$y==x),3)) %>% unlist
names(sample_byclass) = sapply(0:9,function(x) rep(x,3))

#create grid for compare
grid = expand.grid(x1 = sample_byclass,x2 = sample_byclass) %>% 
  mutate(class_x1 = names(sample_byclass)[match(x1,sample_byclass)],
         class_x2 = names(sample_byclass)[match(x2,sample_byclass)]) %>% 
  mutate(y = as.numeric(class_x1 == class_x2)) %>% filter(x1!=x2)

grid[!duplicated(t(apply(grid, 1, sort))),]

#make batch generator
batch_generator = function(data,batch_size){
  
  # start iterator
  i <- 1
  
  # return an iterator function
  function() {
    
    # reset iterator if already seen all data
    if ((i + batch_size - 1) > nrow(data)) i <<- 1
    
    # iterate current batch's rows
    rows <- c(i:min(i + batch_size - 1, nrow(data)))
    
    # update to next iteration
    i <<- i + batch_size
    
    #get index from grid
    x1_idx = grid$x1[rows]
    x2_idx = grid$x2[rows]
    
    #get X for input layer 1 and 2
    train_x1 = mnist$train$x[x1_idx,,] / 255 #normalize
    dim(train_x1) <- c(dim(train_x1),1)
    
    train_x2 = mnist$train$x[x2_idx,,] / 255 #normalize
    dim(train_x2) <- c(dim(train_x2),1)
    
    
    #make Y matrix
    train_y = as.matrix(grid$y[rows],col = 1)
    gc()
    
    return(list(list(train_x1,train_x2),train_y))
  }
  
}

gen = batch_generator(data = grid,batch_size = batch_size)


#2. build keras model----

#make input layer x1 and x2
x1 = layer_input(shape = input_dim)
x2 = layer_input(shape = input_dim)

#feature extraction model
feature_model = keras_model_sequential() %>% 
  #conv2d and max pooling
  layer_conv_2d(8,c(3,3),input_shape = input_dim,
                activation = 'relu',kernel_initializer = 'he_normal') %>% 
  layer_max_pooling_2d() %>% 
  #
  layer_conv_2d(16,c(5,5),activation = 'relu',kernel_initializer = 'he_normal') %>% 
  layer_max_pooling_2d() %>% 
  #flatten layer
  layer_flatten() %>% 
  #feature output layer
  layer_dense(896,activation = 'relu',kernel_initializer = 'he_normal')

#calculate l1 distance(absolute distance) of two output, x1 and x2
abs_dist_layer = function(x,y){keras::k_abs(x-y)}
l1_distance = abs_dist_layer(feature_model(x1),feature_model(x2))

#build total model which contains input layer and output layer(sigmoid activated layer for similarity)
model = keras_model(inputs = list(x1,x2),
                    outputs = layer_dense(units = 1,activation = 'sigmoid')(l1_distance)) %>% 
  compile(optimizer = optimizer_nadam(lr = learning_rate),loss = 'binary_crossentropy',
          metrics = 'accuracy')

gc()

#3. training model ----
hist = model %>% fit_generator(generator = gen,
                               steps_per_epoch = round(nrow(grid) / batch_size)+1,
                               epochs = 10)

#4. make test dataset and evaluate model ----
#sample 500 test data from testset
test_data = lapply(0:9,function(x) sample(which(mnist$test$y==x),500)) %>% unlist
names(test_data) = sapply(0:9,function(x) rep(x,500))

#create grid for compare
test_grid = expand.grid(x1 = test_data,x2 = sample_byclass) %>% 
  mutate(class_x1 = names(test_data)[match(x1,test_data)],
         class_x2 = names(sample_byclass)[match(x2,sample_byclass)]) %>% 
  mutate(y = as.numeric(class_x1 == class_x2))
test_grid[!duplicated(t(apply(test_grid, 1, sort))),]

#predict similarity with support set
# select class by 1-nn based approach
test_x1 = mnist$test$x[test_grid$x1,,] / 255
dim(test_x1) = c(dim(test_x1),1)

test_x2 = mnist$train$x[test_grid$x2,,] / 255
dim(test_x2) = c(dim(test_x2),1)

#predict
pred = predict(model,list(test_x1,test_x2))

#add predict result to test grid
test_grid = test_grid %>% mutate(sim = as.vector(pred))
pred_res = test_grid %>% group_by(x1) %>% filter(sim == max(sim)) 
sum(pred_res$y==1) / nrow(pred_res)
caret::confusionMatrix(as.factor(pred_res$class_x2),
                       as.factor(pred_res$class_x1))




#5. check feature layers ----
#feature model which give every output of layers
feature_process_model = keras_model(inputs = feature_model$input,
                                    outputs = lapply(feature_model$layers,function(x)x$output))

w = model$get_weights() #get model weights from model
feature_process_model$set_weights(w[1:6])

#test data visualization
feature_vis = function(x){
  dim(x) = c(1,dim(x),1)  
  process_pred = predict(feature_process_model,x)
  image(x[1,,,])
  for (i in 1:4){
    layer_comp = process_pred[[i]]
    margin = dim(layer_comp)[4] 
    if(margin>10){
      par(mfrow = c(4,dim(layer_comp)[4] /4))
    }else{
      par(mfrow = c(2,dim(layer_comp)[4] /2))  
    }
    for(j in 1:dim(layer_comp)[4]) image(layer_comp[1,,,j],main = paste0('layer : ',i,' / dim : ',j),axes = F)  
    par(mfrow = c(1,1))
  }
}

feature_vis(test_x1[14,,,])

#6. visualize model structure ----
plot_model(model) #overall model
plot_model(feature_model) #feature extraction model
plot_model(feature_process_model) #feature process model

#7. save model and test/train features ----

#set pathname as time
save_path = paste0('/Users/junmonam/Desktop/RProject/DAT/model_res/few_shot_w_mnist_',Sys.time())
dir.create(save_path) #create

#save model
save_model_hdf5(model,paste0(save_path,'/model.h5'))

#save train list
data.frame(train_data = c(sample_byclass,rep(NA,length(test_data) - length(sample_byclass))),
           test_data = test_data) %>% 
  write.csv(paste0(save_path,'/train_n_test_idx.csv'),row.names = F)

#test data result
pred_res %>% write.csv(paste0(save_path,'/test_res.csv'),row.names = F) #test result data : max sim

test_grid %>% write.csv(paste0(save_path,'/test_grid.csv'),row.names = F) #full test data

test_grid %>% group_by(x1) %>% arrange(desc(sim)) %>% #top 3 data
  top_n(3) %>% group_by(x1) %>% summarise(y_cnt = sum(y)) %>%
  write.csv(paste0(save_path,'/test_grid_summary_top3.csv'),row.names = F)

table(test = pred_res$class_x1,support = pred_res$class_x2) %>% #prediction table
  write.csv(paste0(save_path,'/pred_actual crosstab.csv'))

pred_res %>% group_by(class_x1) %>% 
  summarise(correct = sum(y), n = n()) %>% 
  mutate(acc = correct/n) #accuracy by class

#training progress
ggplot2::ggsave(paste0(save_path,'/training_process.png'),plot(hist)+ggplot2::theme_bw())

