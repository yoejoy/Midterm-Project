##The data set chosen for the project is a survey data from pew research website. This raw data is about the Internet usage on finding health information. The survey is a called survey that raw data are basically categorical based on different options in different questions. The sample space contains 3014 rows with 122 columns representing the questions and options of the questionnaire. The purpose of this project is to find out the correlation between every question and every option of the questions, in order to improve the questionnaire for a more effective future survey. However, based on the raw data, questions are asked over the phone and most of the answer are categorical that without numerical correlations. We can only access the relation of the every data based on the probability of occurrence. Running over all the participants of survey, if the probabilities of occurrence between two questions are both are larger than 90%, we assume the two questions are double-way significant pairs. Less significantly, if there is only one probability of occurrence between two questions is larger than 90%, the pair will be consider as one-way significant, which is less important but still meaningful. For data cleaning, we follow the tidy date rules that make further analysis more convenient. Also, in order to get rid of the meaningless data, we consider the columns with no questions can be found in questionnaire script as invalid and should be eliminated. 




#Import raw data from excel file
data<-read.csv('health2012.csv')

#The raw data contains 3014 rows and 122 columns, which means there are 3014
#individuals participant in the survey and there are 122 questions and options in the questionnaire

ncol(data)
nrow(data)

#Based on the actual questionnaire script, we could eliminate some data that is not categorical and
#some of the questions are not found in the script--with no meaning.
data<-data[,-which(colnames(data)=='standwt')]
data<-data[,-which(colnames(data)=='weight')]
data<-data[,-which(colnames(data)=='FIPS')]
data<-data[,-which(colnames(data)=='state')]
data<-data[,-which(colnames(data)=='age')]
data<-data[,-which(colnames(data)=='q8os')]
data<-data[,-which(colnames(data)=='q23mos')]
data<-data[,-which(colnames(data)=='q26mos')]
data<-data[,-which(colnames(data)=='q29mos')]

#molten data and tidy data: move every column option into every individual interviewee.

insmaple_row<-sample(nrow(data),nrow(data)*0.8)
outsample_row<-sequence(nrow(data))[-insmaple_row]
data_insample<-data[insmaple_row,]
data_outsample<-data[outsample_row,]

#Should only consider options with occurance probability larger than 50%, so limit the sample that has to run the loop.
questions<-colnames(data_insample)[2:length(colnames(data_insample))]
level_string<-c()
level_length<-c()
t1<-0.5
for (i in 1:length(questions)){
  sublevel<-levels(factor(data_insample[,i+1]))
  level_length<-c(level_length,length(sublevel))
  for(j in 1:length(sublevel)){
    p<-length(which(data_insample[,i+1]==level_length[j]))/nrow(data_insample)
    if(p>t1){
      level_string<-c(level_string,paste0(questions[i],':',levels(factor(data_insample[,i+1]))[j]))
    }
    
  }
}

#res_matrix is the large matrix of every relation probability caculated and listed based on the clean data.

res_matrix<-matrix(0,nrow=length(level_string),ncol=length(level_string))
colnames(res_matrix)<-level_string
rownames(res_matrix)<-level_string
for(i in 1:length(level_string)){
  for(j in 1:length(level_string)){
    str_A<-level_string[i]
    str_B<-level_string[j]
    str_A_q<-strsplit(str_A,':')[[1]][1]
    str_A_a<-strsplit(str_A,':')[[1]][2]
    str_B_q<-strsplit(str_B,':')[[1]][1]
    str_B_a<-strsplit(str_B,':')[[1]][2]
    A_column<-which(colnames(data_insample)==str_A_q)
    B_column<-which(colnames(data_insample)==str_B_q)
    data_insample_sub<-data_insample[data_insample[,A_column]==str_A_a,]
    data_insample_subsub<-data_insample_sub[data_insample_sub[,B_column]==str_B_a,]
    if(nrow(data_insample_sub)==0){
      p<-0
    }else{
      p<-round(nrow(data_insample_subsub)/nrow(data_insample_sub),digits = 4)
    }
    res_matrix[i,j]<-p
  }
}

#Set: If the probability that the both qustions have impact on another is larfer than 90%, 
#we consider this two qustions are related. In some cases, the two questions could be
#two options of the same question. As a result, the question/option might be eliminated in the future questionnaire.
#There are two types of relations are calculated: 
#double-way significant pairs: both have more than 90% probability of occurance of each other.
#one-way significant pairs: only one have more than 90% probability of occurance of another.

threhold<-0.9
dw_significant_pairs<-c()
ow_significant_pairs<-c()
for(i in 1:(nrow(res_matrix)-1)){
  for(j in (i+1):nrow(res_matrix)){
    if(res_matrix[i,j]>threhold&&res_matrix[j,i]>threhold){
      dw_significant_pairs<-rbind(dw_significant_pairs,c(colnames(res_matrix)[i],colnames(res_matrix)[j],res_matrix[i,j],res_matrix[j,i]))
      colnames(dw_significant_pairs) <- c("q1", "q2", "q1vq2", "q2vq1")
    }
  }
}
for(i in 1:(nrow(res_matrix))){
  for(j in 1:nrow(res_matrix)){
    if(res_matrix[i,j]>threhold){
      ow_significant_pairs<-rbind(ow_significant_pairs,c(colnames(res_matrix)[i],colnames(res_matrix)[j],res_matrix[i,j]))
      colnames(ow_significant_pairs) <- c("q1", "q2", "q1vq2_ow")
      
        
    }
  }
}

dw_significant_pairs_df <- data.frame(dw_significant_pairs)
ow_significant_pairs_df <- data.frame(ow_significant_pairs)

#plot and visualize significant pairs

dw_significant_pairs_df$q1vq2_intervals<- cut(as.numeric(dw_significant_pairs_df$q1vq2) , 5, label=c("(0.9,0.92)","(0.92,0.94)","(0.94,0.96)","(0.96,0.98)","(0.98,1)"))
dw_significant_pairs_df$q2vq1_intervals<- cut(as.numeric(dw_significant_pairs_df$q2vq1) , 5, label=c("(0.9,0.92)","(0.92,0.94)","(0.94,0.96)","(0.96,0.98)","(0.98,1)"))

library(ggplot2)

ggplot(data = dw_significant_pairs_df, aes(x=q1, y=q2, fill= q1vq2_intervals)) + geom_tile()

ggplot(dw_significant_pairs_df, aes(q1, q2)) + geom_tile(aes(fill=q1vq2_intervals),colour="black") + scale_fill_brewer(palette = "RdYlGn",name="Correlation") 

ggsave(file="correlation_q1vq2.png")

#Using the same method, the relation from q2 to q1 can be plotted.
  
#Forward steps:
  #For a better focus on the relationship between questions and for a better efficiency of this data organization, some columns of the raw data have been eliminated.  For example, the “age” tab could be separate into different age categories instead of using all numbers from 18 to 100 or just eliminating the age data. We can do a separate research on the relationship between ages and the answers of questions.

