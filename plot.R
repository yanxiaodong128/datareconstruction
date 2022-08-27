


library("reshape2")
library("ggplot2")

test_data <-data.frame(
    contextual =c(0.199578,0.1455537,0.112773,0.0930478,0.0810),
    normal= c(0.2742991,0.1922868,0.1433604,0.104484,0.0821),
    sample_size =c(20,40,80,160,240)
  )
  


test_data_long <- melt(test_data, id="sample_size")  # convert to long format

ggplot(data=test_data_long,
       aes(x=sample_size, y=value, colour=variable)) + ylab('RMS')+xlab('n')+scale_color_discrete(name = "Likelihood")+
       geom_line()
       
       
test_data <-data.frame(
    contextual =c(0.4253757,0.3136908,0.250,0.20,0.195),
    normal= c(0.5802048,0.4130087,0.310,0.24,0.20),
    sample_size =c(20,40,80,160,240)
  )
  

test_data_long <- melt(test_data, id="sample_size")  # convert to long format

ggplot(data=test_data_long,
       aes(x=sample_size, y=value, colour=variable)) + ylab('RMS')+xlab('sample size')+scale_color_discrete(name = "Likelihood")+
       geom_line()
              
       
       
       
       
       
       
       
       
       