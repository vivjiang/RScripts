
#dataset: dataset containing month, year, variable, and LoanAmount columns
#month, year, LoanAmount: names of columns corresponding to Month,Year, and loan amount values
#with loan amount being what the variable is being weighted by
#variable: variable whose weighted average you want to calculate 
#output: name of the summary matrix you're adding the WA variable calculation to

Weighted_Average <-function(dataset, month, year, variable, LoanAmount,output){
	
	##generate table with the Month,Year, variable, and loan amount column from the dataset
	##where variable is the variable whose weighted average you want to calculate
	
	date <-paste(dataset[,month], dataset[,year])
	date <-mdy(date)	
	
	variable_table <-data.frame(date,dataset[,variable],dataset[,LoanAmount])
	colnames(variable_table) <-c("Date","variable_value","LoanAmount_value")
	
	variable_table <-as.data.frame(variable_table)				##convert table to a data frame
	date<-unique(date)
	
	WA_variable <-numeric(length(date))
	WA_date <-numeric(length(date))
	counter=1

	for(i in 1:length(date)){
		sub <-subset(variable_table, variable_table$Date ==date[i])
		if(nrow(sub)>0){
			weighted_average <-weighted.mean(sub$variable_value, sub$LoanAmount_value)
			WA_variable[counter]=weighted_average
			WA_date[counter]=date[i]
			counter=counter+1
		}
	}
	WA_date <-as.Date(WA_date)
	WA_table <-data.frame(WA_date, WA_variable)

	return(WA_table)														##return the vector storing all of the WA values for the variable
}


