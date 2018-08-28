library(dplyr)
library(ggplot2)
library(XML)
library(RCurl)
library(rvest)
library(stringr)


page = as.character(1:74)


Laptop_Name = data.frame()
Elements = data.frame()
Price = data.frame()

for(pg in page) {
        
        url = html(paste("https://www.skroutz.gr/c/25/laptop.html?page=" , pg , sep = ''))
        
        
        # 1st variable 
        Lapname = url %>% html_nodes('.details .js-sku-link') %>% html_text
        
        
        # 2nd variable
        Elem = url %>% html_nodes('.specs') %>% html_text()
        
        
        # 3rd variable
        Pr = url %>% html_nodes('.sku-link') %>% html_text()
        
        
        Laptop_Name = rbind(Laptop_Name , as.data.frame(Lapname))
        Elements = rbind(Elements , as.data.frame(Elem))
        Price = rbind(Price , as.data.frame(Pr))
        
}

rm(url , page)

rm(Lapname , Elem , Pr , pg)


laptops = data.frame(Laptop_Name = Laptop_Name , Elements = Elements , Price = Price , stringsAsFactors = FALSE)

colnames(laptops) = c('Laptop_Name' , 'Elements' , 'Price')

laptops$Laptop_Name = as.character(laptops$Laptop_Name)
laptops$Elements = as.character(laptops$Elements)
laptops$Price = as.character(laptops$Price)


### 1st column cleaning
#-------------------------------------------------------------------------------------------------------------------

# Split the Laptop_name variable. We want as a separate variable the name of the company.
splited_laptop_name = sapply(laptops$Laptop_Name , function(x) strsplit(x , split = ' '))

# "Apple" , "Acer" ...
company_name = sapply(splited_laptop_name , function(x) x[1])

table(as.factor(company_name))


# "Macbook Pro" , "Inspiron 3567"
product_name = sapply(splited_laptop_name , function(x) paste(x[2] , x[3]))

table(as.factor(product_name))


# Merge the company_name and the product_name variables with the laptops dataset
laptops = data.frame(Company = company_name , Product = product_name , laptops)


# Fix row names
row.names(laptops) = 1:dim(laptops)[1]


# Remove the Laptop_Name column
laptops = laptops[ , -3]

head(laptops)

rm(company_name , product_name , splited_laptop_name)


#-------------------------------------------------------------------------------------------------------------------



### 2nd column cleaning
#-------------------------------------------------------------------------------------------------------------------

# Split the Elements column
splited_elements = sapply(laptops$Elements, function(x) strsplit(x , split = ','))

head(splited_elements)

# We want to test the length of each splited value of the element variable
splited_values_length = sapply(splited_elements , function(x) length(x))

table(splited_values_length)

# 17 rows have length of 7. We remove them.

idx = which(splited_values_length != 8)

laptops = laptops[-idx , ]  
splited_elements = splited_elements[-idx]


rm(splited_values_length)

# 1) "Ultrabook" , "Notebook" , ...
type_name = sapply(splited_elements , function(x) x[1])
type_name = sub(pattern = '\n        ' , '' , type_name)

table(type_name)


# 2) Screen
screen_var = sapply(splited_elements , function(x) x[2])

screen_split = strsplit(screen_var , split = '"')

#       a) "13.3" , "15.6" , ...
        inches = sapply(screen_split , function(x) x[1])
        
        # Remove the blank(" ") from the inches variable
        inches = gsub(pattern = ' ' , replacement = '' , inches)
        
        table(inches)
        
        
#       b) "IPS Panel Retina Display 2560x1600" , ...
        screen_resolution = sapply(screen_split , function(x) x[2])
        
        # Remove only the first blank(" ") from the screen_resolution variable
        screen_resolution = sub(pattern = ' ' , replacement = '' , screen_resolution)
        
        table(screen_resolution)
        
        
        rm(screen_var)
        
        
# 3) "Intel Core i5 2.3GHz" , "Intel Core i5 1.8GHz" , ...
cpu = sapply(splited_elements , function(x) x[3])     
cpu = sub(' CPU: ' , '' , cpu)

table(cpu)



# 4) " RAM: 16GB" , " RAM: 8GB" , ...
ram = sapply(splited_elements , function(x) x[4])
ram = sub(' RAM: ' , '' , ram)

table(ram)



# 5) "256GB SSD" , "128GB Flash Storage"
memory = sapply(splited_elements , function(x) x[5])
memory = sub(' ' , '' , memory)

table(memory)


# 6) "AMD Radeon Pro 455" , "Intel HD Graphics 620" , ....
gpu = sapply(splited_elements , function(x) x[6])
gpu = sub(' GPU: ' , '' , gpu)

table(gpu)


# 7) "mac os" , "no os" , ...
oper_sys = sapply(splited_elements , function(x) x[7])
oper_sys = sub(' ' , '' , oper_sys)

table(oper_sys)


# 8) "1.86kg" , "1.83kg" , ...
weight = sapply(splited_elements , function(x) x[8])
weight = sub('\n      ' , '' , weight)
weight = sub(' ' , '' , weight)

table(weight)

rm(idx , splited_elements , screen_split)


# Merge the dataset based on the new changes on the Elements column
New_elements = data.frame(TypeName=type_name,Inches=inches,ScreenResolution=screen_resolution,Cpu=cpu,Ram=ram,Memory=memory,Gpu=gpu,OpSys=oper_sys,Weight=weight)

laptops = data.frame(laptops[ , 1:2] , New_elements , Price = laptops[ , 4])

rm(New_elements , cpu , gpu , inches , memory , oper_sys , weight , type_name , screen_resolution , ram)

#-------------------------------------------------------------------------------------------------------------------



### 3rd column cleaning
#-------------------------------------------------------------------------------------------------------------------

laptops$Price = as.character(laptops$Price)

laptops$Price = sub("[.]" , "" , laptops$Price)
laptops$Price = sub("," , "." , laptops$Price)
splited_euro = strsplit(laptops$Price , split = ' ')
laptops$Price = sapply(splited_euro , function(x) x[1])


#-------------------------------------------------------------------------------------------------------------------


# write.csv(x = laptops , file = "skroutz_laptops.csv")


