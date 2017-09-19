library(R.matlab)
library(MASS)
library(matlab)
library(matlib)
  
inp <- readMat("D:/Users/earmmor/OneDrive - University of Leeds/Glider - Private/WP2/IO Model/UKmodel2016_4region/Z_2013.mat")
z <- inp$Z.2013  
inp <- readMat("D:/Users/earmmor/OneDrive - University of Leeds/Glider - Private/WP2/IO Model/UKmodel2016_4region/Y_2013.mat")
y  <- inp$Y.2013
inp <- readMat("D:/Users/earmmor/OneDrive - University of Leeds/Glider - Private/WP2/IO Model/UKmodel2016_4region/v_2013.mat")
v <- inp$V.2013
inp <- readMat("D:/Users/earmmor/OneDrive - University of Leeds/Glider - Private/WP2/IO Model/UKmodel2016_4region/GHG_2013.mat")
ghg <- inp$GHG.2013


#x = sum(Z_2013,2)+sum(Y_2013,2); %sum each row in Z and each row in Y then add togther
x = rowSums(z)+sum(y)
#x(x<=0) = 0.00000001; %Remove all the 0
x[x<=0] <-  0.00000001
#A = Z_2013./(repmat(x',848,1)); %Calculate IO coefficents by dividing Z by x' (x' is transpose of x)
rep <-  matrix(t(x),nrow=848,ncol=length(v),byrow=TRUE)
A = z / rep
#L = inv(eye(848)-A); % find the Leontive Inverse L = (I-A)^-1
I <- diag(nrow(z))
L = solve(I-A)

#e = GHG_2013./x'; % find emisssions intesivity by dividing GHG by x'
e = as.numeric(ghg / t(x))

#Y_UK = sum(Y_2013(:,1:7),2); %Sum only the first 7 columns of Y to get results for UK only
Y_UK = rowSums(y[,1:7])

#Q_UK = diag(e)*L*diag(Y_UK); %Find UK footprint Q = e * L * Y (diagonalised e and Y becuase otherwise you get a single number answer)
Q_UK = diag(e) %*% L %*% diag(Y_UK)

B = I - A
B.inv <- solve(B)
C = B %*% B.inv

