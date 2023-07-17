# This is Laminaat.R
# This function produces a CSV-file and plot of how laminate should be placed in
# a room. 
# TODO for next updates, what to do if room is not rectangular but parallelogram- or trapezoid-like shaped? 
# What if there are corners? 

# Inputs
# CONSTANTS

# Measurements of room, including the 1cm margin on all sides
ROOM.LENGTH <- 950
ROOM.WIDTH <- 550
# surface <- room.length * room.width
# Beginning length in cms
START.LENGTH <- 50

#Sawing precision in millimeters
PRECISION <- 1

#Length and width of boards in cms
BOARD.LENGTH <- 138
BOARD.WIDTH <- 19.3

#get the number of rows
NUMBEROFROWS <- round(ROOM.WIDTH / BOARD.WIDTH) + 1

# initialize the variables
i <- numeric(0)
NumberOfBoards <- numeric(0)
end.length <- numeric(0)
position <- numeric(0)
TotalNumberOfBoards <- 0
PositionMatrix <- 0 

#Begin calculation
begin.length <- START.LENGTH

GetPositions <- function(TotalNumberOfBoards, begin.length) {
	positions <- numeric(0)
	j <- 0
	for (j in 0:TotalNumberOfBoards) {	
		answer <- begin.length + board.length * j
		if (answer > room.length) {
			answer <- NA
		}
		positions[j+1] <- answer
	}
	
	GetPositions <- positions
}

for (i in 1:(NUMBEROFROWS + 1)) {
	#i <-  i + 1
	#get the number of boards for this row
	NumberOfBoards[i] <- floor((room.length - begin.length)/ board.length)
	
	#get an upper bound of the number of boards per row
	if (TotalNumberOfBoards == 0) {		
		TotalNumberOfBoards <- NumberOfBoards[i]
	} 
	
	NumberOfBoards
	#correct for sawing precision
	left.length <- (room.length - begin.length)/ (board.length) - NumberOfBoards[i]
	left.length
	left.length <- left.length - (NumberOfBoards[i] + 1) * precision/ 1000
	left.length

	#calculate the length of the last board in this row
	end.length[i] <- round(left.length * board.length, digits = 3)
	end.length[1]
	position[2] <- begin.length + (NumberOfBoards[2]) * board.length
	room.length - end.length[1]
	
	#get the beginning positions of each board in this row and put it in the matrix
	if (length(PositionMatrix[[1]]) == 1) {
		PositionMatrix <- data.frame(GetPositions(TotalNumberOfBoards, begin.length))
	} else {
		PositionMatrix <- data.frame(PositionMatrix, GetPositions(TotalNumberOfBoards, begin.length))
	}

	#calculate the new begin.length of the next row
	begin.length <- board.length - end.length[i]
	begin.length

}
names(PositionMatrix) <- c(paste("Row", 1:NUMBEROFROWS, sep=""))
write.table(file="PositionMatrix.csv", PositionMatrix, sep=";")

#Start the plotting
plot(0, board.width, type="h", col="black", lty=1, 
	xlim=c(0, ROOM.LENGTH), 
	ylim=c(0, ROOM.WIDTH), 
	ylab="Room width", 
	xlab="Room length"
)

row <- 1
board <- 2
# Mark the four walls of the room
lines(c(0,ROOM.LENGTH), c(0, 0))
lines(c(0,0), c(0, ROOM.WIDTH))
lines(c(0, ROOM.LENGTH), c(ROOM.WIDTH, ROOM.WIDTH))
lines(c(ROOM.LENGTH, ROOM.LENGTH), c(0, ROOM.WIDTH))

for (row in 1:(NUMBEROFROWS)) {
	lines(c(0, ROOM.LENGTH), c(row * BOARD.WIDTH, row * BOARD.WIDTH))
	for (board in 1:(TotalNumberOfBoards + 1)) {
		lines(
			c(PositionMatrix[[row]][board], PositionMatrix[[row]][board]),
			c((row - 1)* BOARD.WIDTH, row * BOARD.WIDTH)	
		)
	}
}



#print(paste(end.length, NumberOfBoards), sep="\n")
