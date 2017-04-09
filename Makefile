

all: FastDownward alien


FastDownward:
	git clone git@github.com:guicho271828/FastDownward.git
	+FastDownward/build.py

alien:
	ros build alien.ros
