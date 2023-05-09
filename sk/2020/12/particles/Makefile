CC=/usr/local/cuda-10.2/bin/nvcc 

CFLAGS=-ccbin g++ -I../../common/inc  -m64    \
	      -gencode arch=compute_30,code=sm_30 \
	      -gencode arch=compute_32,code=sm_32 \
	      -gencode arch=compute_53,code=sm_53 -gencode arch=compute_61,code=sm_61 \
	      -gencode arch=compute_62,code=sm_62 -gencode arch=compute_70,code=sm_70 \
	      -gencode arch=compute_72,code=sm_72 -gencode arch=compute_75,code=sm_75 \
	      -gencode arch=compute_75,code=compute_75 

LIBS=-L/usr/lib/nvidia-container-csv-cuda -lGL -lGLU -lglut

OBJ = particleSystem.o particleSystem_cuda.o particles.o 
     
%.o: %.cpp
	$(CC) $(CFLAGS) -c -o $@ $< 

%.o: %.cu
	$(CC) $(CFLAGS) -c -o $@ $< 

particles.exe: $(OBJ)
	$(CC) -o $@ $^ $(LIBS) 

clean:
	rm -f *.o 
	rm -f *.exe
	
