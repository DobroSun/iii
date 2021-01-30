.PHONY: run-debug run-release r release debug %.o clean

CXX := g++
DEPS := main.o
EXE_NAME := main
DEFAULT_CXX_FLAGS := #-Wall -Wextra -L. -I.

run-debug: clean debug
	./$(EXE_NAME)

run-release: clean release
	./$(EXE_NAME)

r: run-release

%.o: %.cpp
	$(CXX) -c -o $@ $^ $(CXXFLAGS)

release: CXXFLAGS += $(DEFAULT_CXX_FLAGS) -DNDEBUG -O3
release: clean $(DEPS)
	$(CXX) -o $(EXE_NAME) $(DEPS) $(CXXFLAGS) 
	strip $(EXE_NAME)

debug: CXXFLAGS += $(DEFAULT_CXX_FLAGS) -g -O0
debug: clean $(DEPS)
	$(CXX) -o $(EXE_NAME) $(DEPS) $(DEBUG_CFLAGS)

clean:
	rm -f $(DEPS) $(EXE_NAME)
