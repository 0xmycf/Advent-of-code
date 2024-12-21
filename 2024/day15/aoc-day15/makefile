.PHONY: all clean run

# Default target
all: target/aoc-day15-1.0-SNAPSHOT.jar

# Find all Java source files
JAVA_SRC := $(shell find src/main/java -name '*.java')

# Target JAR depends on Java source files
target/aoc-day15-1.0-SNAPSHOT.jar: $(JAVA_SRC) pom.xml
	mvn package

# Run the application
run: all
	java -cp ./target/aoc-day15-1.0-SNAPSHOT.jar me.mycf.App

# Clean the project
clean:
	mvn clean
