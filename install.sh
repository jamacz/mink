#!/bin/bash

# Default installation directory
INSTALL_DIR=/usr/local/bin

JAR_TARGET_LOC=/usr/local/lib/mink
JAR_TARGET_NAME=mink-1.0.0.jar

JAR_COMPILE_LOC=./target/scala-2.13/mink-assembly-1.0.0.jar

# Set variables for local installation
if [ $# -gt 0 ]; then
    if [ $# -eq 1 ] && [ "$1" = "-l" ]; then
        # Create target directory for script
        INSTALL_DIR=~/.mink/bin
        JAR_TARGET_LOC=~/.mink/lib

        mkdir -p ~/.mink/bin

        # Add to PATH
        if ! grep -q "export PATH=\$PATH:$INSTALL_DIR" ~/.bashrc; then
            echo "export PATH=\"\$PATH:$INSTALL_DIR\"" >> ~/.bashrc
            source ~/.bashrc
        fi

        echo "Installing Mink locally"
    else
        echo "Invalid argument: use -l to install locally or no flags to install globally"
        exit 1
    fi
else
    echo "Installing Mink globally"
fi

# Create target directory for jar
echo "Creating target directory"
mkdir -p $JAR_TARGET_LOC || exit 1

# Compile
echo "Compiling Mink"
sbt assembly > /dev/null || exit 1

# Copy the compiled jar to the jar target directory
echo "Copying to target directory"
cp $JAR_COMPILE_LOC $JAR_TARGET_LOC/$JAR_TARGET_NAME

# Create shell script to run the jar file
echo "Creating shell script"
echo "#!/bin/bash" > $INSTALL_DIR/mink
echo "java -jar $JAR_TARGET_LOC/$JAR_TARGET_NAME \$@" >> $INSTALL_DIR/mink

# Make the shell script executable
chmod +x $INSTALL_DIR/mink

echo "Mink installed successfully in $INSTALL_DIR"