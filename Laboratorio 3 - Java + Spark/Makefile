JAVAC = javac
JAVA = java
SRC_DIR = src
SRC_MAIN_DIR = src/main/java
LIB_DIR = lib
LIBS = $(wildcard $(LIB_DIR)/*.jar)
CLASSPATH = $(SRC_MAIN_DIR):$(subst $(SPACE),:,$(LIBS)):$(SRC_DIR)
JAVAC_FLAGS = -d bin -cp $(CLASSPATH)
JAVA_FILES = $(shell find $(SRC_MAIN_DIR) -name "*.java") $(SRC_DIR)/App.java
CLASS_FILES = $(patsubst $(SRC_MAIN_DIR)/%.java,bin/%.class,$(patsubst $(SRC_DIR)/%.java,bin/%.class,$(JAVA_FILES)))
MAIN_CLASS = App
OUTPUT_DIR = bin

SPACE := $(empty) $(empty)

all: $(CLASS_FILES)

bin/%.class: $(SRC_MAIN_DIR)/%.java
	$(JAVAC) $(JAVAC_FLAGS) $<

bin/%.class: $(SRC_DIR)/%.java
	$(JAVAC) $(JAVAC_FLAGS) $<

run: bin/$(MAIN_CLASS).class
	@$(JAVA) -cp $(CLASSPATH):bin $(MAIN_CLASS) $(ARGS)

clean:
	rm -rf $(OUTPUT_DIR)/*.class

.PHONY: all run clean
